################################################################################
# Author: jahred liddie
# purpose: Process and merge necessary data from UCMR 5
  # UCMR 5 version: 08/2026 (final release version)
  # see: https://www.epa.gov/dwucmr/fifth-unregulated-contaminant-monitoring-rule
################################################################################

library(sf)
library(here)
library(tidyverse)

ucmr5_file <- here("data", "ucmr5-occurrence-data_082026", "UCMR5_All.txt")
ucmr5_elements_file <- here("data", "ucmr5-occurrence-data_082026", "UCMR5_AddtlDataElem.txt")

# version 2.0 used here
CWS_boundaries <- st_read(here("data", "EPA_CWS_V2p0", "CWS_Boundaries_Latest", "CWS_2_0.gpkg"))

# these are results from final July 2026 update (see files)
ucmr <- read.delim(ucmr5_file, 
                   sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                   fileEncoding="latin1", quote = "")

ucmr_other_field <- read.delim(ucmr5_elements_file, 
                               sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                               fileEncoding="latin1", quote = "")

  # some checks
  unique(ucmr$Units) # should only be ug/L

  table(ucmr$MethodID, useNA = "always")
  table(ucmr$Contaminant, useNA = "always")
  
  # these should match EPA's online summary: 
  # https://www.epa.gov/system/files/documents/2023-08/ucmr5-data-summary_0.pdf
  table(ucmr$Contaminant)
  
  num_systems_per_analyte <- ucmr %>%
    group_by(Contaminant) %>%
    arrange(-desc(Contaminant)) %>%
    dplyr::summarise("UCMR 5 MRL (µg/L)" = unique(MRL),
                      "Total number of results" = n(),
                      "Number of results ≥MRL" = sum(AnalyticalResultsSign == "="),
                      "Total number of PWS with results" = n_distinct(PWSID),
                      "Number of PWS with results ≥MRL" = n_distinct(PWSID[AnalyticalResultsSign == "="])) %>%
    ungroup()
  
  num_systems_per_analyte <- num_systems_per_analyte %>%
    mutate(Contaminant = fct_relevel(Contaminant,
                                     "PFOS", "PFOA", "HFPO-DA", "PFHxS", "PFNA", "PFPeA", "PFBS",
                                     "lithium", "PFBA", "PFHxA", "PFDA", "11Cl-PF3OUdS", "8:2 FTS",
                                     "4:2 FTS", "6:2 FTS", "ADONA", "9Cl-PF3ONS", "NFDHA", "PFEESA",
                                     "PFMPA", "PFMBA", "PFDoA", "PFHpS", "PFHpA", "PFPeS", "PFUnA", 
                                     "NEtFOSAA", "NMeFOSAA", "PFTA", "PFTrDA"))
  
  # print(num_systems_per_analyte %>% arrange(Contaminant))

ucmr5_contaminants <- c("PFOS", "PFOA", "HFPO-DA", "PFHxS", "PFNA", "PFPeA", "PFBS",
                        "lithium", "PFBA", "PFHxA", "PFDA", "11Cl-PF3OUdS", "8:2 FTS",
                        "4:2 FTS", "6:2 FTS", "ADONA", "9Cl-PF3ONS", "NFDHA", "PFEESA",
                        "PFMPA", "PFMBA", "PFDoA", "PFHpS", "PFHpA", "PFPeS", "PFUnA", 
                        "NEtFOSAA", "NMeFOSAA", "PFTA", "PFTrDA")

# pivot wide
ucmr_wide <- ucmr %>% 
  group_by(PWSID, PWSName, CollectionDate, State, Region,
           Contaminant, FacilityName, Size, FacilityWaterType, 
           FacilityID, SamplePointID, SampleEventCode) %>%
  mutate(flag = row_number()) %>%
  pivot_wider(id_cols = c(PWSID, PWSName, State, Region, CollectionDate, 
                          FacilityName, Size, flag, FacilityWaterType,
                          FacilityID, SamplePointID, SampleEventCode), 
              names_from = Contaminant,
              values_from = c(AnalyticalResultValue, MRL)) %>% 
  ungroup()

# conversions to ng/L from ug/L
ucmr_wide <- ucmr_wide %>%
  mutate(across(starts_with("MRL"), ~.x*1000),
         across(starts_with("AnalyticalResultValue"), ~.x*1000)) 

# rename results columns
ucmr_wide <- ucmr_wide %>%
  rename_at(vars(starts_with("AnalyticalResultValue")), 
            ~str_replace(string = ., pattern = "AnalyticalResultValue_", 
                         replacement = ""))

# reorder columns to common order:
ucmr_wide <- ucmr_wide %>%
  relocate(PWSID:SampleEventCode, ucmr5_contaminants, 
           paste("MRL_", ucmr5_contaminants, sep = ""))

# function to identify detects; 
  # if you set the operator to '<`, it should reproduce EPA's results here: 
  # https://www.epa.gov/dwucmr/data-summary-fifth-unregulated-contaminant-monitoring-rule
  detect_f <- function(MRL_var, PFAS_var){
    
    ifelse(!is.na(MRL_var) & PFAS_var <= MRL_var |
             is.na(PFAS_var) & !is.na(MRL_var), 0, 
           ifelse(is.na(MRL_var) & is.na(PFAS_var), NA, 1)) # this captures NAs
    
  }

# create detect vars
ucmr_wide <- ucmr_wide %>%
  mutate(across(.cols = PFOS:PFTrDA,
                .fns = ~detect_f(MRL_var = get(glue::glue("MRL_{cur_column()}")), PFAS_var = .x), 
                .names = "{.col}_detect")
         )

analyte_list <- names(ucmr_wide %>% dplyr::select(PFOS:PFTrDA))
                 
# check:
min_detect <- function(PFAS) {
  
  suppressWarnings(
  min.PFAS <- 
    min(ucmr_wide %>% dplyr::select(PFAS, paste(PFAS, "_detect", sep = "")) %>% 
        filter(!!as.symbol(paste(PFAS, "_detect", sep = "")) == 1) %>% 
        dplyr::select(PFAS), na.rm = "TRUE")
  )
  
  min.PFAS <- 
    ifelse(suppressWarnings(is.infinite(min.PFAS)), NA,
         min.PFAS)
  
  min.detects <- tibble(PFAS, min.PFAS)
  
  return(min.detects)
  
}

# should all be above the respective MRL
print(map_dfr(analyte_list, ~min_detect(PFAS = .x)), n = 30)

###############################################################################
# create function to return NA if all values are NA
sumna <- function(x) {
  if( all(is.na(x)) ) NA 
  else sum(x, na.rm = TRUE)
}

# (de facto) non-detect substitution w/ 0. This distinguishes measured but ND from unmeasured.
ucmr_wide <- ucmr_wide %>%
  mutate(across(.cols = PFOS:PFTrDA,
                .fns = ~ifelse(is.na(.x) & !is.na(get(glue::glue("MRL_{cur_column()}"))) | 
                                 .x <= get(glue::glue("MRL_{cur_column()}")) & 
                                 !is.na(get(glue::glue("MRL_{cur_column()}"))), 0, .x), 
                .names = "{.col}")
            )

n_distinct(ucmr_wide$PWSID)

# keeping only groundwater systems (FacilityWaterType = GW) and generate new
  # PWSID/facility/sample point/sample event identiifer
ucmr_wide <- ucmr_wide %>%
  filter(FacilityWaterType == "GW") %>%
  mutate(treatment_identifer = paste(PWSID, FacilityID, SamplePointID, SampleEventCode,
                                     sep = "_"))

n_distinct(ucmr_wide$PWSID)

ucmr_wide %>%
  group_by(PWSID) %>%
  mutate(n_facilities = n_distinct(FacilityID)) %>%
  ungroup() %>%
  dplyr::summarise(median_facilities = median(n_facilities),
                   mean_facilities = mean(n_facilities),
                   pct25_facilities = quantile(n_facilities, 0.25),
                   pct75_facilities = quantile(n_facilities, 0.75))
  
# this is based on the most relevant identified by the US EPA here: 
  # https://tdb.epa.gov/tdb/contaminant?id=11020
  # and based on the Economic Analysis for the PFAS MCLs
relevant_treatment_tech <- c("GAC", "IEX", "NRO")

# see here for descriptions: 
  # https://www.epa.gov/dwucmr/data-summary-fifth-unregulated-contaminant-monitoring-rule
pfas_treatment_field <- ucmr_other_field %>% 
  filter(AdditionalDataElement == "PFASTreatment") %>%
  filter(Response %in% relevant_treatment_tech) %>%
  mutate(treatment_identifer = paste(PWSID, FacilityID, SamplePointID, SampleEventCode,
                              sep = "_"))

n_distinct(ucmr_wide$PWSID)

# dropping samples according to this identifier
ucmr_wide <- ucmr_wide %>%
  filter(!treatment_identifer %in% pfas_treatment_field$treatment_identifer)

n_distinct(ucmr_wide$PWSID)

# now cross-reference w/ CWS SAB dataset (v2.0) and get centroids
  CWS_boundaries <- st_make_valid(CWS_boundaries)
  CWS_boundaries <- CWS_boundaries %>% dplyr::select(PWSID)
  
  # for one PWSID (MO6024530), the boundaries are first union-ed and then the centroids 
    # are determined, - this PWSID is listed twice  with non-intersecting boundaries
  PWSID_dup <- CWS_boundaries %>%
    filter(PWSID %in% PWSID[duplicated(PWSID)]) %>%
    group_by(PWSID) %>%
    summarise() %>%
    ungroup()
  
  CWS_boundaries <- CWS_boundaries %>% 
    filter(!PWSID %in% PWSID_dup$PWSID) %>%
    rbind(PWSID_dup)
  
  CWS_boundaries_centroids <- st_centroid(CWS_boundaries)
  
  ucmr_wide <- ucmr_wide %>%
    filter(PWSID %in% CWS_boundaries$PWSID)
  
  n_distinct(ucmr_wide$PWSID)
  
  ucmr_wide_geom <- left_join(ucmr_wide, 
                              CWS_boundaries_centroids %>% dplyr::select(PWSID, geom))

  ucmr_wide_geom <- ucmr_wide_geom %>%
    mutate(lon = st_coordinates(geom)[,1],
           lat = st_coordinates(geom)[,2]) %>%
    dplyr::select(PWSID:SampleEventCode, lon, lat, 
                  contains("PFBA"), contains("PFPeA"),
                  contains("PFOA"), contains("PFOS"), 
                  -flag)
  
  # these are irrelevant - pertain to measurements of other analytes
  ucmr_wide_geom <- ucmr_wide_geom %>%
    filter(! (is.na(PFBA) & is.na(PFPeA) & is.na(PFOA) & is.na(PFOS)))
  
  n_distinct(ucmr_wide_geom$PWSID)
  
  ucmr_wide_geom <- ucmr_wide_geom %>%
    filter(!State %in% c("AK", "HI", "GU", "PR", "MP")) # drop non-contiguous PFAS
  
  n_distinct(ucmr_wide_geom$PWSID)

################################################################################
# number of facilities per PWSID (system)
ucmr_wide_geom <- ucmr_wide_geom %>%
    mutate(PWSID_FacilityID = paste(PWSID, FacilityID, sep = "_")) %>%
    group_by(PWSID) %>%
    mutate(n_facilities = n_distinct(FacilityID)) %>%
    ungroup()
  
num_facilities <- ucmr_wide_geom %>% 
  dplyr::select(PWSID, n_facilities) %>% 
  unique()

# most have 1-4 facilities
num_facilities %>%
  dplyr::summarise(mean_num_facilities = mean(n_facilities),
                   p25_num_facilities = quantile(n_facilities, probs = 0.25),
                   median_num_facilities = median(n_facilities),
                   p75_num_facilities = quantile(n_facilities, probs = 0.75),
                   p98_num_facilities = quantile(n_facilities, probs = 0.98),
                   max_num_facilities = max(n_facilities))

num_facilities_measurements <- ucmr_wide_geom %>%
  dplyr::select(PWSID, PWSID_FacilityID, n_facilities) %>%
  group_by(PWSID_FacilityID) %>%
  mutate(n_measurements = n()) %>%
  ungroup() %>%
  unique()

# however, most facilities only have 2 measurements, so choice of mean vs median among facility doesn't matter
  # as shown below
num_facilities_measurements %>%
  dplyr::summarise(mean_num_sample = mean(n_measurements),
                   p25_num_sample = quantile(n_measurements, probs = 0.25),
                   median_num_sample = median(n_measurements),
                   p75_num_sample = quantile(n_measurements, probs = 0.75),
                   p98_num_sample = quantile(n_measurements, probs = 0.98),
                   max_num_sample = max(n_measurements))

# collapse one level (combine measurements from same facility)
ucmr_wide_geom_collapse <- ucmr_wide_geom %>%
  group_by(PWSID, PWSID_FacilityID, State, Region, FacilityName, FacilityID, lat, lon, n_facilities) %>%
  dplyr::summarise(PFBA_detects = sum(PFBA_detect, na.rm = T),
                   PFBA_measurements = sum(!is.na(PFBA_detect)),
                   PFPeA_detects = sum(PFPeA_detect, na.rm = T),
                   PFPeA_measurements = sum(!is.na(PFPeA_detect)),
                   PFOA_detects = sum(PFOA_detect, na.rm = T),
                   PFOA_measurements = sum(!is.na(PFOA_detect)),
                   PFOS_detects = sum(PFOS_detect, na.rm = T),
                   PFOS_measurements = sum(!is.na(PFOS_detect)),
                   PFBA = ifelse(PFBA_detects == 0, 0, mean(PFBA[PFBA != 0], na.rm = T)),
                   PFPeA = ifelse(PFPeA_detects == 0, 0, mean(PFPeA[PFPeA != 0], na.rm = T)),
                   PFOA = ifelse(PFOA_detects == 0, 0, mean(PFOA[PFOA != 0], na.rm = T)),
                   PFOS = ifelse(PFOS_detects == 0, 0, mean(PFOS[PFOS != 0], na.rm = T))) %>%
  ungroup() %>%
  mutate(PFBA_detect = ifelse(PFBA_detects > 0, 1, 0),
         PFPeA_detect = ifelse(PFPeA_detects > 0, 1, 0),
         PFOA_detect = ifelse(PFOA_detects > 0, 1, 0),
         PFOS_detect = ifelse(PFOS_detects > 0, 1, 0))

# generally, all samples were detect or non-detect from a given facility, see:
nrow(ucmr_wide_geom_collapse %>% filter(PFBA_detects != PFBA_measurements & PFBA_detects != 0))/nrow(ucmr_wide_geom_collapse)
nrow(ucmr_wide_geom_collapse %>% filter(PFPeA_detects != PFPeA_measurements & PFPeA_detects != 0))/nrow(ucmr_wide_geom_collapse)
nrow(ucmr_wide_geom_collapse %>% filter(PFOA_detects != PFOA_measurements & PFOA_detects != 0))/nrow(ucmr_wide_geom_collapse)
nrow(ucmr_wide_geom_collapse %>% filter(PFOS_detects != PFOS_measurements & PFOS_detects != 0))/nrow(ucmr_wide_geom_collapse)

library(lme4)

model_dat <- ucmr_wide_geom_collapse %>% filter(n_facilities > 1)

m1 <- glmer(PFBA_detect ~ (1|PWSID), data = model_dat, family = "binomial")
m2 <- glmer(PFPeA_detect ~ (1|PWSID), data = model_dat, family = "binomial")
m3 <- glmer(PFOA_detect ~ (1|PWSID), data = model_dat, family = "binomial")
m4 <- glmer(PFOS_detect ~ (1|PWSID), data = model_dat, family = "binomial")

performance::icc(m1)
performance::icc(m2)
performance::icc(m3)
performance::icc(m4)

m1_cont <- lmer(PFBA ~ (1|PWSID), data = model_dat %>% filter(PFBA_detect == 1))
m2_cont <- lmer(PFPeA ~ (1|PWSID), data = model_dat %>% filter(PFPeA_detect == 1))
m3_cont <- lmer(PFOA ~ (1|PWSID), data = model_dat %>% filter(PFOA_detect == 1))
m4_cont <- lmer(PFOS ~ (1|PWSID), data = model_dat %>% filter(PFOS_detect == 1))

# the ICCs are generally low on the identity / continuous scale
performance::icc(m1_cont)
performance::icc(m2_cont)
performance::icc(m3_cont)
performance::icc(m4_cont)

# compare after collapsing further into one observation per PWSID
ucmr_final <- ucmr_wide_geom_collapse %>%
  group_by(PWSID, State, Region, lat, lon, n_facilities) %>%
  dplyr::summarise(PFBA_detect = ifelse(sum(PFBA_detect) > 0, 1, 0),
                   PFPeA_detect = ifelse(sum(PFPeA_detect) > 0, 1, 0),
                   PFOA_detect = ifelse(sum(PFOA_detect) > 0, 1, 0),
                   PFOS_detect = ifelse(sum(PFOS_detect) > 0, 1, 0),
                   
                   PFBA_mean = ifelse(max(PFBA) == 0, 0, mean(PFBA[PFBA != 0], na.rm = T)),
                   PFPeA_mean = ifelse(max(PFPeA) == 0, 0, mean(PFPeA[PFPeA != 0], na.rm = T)),
                   PFOA_mean = ifelse(max(PFOA) == 0, 0, mean(PFOA[PFOA != 0], na.rm = T)),
                   PFOS_mean = ifelse(max(PFOS) == 0, 0, mean(PFOS[PFOS != 0], na.rm = T)),
                   
                   PFBA_median = ifelse(max(PFBA) == 0, 0, median(PFBA[PFBA != 0], na.rm = T)),
                   PFPeA_median = ifelse(max(PFPeA) == 0, 0, median(PFPeA[PFPeA != 0], na.rm = T)),
                   PFOA_median = ifelse(max(PFOA) == 0, 0, median(PFOA[PFOA != 0], na.rm = T)),
                   PFOS_median = ifelse(max(PFOS) == 0, 0, median(PFOS[PFOS != 0], na.rm = T)),
                   
                   PFBA_max = ifelse(max(PFBA) == 0, 0, max(PFBA[PFBA != 0], na.rm = T)),
                   PFPeA_max = ifelse(max(PFPeA) == 0, 0, max(PFPeA[PFPeA != 0], na.rm = T)),
                   PFOA_max = ifelse(max(PFOA) == 0, 0, max(PFOA[PFOA != 0], na.rm = T)),
                   PFOS_max = ifelse(max(PFOS) == 0, 0, max(PFOS[PFOS != 0], na.rm = T)),
                   
                   PFBA_min = ifelse(max(PFBA) == 0, 0, min(PFBA[PFBA != 0], na.rm = T)),
                   PFPeA_min = ifelse(max(PFPeA) == 0, 0, min(PFPeA[PFPeA != 0], na.rm = T)),
                   PFOA_min = ifelse(max(PFOA) == 0, 0, min(PFOA[PFOA != 0], na.rm = T)),
                   PFOS_min = ifelse(max(PFOS) == 0, 0, min(PFOS[PFOS != 0], na.rm = T))) %>%
  ungroup()

metric_comparison <- pivot_longer(ucmr_final, cols = PFBA_mean:PFOS_min, 
                                  names_to = "metric", values_to = "value")

metric_comparison <- metric_comparison %>%
  mutate(metric_type = case_when(grepl("mean", metric) ~ "mean",
                                 grepl("median", metric) ~ "median",
                                 grepl("max", metric) ~ "max",
                                 grepl("min", metric) ~ "min"),
         analyte = case_when(grepl("PFBA", metric) ~ "PFBA",
                             grepl("PFPeA", metric) ~ "PFPeA",
                             grepl("PFOA", metric) ~ "PFOA",
                             grepl("PFOS", metric) ~ "PFOS"))

metric_comparison <- pivot_wider(metric_comparison, id_cols = c(PWSID, analyte, n_facilities),
                                 names_from = metric_type, values_from = value)

metric_comparison <- metric_comparison %>%
  mutate(n_facilities_bins = case_when(n_facilities == 1 ~ "1",
                                       n_facilities == 2 ~ "2",
                                       n_facilities > 2 & n_facilities <= 4 ~ "3-4",
                                       n_facilities > 4 & n_facilities <= 10 ~ "5-10",
                                       n_facilities > 10 ~ ">10"))

metric_comparison$n_facilities_bins <- fct_relevel(metric_comparison$n_facilities_bins,
                                                   "1", "2", "3-4", "5-10", ">10")
 
ggplot(metric_comparison %>% filter(min > 0), aes(x = mean, y = max, color = n_facilities_bins)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values = MetBrewer::met.brewer("Egypt", n = 5),
                     name = "# facilities") +
  geom_smooth(method = "lm", se = F) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Mean among facilities (with detections > MRL)",
       y = "Max among facilities (with detections > MRL)") +
  facet_grid(n_facilities_bins~analyte) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(here("figures", "max_mean_comparison_UCMR5_facilities.png"), 
       dpi = 600, width = 6, height = 6)

ggplot(metric_comparison %>% filter(min > 0), aes(x = mean, y = median, color = n_facilities_bins)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values = MetBrewer::met.brewer("Egypt", n = 5),
                     name = "# facilities") +
  geom_smooth(method = "lm", se = F) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Mean among facilities (with detections > MRL)",
       y = "Max among facilities (with detections > MRL)") +
  facet_grid(n_facilities_bins~analyte) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(here("figures", "median_mean_comparison_UCMR5_facilities.png"), 
       dpi = 600, width = 6, height = 6)

ggplot(metric_comparison %>% filter(min > 0), aes(x = mean, y = min, color = n_facilities_bins)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values = MetBrewer::met.brewer("Egypt", n = 5),
                     name = "# facilities") +
  geom_smooth(method = "lm", se = F) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Mean among facilities (with detections > MRL)",
       y = "Max among facilities (with detections > MRL)") +
  facet_grid(n_facilities_bins~analyte) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(here("figures", "min_mean_comparison_UCMR5_facilities.png"),
       dpi = 600, width = 6, height = 6)

dropped_PWSIDs <- ucmr_final$PWSID[ucmr_final$n_facilities >= 5]

# most that are dropped are larger PWS (67%)
prop.table(
  table(ucmr_wide %>% 
        filter(PWSID %in% dropped_PWSIDs) %>%
        group_by(PWSID) %>%
        slice(1) %>%
        ungroup() %>%
        pull(Size))
)

# drop systems with 5+ facilities
ucmr_final <- ucmr_final %>%
  dplyr::filter(n_facilities < 5)

n_distinct(ucmr_final$PWSID)

write.csv(ucmr_final, here("data", "output", "final_UCMR5_data_with_locations.csv"))
