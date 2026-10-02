################################################################################
# primary authors: jahred liddie, ellen wei
# purpose: combine UCMR 5 and WQP data, drop/rename columns
# date created: 10/14/2025
################################################################################

library(here)
library(tidyverse)

ucmr <- read.csv(here("data", "output", "final_UCMR5_data_with_locations.csv"))
WQP <- read.csv(here("data", "output", "PFAS_WQP.csv"))

WQP <- WQP %>% 
  dplyr::select(PWSID_or_MonitoringLocationIdentifier = MonitoringLocationIdentifier, 
                lat = MonitoringLocationLatitudeMeasure, lon = MonitoringLocationLongitudeMeasure, 
                CharacteristicName, ResultMeasureValue_ngL, DetectionLimit_ngL) %>%
  mutate(CharacteristicName = case_when(CharacteristicName == "Perfluorobutanoic acid" ~ "PFBA",
                                        CharacteristicName == "Perfluorooctanesulfonic acid" ~ "PFOS",
                                        CharacteristicName == "Perfluorooctanoic acid" ~ "PFOA",
                                        CharacteristicName == "Perfluoropentanoate" ~ "PFPeA"),
         # this is to distinguish non-detects from no measurements when pivoting wide:
         ResultMeasureValue_ngL = ifelse(is.na(ResultMeasureValue_ngL), 0, ResultMeasureValue_ngL)
         )

ucmr_detect_long <- pivot_longer(ucmr %>% dplyr::select(-contains("mean"), -contains("max"), 
                                                 -contains("median"), -contains("min")),
                          cols = c(PFBA_detect:PFOS_detect),
                          names_to = "compound", values_to = "ever_detected")

ucmr_detect_long <- ucmr_detect_long %>%
  mutate(compound = str_replace(compound, pattern = "_detect", replacement = ""),
         DL_most_recent = case_when(compound == "PFBA" ~ 5,
                                    compound == "PFPeA" ~ 3,
                                    compound %in% c("PFOA", "PFOS") ~ 4))

ucmr_mean_long <- pivot_longer(ucmr %>% dplyr::select(-contains("max"), -contains("detect"),
                                                      -contains("median"), -contains("min")),
                               cols = c(PFBA_mean:PFOS_mean),
                               names_to = "compound", values_to = "result_most_recent")
ucmr_mean_long <- ucmr_mean_long %>%
  mutate(compound = str_replace(compound, pattern = "_mean", replacement = ""))

ucmr_long <- left_join(ucmr_detect_long, ucmr_mean_long)

ucmr_long <- ucmr_long %>%
  rename(PWSID_or_MonitoringLocationIdentifier = PWSID) %>%
  dplyr::select(-X, -State, -Region, -n_facilities)

WQP <- WQP %>%
  rename(result_most_recent = ResultMeasureValue_ngL, DL_most_recent = DetectionLimit_ngL,
         compound = CharacteristicName) %>%
  mutate(result_most_recent = ifelse(!is.na(DL_most_recent) & result_most_recent <= DL_most_recent, 0, result_most_recent),
         ever_detected = ifelse(result_most_recent > 0 & is.na(DL_most_recent) | result_most_recent > DL_most_recent, 1, 0),
         )

combined_dat <- rbind(WQP, ucmr_long)


write.csv(combined_dat, here("data", "output", "UCMR_WQP_PFAS_combined.csv"))
