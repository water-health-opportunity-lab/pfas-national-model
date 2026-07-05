# ===========================================================================
# Author: Kyndra Shea
# Last updated: 05/7/2026
#
# Predictors added:
#   impact_pfas_sites, dist_pfas_sites
#   dist_fed_known, dist_fed_suspected, dist_discharge
#   dist_superfund_pfas, dist_superfund_npl, dist_spills
#   dist_part139_airports, dist_treatment_plants, dist_onsite_systems
# ===========================================================================

# Set up
library(spdep)
library(tidyverse)
library(readr)
library(dplyr)
library(sf)
library(readxl)
library(terra)

# Establish target CRS for uniform spatial standards
target_crs <- 5070  # NAD83 / CONUS Albers (meters)

# ===========================================================================
# STEP 1: Read in existing gpkg and remove deprecated individual columns
# Note: commented out after first run — gpkg already cleaned
# ===========================================================================

# Vuln <- st_read("/Users/kyndrashea/pfas-project/data/output/full_pfas_covars.gpkg")

# Vuln <- Vuln %>%
#   select(-starts_with("impact_naics_"))

# st_write(Vuln, "/Users/kyndrashea/pfas-project/data/output/full_pfas_covars.gpkg",
#          delete_dsn = TRUE)

# ===========================================================================
# STEP 2: Read in wells and build source layers
# ===========================================================================

# Reproject for spatial operations
Vuln_m <- st_transform(Vuln, target_crs)

# Helper to read Excel and convert to sf
read_sf_points <- function(file, lat_col = "Latitude", lon_col = "Longitude", crs = 4326) {
  read_excel(file) %>%
    mutate(
      latitude  = suppressWarnings(as.numeric(.data[[lat_col]])),
      longitude = suppressWarnings(as.numeric(.data[[lon_col]]))
    ) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = crs, remove = FALSE)
}

# Elevation raster — load only, do NOT reproject to save disk space
elevation <- rast("/Users/kyndrashea/pfas-project/data/source/LF2020_Elev_CONUS/Tif/LF2020_Elev_CONUS.tif")
elev_crs <- crs(elevation)

# HUC12
huc12 <- st_read("/Users/kyndrashea/pfas-project/data/source/HUC12/WBD_HUC12_CONUS_pulled10262020/WBD_HUC12_CONUS_pulled10262020.shp")
huc12 <- st_transform(huc12, target_crs)

# Assign HUC12 to wells
Vuln_m <- st_join(Vuln_m, huc12["huc12"])

# Non-NAICS sources

cwns_path <- "/Users/kyndrashea/pfas-project/data/source/CWNS Data/2022CWNS_NATIONAL_APR2024"
cwns_tables <- list.files(cwns_path, pattern = "\\.csv$", full.names = TRUE) %>%
  set_names(nm = basename(.) %>% gsub(".csv$", "", .)) %>%
  map(read_csv)

treatment_sf <- cwns_tables$FACILITIES %>%
  filter(INFRASTRUCTURE_TYPE == "Wastewater") %>%
  left_join(cwns_tables$FACILITY_TYPES, by = "CWNS_ID") %>%
  filter(FACILITY_TYPE == "Treatment Plant") %>%
  left_join(cwns_tables$PHYSICAL_LOCATION, by = "CWNS_ID") %>%
  distinct(CWNS_ID, .keep_all = TRUE) %>%
  select(CWNS_ID, FACILITY_NAME, LATITUDE, LONGITUDE, CITY, STATE_CODE) %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(target_crs)

onsite_sf <- cwns_tables$FACILITIES %>%
  filter(INFRASTRUCTURE_TYPE == "Decentralized Wastewater Treatment") %>%
  left_join(cwns_tables$FACILITY_TYPES, by = "CWNS_ID") %>%
  filter(FACILITY_TYPE == "Onsite Wastewater Treatment System") %>%
  left_join(cwns_tables$PHYSICAL_LOCATION, by = "CWNS_ID") %>%
  distinct(CWNS_ID, .keep_all = TRUE) %>%
  select(CWNS_ID, FACILITY_NAME, LATITUDE, LONGITUDE, CITY, STATE_CODE) %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(target_crs)

federal_sf     <- read_sf_points("/Users/kyndrashea/pfas-project/data/source/Federal Sites_PFASAnalytic.xlsx")
discharge_sf   <- read_sf_points("/Users/kyndrashea/pfas-project/data/source/Discharge_PFASAnalytics.xlsx")
superfund_pfas <- read_sf_points("/Users/kyndrashea/pfas-project/data/source/Superfund_PFASAnalytic.xlsx")
spills_sf      <- read_sf_points("/Users/kyndrashea/pfas-project/data/source/SpillsData_PFASAnalytic.xlsx")

superfund_npl <- st_read(
  "/Users/kyndrashea/pfas-project/data/source/Superfund/Superfund_National_Priorities_List_(NPL)_Sites_with_Status_Information/Superfund_National_Priorities_List_(NPL)_Sites_with_Status_Information.shp"
) %>% filter(!st_is_empty(.))

part139_sf <- st_read(
  "/Users/kyndrashea/pfas-project/data/source/NTAD_Aviation_Facilities_6916451147882473169/Aviation_Facilities.shp"
) %>%
  st_transform(target_crs) %>%
  filter(!is.na(FAR_139_TY))

federal_known     <- federal_sf %>% filter(`PFAS Presence` == "Known Detection")
federal_suspected <- federal_sf %>% filter(`PFAS Presence` == "Suspected")

# NAICS sources

frs_national <- read_csv(
  "/Users/kyndrashea/pfas-project/data/source/national_combined/NATIONAL_FACILITY_FILE.CSV",
  col_types = cols(TRIBAL_LAND_CODE = col_character())
)
frs_naics <- read_csv(
  "/Users/kyndrashea/pfas-project/data/source/national_combined/NATIONAL_NAICS_FILE.CSV"
)

frs_combined <- frs_national %>%
  left_join(frs_naics, by = "REGISTRY_ID") %>%
  mutate(NAICS_STR = as.character(NAICS_CODE))

target_naics <- c(
  "211120", "211130",
  "212221", "212291", "212299", "212393",
  "313110", "313210", "313220", "313230", "313310", "313320",
  "314110", "314910", "314999",
  "316110", "316998",
  "322121", "322130", "322219", "322220",
  "323111", "323120",
  "324110", "324191",
  "325120", "325130", "325180", "325193", "325199",
  "325211", "325212", "325220", "325510",
  "325611", "325612", "325613", "325998",
  "326112", "326113", "326121", "326130", "326211",
  "327215", "327310",
  "331110", "331313", "331410",
  "332812", "332813", "332999",
  "333249", "333316", "333318",
  "334220", "334310", "334412", "334413", "334418", "334419", "334512",
  "335911", "335912", "335931", "335999",
  "424690", "442291", "488119", "561740",
  "562112", "562211", "562212", "562213", "562219",
  "811420", "928110"
)

naics_all_sf <- frs_combined %>%
  filter(NAICS_STR %in% target_naics) %>%
  filter(!is.na(LATITUDE83), !is.na(LONGITUDE83)) %>%
  st_as_sf(coords = c("LONGITUDE83", "LATITUDE83"), crs = 4269) %>%
  st_transform(target_crs)

# --- Combine all sources for collapsed predictors ---

harmonize_sf <- function(x) {
  x %>% select(geometry) %>% st_transform(target_crs)
}

all_pfas_sources <- bind_rows(
  harmonize_sf(federal_known),
  harmonize_sf(federal_suspected),
  harmonize_sf(discharge_sf),
  harmonize_sf(superfund_pfas),
  harmonize_sf(superfund_npl),
  harmonize_sf(spills_sf),
  harmonize_sf(part139_sf),
  harmonize_sf(treatment_sf),
  harmonize_sf(onsite_sf),
  harmonize_sf(naics_all_sf)
)

# Attach HUC12 and elevation to combined sources (needed for impact score)
all_pfas_sources <- st_join(all_pfas_sources, huc12["huc12"])

elev_vals <- terra::extract(elevation, terra::vect(st_transform(all_pfas_sources, elev_crs)))
all_pfas_sources$elev <- elev_vals[, 2]
all_pfas_sources$elev[all_pfas_sources$elev < -1000] <- NA

# Attach HUC12 and elevation to wells (needed for impact score)
if (!"huc12" %in% names(Vuln_m)) {
  Vuln_m <- st_join(Vuln_m, huc12["huc12"])
}
if (!"elev" %in% names(Vuln_m)) {
  well_elev <- terra::extract(elevation, terra::vect(st_transform(Vuln_m, elev_crs)))
  Vuln_m$elev <- well_elev[, 2]
  Vuln_m$elev[Vuln_m$elev < -1000] <- NA
}

# ===========================================================================
# STEP 3: Impact score function
# ===========================================================================

impact_score <- function(from, to, max_dist = 5000,
                         decay_function = function(d_km) 1 / exp(d_km)) {
  from_huc  <- from$huc12
  from_elev <- from$elev
  out <- numeric(nrow(from))

  for (i in seq_len(nrow(from))) {
    dists    <- as.numeric(st_distance(from[i, ], to))
    in_range <- which(dists <= max_dist)

    if (length(in_range) == 0) {
      out[i] <- 0
      next
    }

    in_range <- in_range[order(dists[in_range])][1:min(100, length(in_range))]
    cand     <- to[in_range, ]
    raw_dist <- dists[in_range]

    valid <- which(
      cand$huc12 == from_huc[i] &
        cand$elev >= from_elev[i]
    )

    if (length(valid) == 0) {
      out[i] <- 0
    } else {
      d_km   <- raw_dist[valid] / 1000
      out[i] <- sum(decay_function(d_km))
    }
  }
  out
}

# ===========================================================================
# STEP 4: Compute impact_pfas_sites (collapsed NAICS + all sources)
# ===========================================================================

cat("Computing impact_pfas_sites...\n")
Vuln[["impact_pfas_sites"]] <- impact_score(Vuln_m, all_pfas_sources, max_dist = 5000)

cat("impact_pfas_sites summary:\n")
print(summary(Vuln$impact_pfas_sites))
cat("Non-zero wells:", sum(Vuln$impact_pfas_sites > 0), "\n")

# ===========================================================================
# STEP 5: Compute dist_pfas_sites — minimum distance to any PFAS source
# ===========================================================================

cat("Finding nearest PFAS source for each well...\n")
nearest_idx <- st_nearest_feature(Vuln_m, all_pfas_sources)

cat("Computing distances...\n")
Vuln[["dist_pfas_sites"]] <- as.numeric(
  st_distance(Vuln_m, all_pfas_sources[nearest_idx, ], by_element = TRUE)
)

cat("dist_pfas_sites summary (meters):\n")
print(summary(Vuln$dist_pfas_sites))
cat("Wells within 5km:", sum(Vuln$dist_pfas_sites <= 5000), "\n")
cat("Wells within 1km:", sum(Vuln$dist_pfas_sites <= 1000), "\n")

# ===========================================================================
# STEP 5b: Compute per-source distance variables (meters)
# ===========================================================================

compute_dist <- function(wells_m, sources, var_name) {
  cat("Computing", var_name, "...\n")
  sources_m <- st_transform(sources, target_crs)
  sources_m <- sources_m[!st_is_empty(sources_m), ]
  nearest_idx <- st_nearest_feature(wells_m, sources_m)
  as.numeric(st_distance(wells_m, sources_m[nearest_idx, ], by_element = TRUE))
}

Vuln[["dist_fed_known"]]        <- compute_dist(Vuln_m, federal_known,     "dist_fed_known")
Vuln[["dist_fed_suspected"]]    <- compute_dist(Vuln_m, federal_suspected,  "dist_fed_suspected")
Vuln[["dist_discharge"]]        <- compute_dist(Vuln_m, discharge_sf,       "dist_discharge")
Vuln[["dist_superfund_pfas"]]   <- compute_dist(Vuln_m, superfund_pfas,     "dist_superfund_pfas")
Vuln[["dist_superfund_npl"]]    <- compute_dist(Vuln_m, superfund_npl,      "dist_superfund_npl")
Vuln[["dist_spills"]]           <- compute_dist(Vuln_m, spills_sf,          "dist_spills")
Vuln[["dist_part139_airports"]] <- compute_dist(Vuln_m, part139_sf,         "dist_part139_airports")
Vuln[["dist_treatment_plants"]] <- compute_dist(Vuln_m, treatment_sf,       "dist_treatment_plants")
Vuln[["dist_onsite_systems"]]   <- compute_dist(Vuln_m, onsite_sf,          "dist_onsite_systems")

cat("All distance variables computed.\n")
cat("dist_fed_known summary:\n");         print(summary(Vuln$dist_fed_known))
cat("dist_fed_suspected summary:\n");    print(summary(Vuln$dist_fed_suspected))
cat("dist_discharge summary:\n");        print(summary(Vuln$dist_discharge))
cat("dist_superfund_pfas summary:\n");   print(summary(Vuln$dist_superfund_pfas))
cat("dist_superfund_npl summary:\n");    print(summary(Vuln$dist_superfund_npl))
cat("dist_spills summary:\n");           print(summary(Vuln$dist_spills))
cat("dist_part139_airports summary:\n"); print(summary(Vuln$dist_part139_airports))
cat("dist_treatment_plants summary:\n"); print(summary(Vuln$dist_treatment_plants))
cat("dist_onsite_systems summary:\n");   print(summary(Vuln$dist_onsite_systems))

# ===========================================================================
# STEP 6: Write output
# ===========================================================================

output_path <- "/Users/kyndrashea/pfas-project/data/output/full_pfas_covars.gpkg"
st_write(Vuln, output_path, delete_dsn = TRUE)
cat("Written to:", output_path, "\n")