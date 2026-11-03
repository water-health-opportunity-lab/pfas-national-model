# --------------------------------------------------------
# Building 1km by 1km CONUS Map for Physical Vulnerability Predictors
#
# - Suspected federal PFAS site
# - Superfund site
# - National Priorities List site
# - PFAS spill site
# - Part 139 airport
# - Treatment plant
# - Collapsed NAICS
#
# Author: Kyndra Shea & Ellen Wei
# Last updated: 2026-11-02
# --------------------------------------------------------

### LOAD PACKAGES ###

library(dplyr)
library(FNN)
library(readxl)
library(sf)
library(terra)

### SPATIAL MODELING STANDARDS ###

grid_crs <- "EPSG:5070"   # NAD83 / CONUS Albers
dist_cap <- 50000   # meters

### LOAD CONUS GRID ###

grid_path <- file.path(
  "/Users/kyndrashea/pfas-project/data/output/",
  "conus_map_physical_predictors.gpkg"
)

# Read cell_id only

grid_vect <- terra::vect(
  grid_path,
  query = "SELECT cell_id FROM conus_physical_predictors"
)

grid_covars <- data.frame(cell_id = grid_vect$cell_id)

# Centroid coordinates (EPSG:5070)

xy <- terra::crds(terra::centroids(grid_vect))

### HELPER FUNCTION ###

# Distance (m) from each centroid to the nearest source point
dist_nearest <- function(src_sf) {
  src_xy <- sf::st_coordinates(sf::st_transform(src_sf, grid_crs))
  FNN::get.knnx(data = src_xy, query = xy, k = 1)$nn.dist[, 1]
}

### SUSPECTED FEDERAL PFAS SITE ###

federal_path <- file.path(
  "/Users/kyndrashea/pfas-project/data/input",
  "Source/Federal Sites_PFASAnalytic.xlsx"
)

federal_sf <- readxl::read_excel(federal_path) |>
  dplyr::mutate(
    latitude  = suppressWarnings(as.numeric(Latitude)),
    longitude = suppressWarnings(as.numeric(Longitude))
  ) |>
  dplyr::filter(
    !is.na(latitude),
    !is.na(longitude),
    dplyr::between(latitude, -90, 90),
    dplyr::between(longitude, -180, 180)
  ) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


grid_covars$dist_fed_suspected <- dist_nearest(
  dplyr::filter(federal_sf, `PFAS Presence` == "Suspected")
)

# Cap the distances to 50,000 meters

for (nm in "dist_fed_suspected") {
  grid_covars[[nm]] <- pmin(grid_covars[[nm]], dist_cap)
}

### SUPERFUND SITE ###

superfund_pfas_path <- file.path(
  "/Users/kyndrashea/pfas-project/data/input",
  "Source/Superfund_PFASAnalytic.xlsx"
)

superfund_pfas_sf <- readxl::read_excel(superfund_pfas_path) |>
  dplyr::mutate(
    latitude  = suppressWarnings(as.numeric(Latitude)),
    longitude = suppressWarnings(as.numeric(Longitude))
  ) |>
  dplyr::filter(
    !is.na(latitude),
    !is.na(longitude),
    dplyr::between(latitude, -90, 90),
    dplyr::between(longitude, -180, 180)
  ) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

grid_covars$dist_superfund_pfas <- dist_nearest(superfund_pfas_sf)

# Cap the distances to 50,000 meters
grid_covars$dist_superfund_pfas <- pmin(
  grid_covars$dist_superfund_pfas, dist_cap
)

### SUPERFUND NPL SITE ###

superfund_npl_path <- file.path(
  "/Users/kyndrashea/pfas-project/data/input",
  "Source/Superfund/",
  "Superfund_National_Priorities_List_(NPL)_Sites_with_Status_Information.shp"
)

superfund_npl_sf <- sf::st_read(superfund_npl_path, quiet = TRUE) |>
  dplyr::filter(!sf::st_is_empty(geometry))

grid_covars$dist_superfund_npl <- dist_nearest(superfund_npl_sf)

# Cap the distances to 50,000 meters
grid_covars$dist_superfund_npl <- pmin(
  grid_covars$dist_superfund_npl, dist_cap
)

### PFAS SPILL SITE ###

spills_path <- file.path(
  "/Users/kyndrashea/pfas-project/data/input",
  "Source/SpillsData_PFASAnalytic.xlsx"
)

spills_sf <- readxl::read_excel(spills_path) |>
  dplyr::mutate(
    latitude  = suppressWarnings(as.numeric(Latitude)),
    longitude = suppressWarnings(as.numeric(Longitude))
  ) |>
  dplyr::filter(
    !is.na(latitude),
    !is.na(longitude),
    dplyr::between(latitude, -90, 90),
    dplyr::between(longitude, -180, 180)
  ) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

grid_covars$dist_spills <- dist_nearest(spills_sf)

# Cap the distances to 50,000 meters
grid_covars$dist_spills <- pmin(
  grid_covars$dist_spills, dist_cap
)

### PART 139 AIRPORT ###

part139_airports_path <- file.path(
  "/Users/kyndrashea/pfas-project/data/input",
  "Source/NTAD/Aviation_Facilities.shp"
)

part139_airports_sf <- sf::st_read(part139_airports_path, quiet = TRUE) |>
  dplyr::filter(
    !is.na(FAR_139_TY),
    !sf::st_is_empty(geometry)
  )

grid_covars$dist_part139_airports <- dist_nearest(part139_airports_sf)

# Cap the distances to 50,000 meters
grid_covars$dist_part139_airports <- pmin(
  grid_covars$dist_part139_airports, dist_cap
)

### TREATMENT PLANT ###

cwns_dir <- file.path(
  "/Users/kyndrashea/pfas-project/data/input",
  "CWNS Data 2022"
)

cwns_facilities <- readr::read_csv(
  file.path(cwns_dir, "FACILITIES.csv"), show_col_types = FALSE
)
cwns_types <- readr::read_csv(
  file.path(cwns_dir, "FACILITY_TYPES.csv"), show_col_types = FALSE
)
cwns_locations <- readr::read_csv(
  file.path(cwns_dir, "PHYSICAL_LOCATION.csv"), show_col_types = FALSE
) |>
  dplyr::distinct(CWNS_ID, .keep_all = TRUE)

treatment_sf <- cwns_facilities |>
  dplyr::filter(INFRASTRUCTURE_TYPE == "Wastewater") |>
  dplyr::left_join(cwns_types, by = "CWNS_ID") |>
  dplyr::filter(FACILITY_TYPE == "Treatment Plant") |>
  dplyr::distinct(CWNS_ID, .keep_all = TRUE) |>
  dplyr::left_join(cwns_locations, by = "CWNS_ID") |>
  dplyr::mutate(
    latitude  = suppressWarnings(as.numeric(LATITUDE)),
    longitude = suppressWarnings(as.numeric(LONGITUDE))
  ) |>
  dplyr::filter(!is.na(latitude), !is.na(longitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

grid_covars$dist_treatment_plants <- dist_nearest(treatment_sf)

# Cap the distances to 50,000 meters
grid_covars$dist_treatment_plants <- pmin(
  grid_covars$dist_treatment_plants, dist_cap
)

### COLLAPSED NAICS ###

frs_facility_path <- file.path(
  "/Users/kyndrashea/pfas-project/data/input",
  "national_combined/NATIONAL_FACILITY_FILE.csv"
)

frs_naics_path <- file.path(
  "/Users/kyndrashea/pfas-project/data/input",
  "national_combined/NATIONAL_NAICS_FILE.csv"
)

naics_for_distance <- c(
  212221, 212291, 212299, 212393,
  313210, 313230, 313320,
  314110, 314999,
  316110,
  322121, 322130, 323111,
  324110, 324191,
  325120, 325193, 325199, 325211, 325212,
  325510, 325611, 325612, 325613, 325998,
  326112, 326113, 326121, 326130, 326211,
  327215, 327310,
  332812, 332813, 332999,
  334220, 334310, 334412, 334413, 334418,
  334419, 334512,
  335911, 335912, 335931, 335999,
  424690, 442291,
  488119,
  561740,
  562112, 562211, 562212, 562213, 562219,
  811420,
  928110
)

# Facilities with any listed NAICS code

frs_naics <- readr::read_csv(
  frs_naics_path,
  col_select = c(REGISTRY_ID, NAICS_CODE),
  col_types  = readr::cols(.default = readr::col_character())
) |>
  dplyr::filter(NAICS_CODE %in% as.character(naics_for_distance)) |>
  dplyr::distinct(REGISTRY_ID)

# Facility coordinates (NAD83)

frs_facilities <- readr::read_csv(
  frs_facility_path,
  col_select = c(REGISTRY_ID, LATITUDE83, LONGITUDE83),
  col_types  = readr::cols(.default = readr::col_character())
)

naics_collapsed_sf <- frs_facilities |>
  dplyr::inner_join(frs_naics, by = "REGISTRY_ID") |>
  dplyr::mutate(
    latitude  = suppressWarnings(as.numeric(LATITUDE83)),
    longitude = suppressWarnings(as.numeric(LONGITUDE83))
  ) |>
  dplyr::filter(
    !is.na(latitude),
    !is.na(longitude),
    dplyr::between(latitude, -90, 90),
    dplyr::between(longitude, -180, 180)
  ) |>
  dplyr::distinct(REGISTRY_ID, .keep_all = TRUE) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

grid_covars$dist_naics_collapsed <- dist_nearest(naics_collapsed_sf)

# Cap the distances to 50,000 meters

grid_covars$dist_naics_collapsed <- pmin(
  grid_covars$dist_naics_collapsed, dist_cap
)

### SAVE OUTPUT ###

out_gpkg <- file.path(
  "/Users/kyndrashea/pfas-project/data/output/",
  "conus_map_predictors.gpkg"
)

# Physical predictors (attribute table only, no geometry)

phys_covars <- terra::vect(grid_path, what = "attributes")

# Join distance variables to physical predictors by cell_id

all_covars <- grid_covars |>
  dplyr::left_join(phys_covars, by = "cell_id") |>
  dplyr::relocate(names(grid_covars)[-1], .after = dplyr::last_col())

terra::values(grid_vect) <- all_covars

terra::writeVector(
  grid_vect,
  out_gpkg,
  filetype  = "GPKG",
  layer     = "conus_predictors",
  overwrite = TRUE
)