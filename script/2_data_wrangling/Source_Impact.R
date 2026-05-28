# 5/22 Run 
# Per 5/21 meeting, no nearest_distance. Impact scores only. 
# Helper function includes the closest 100 or fewer sources for efficiency. 
# New NAICS list yet to be evaluated by Cindy. 

# Set up
library(xml2)
library(tidyverse)
library(readr)
library(dplyr)
library(sf)
library(stringr)
library(readxl)
library(nngeo)
library(tools)
library(terra)

# Establish target CRS for uniform spatial standards
target_crs <- 5070   # NAD83 / CONUS Albers (meters)

# Read in Kyndra's physical vulnerability data
Vuln0505 <- st_read("/Users/ellenwei/Desktop/PFAS R CODE/0505/wells_covars.gpkg copy")

# Clean out old data from my side
Vuln0505 <- Vuln0505 %>%
  select(
    -starts_with("dist_naics"),
    -any_of(c(
      "dist_fed_known_m", "dist_fed_suspected_m", "dist_discharge_m",
      "dist_superfund_pfas_m", "dist_superfund_npl_m", "dist_spills_m",
      "dist_part139_airports_m", "dist_treatment_plants_m",
      "dist_onsite_systems_m"
    ))
  )

Vuln0505 <- Vuln0505 %>%
  select(-starts_with("dist_naics"))

# Reproject kyndra's original file
Vuln0505_m <- st_transform(Vuln0505, target_crs)

# Helper to read Excel and convert to sf
read_sf_points <- function(file, lat_col = "Latitude", lon_col = "Longitude", crs = 4326) {
  read_excel(file) %>%
    mutate(
      latitude = suppressWarnings(as.numeric(.data[[lat_col]])),
      longitude = suppressWarnings(as.numeric(.data[[lon_col]]))
    ) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = crs, remove = FALSE)
}

# Read in continuous elevation .tif and reproject to target crs 
elevation <- rast("/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/LF2020_Elev_CONUS/Tif/LF2020_Elev_CONUS.tif")
elevation <- terra::project(elevation, paste0("EPSG:", target_crs))

# Read in HUC12 file and reproject to target crs
huc12 <- st_read("/Users/ellenwei/Desktop/PFAS R CODE/HUC12/WBD_HUC12_CONUS_pulled10262020/WBD_HUC12_CONUS_pulled10262020.shp")
huc12 <- st_transform(huc12, target_crs)

# Assign HUC12 to wells as well 
Vuln0505_m <- st_join(Vuln0505_m, huc12["huc12"])

# Read in PFAS Analytic tools industry sector download
IndustrySectors <- read_excel("/Users/ellenwei/Desktop/industrysectors.xlsx")

# Read in the PFAS analytic tools list of industries
EPAIndustryList <- read_excel("/Users/ellenwei/Desktop/PFASHandlingIndustrySectors-Apr2023-Pub.xlsx", sheet = 2)

# View unique values the naics codes of list of industries
unique(EPAIndustryList$`2017 NAICS Code`)

# Read Wastewater Data
cwns_path <- "/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/CWNS Data/2022CWNS_NATIONAL_APR2024"
cwns_tables <- list.files(cwns_path, pattern = "\\.csv$", full.names = TRUE) %>%
  set_names(nm = basename(.) %>% gsub(".csv$", "", .)) %>%
  map(read_csv)

treatment_plants_unique <- cwns_tables$FACILITIES %>%
  filter(INFRASTRUCTURE_TYPE == "Wastewater") %>%
  left_join(cwns_tables$FACILITY_TYPES, by = "CWNS_ID") %>%
  filter(FACILITY_TYPE == "Treatment Plant") %>%
  left_join(cwns_tables$PHYSICAL_LOCATION, by = "CWNS_ID") %>%
  distinct(CWNS_ID, .keep_all = TRUE) %>%
  select(CWNS_ID, FACILITY_NAME, LATITUDE, LONGITUDE, CITY, STATE_CODE)

onsite_systems_unique <- cwns_tables$FACILITIES %>%
  filter(INFRASTRUCTURE_TYPE == "Decentralized Wastewater Treatment") %>%
  left_join(cwns_tables$FACILITY_TYPES, by = "CWNS_ID") %>%
  filter(FACILITY_TYPE == "Onsite Wastewater Treatment System") %>%
  left_join(cwns_tables$PHYSICAL_LOCATION, by = "CWNS_ID") %>%
  distinct(CWNS_ID, .keep_all = TRUE) %>%
  select(CWNS_ID, FACILITY_NAME, LATITUDE, LONGITUDE, CITY, STATE_CODE)

treatment_sf <- treatment_plants_unique %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(target_crs)

onsite_sf <- onsite_systems_unique %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(target_crs)

# Read federal source data
federal_sf       <- read_sf_points("/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/Federal Sites_PFASAnalytic.xlsx")

# Read discharge source data
discharge_sf     <- read_sf_points("/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/Discharge_PFASAnalytics.xlsx")

# Read superfund source data
superfund_pfas   <- read_sf_points("/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/Superfund_PFASAnalytic.xlsx")

# Read spills source data
spills_sf        <- read_sf_points("/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/SpillsData_PFASAnalytic.xlsx")

# Read in general Superfund NPL shapefile
superfund_npl <- st_read(
  "/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/Superfund/Superfund_National_Priorities_List_(NPL)_Sites_with_Status_Information/Superfund_National_Priorities_List_(NPL)_Sites_with_Status_Information.shp"
) %>% filter(!st_is_empty(.))

# Read in FAA shapefile
aviation_facilities_sf <- st_read(
  "/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/NTAD_Aviation_Facilities_6916451147882473169/Aviation_Facilities.shp"
) %>% st_transform(target_crs)

# Filter by p139 airports
part139_sf <- aviation_facilities_sf %>% dplyr::filter(!is.na(FAR_139_TY))
airports_139_m <- st_transform(part139_sf, target_crs)

# Filter federal sites by PFAS presence
federal_known     <- federal_sf %>% filter(`PFAS Presence` == "Known Detection")
federal_suspected <- federal_sf %>% filter(`PFAS Presence` == "Suspected")

# NAICS CODES
# Read facility data
frs_national <- read_csv(
  "/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/national_combined/NATIONAL_FACILITY_FILE.CSV",
  col_types = cols(
    TRIBAL_LAND_CODE = col_character()
  )
)

# Read NAICS data
frs_naics <- read_csv(
  "/Users/ellenwei/Desktop/PFAS R CODE/PFAS Source Indicator Data/national_combined/NATIONAL_NAICS_FILE.CSV"
)

# Join datasets
frs_combined <- frs_national %>% 
  left_join(frs_naics, by = "REGISTRY_ID")

# Convert NAICS codes to character
frs_combined <- frs_combined %>% 
  mutate(NAICS_STR = as.character(NAICS_CODE))

# Exact NAICS codes to match
target_naics <- c(
  "211120",
  "211130",
  "212221",
  "212291",
  "212299",
  "212393",
  "313110",
  "313210",
  "313220",
  "313230",
  "313310",
  "313320",
  "314110",
  "314910",
  "314999",
  "316110",
  "316998",
  "322121",
  "322130",
  "322219",
  "322220",
  "323111",
  "323120",
  "324110",
  "324191",
  "325120",
  "325130",
  "325180",
  "325193",
  "325199",
  "325211",
  "325212",
  "325220",
  "325510",
  "325611",
  "325612",
  "325613",
  "325998",
  "326112",
  "326113",
  "326121",
  "326130",
  "326211",
  "327215",
  "327310",
  "331110",
  "331313",
  "331410",
  "332812",
  "332813",
  "332999",
  "333249",
  "333316",
  "333318",
  "334220",
  "334310",
  "334412",
  "334413",
  "334418",
  "334419",
  "334512",
  "335911",
  "335912",
  "335931",
  "335999",
  "424690",
  "442291",
  "488119",
  "561740",
  "562112",
  "562211",
  "562212",
  "562213",
  "562219",
  "811420",
  "928110"
)

# Filter ONLY exact matches
fac_exact <- frs_combined %>%
  filter(NAICS_STR %in% target_naics) %>%
  filter(!is.na(LATITUDE83), !is.na(LONGITUDE83)) %>%
  st_as_sf(coords = c("LONGITUDE83", "LATITUDE83"), crs = 4269) %>%
  st_transform(target_crs)

# split up the naics codes for the source list
naics_split <- split(fac_exact, fac_exact$NAICS_STR)

sources_naics <- lapply(naics_split, function(x) x)

names(sources_naics) <- paste0("naics_", names(naics_split))

# Compile all sources
sources <- c(
  list(
    fed_known        = federal_known,
    fed_suspected    = federal_suspected,
    discharge        = discharge_sf,
    superfund_pfas   = superfund_pfas,
    superfund_npl    = superfund_npl,
    spills           = spills_sf,
    part139_airports = airports_139_m,
    treatment_plants = treatment_sf,
    onsite_systems   = onsite_sf
  ),
  sources_naics
)

# Transform all sources to target CRS if not empty
sources <- lapply(sources, function(x) {
  if(!is.null(x) && nrow(x) > 0) st_transform(x, target_crs) else NULL
})

# Apply the huc12 codes to all sources
sources <- lapply(sources, function(x) {
  if (!is.null(x) && nrow(x) > 0) {
    st_join(x, huc12["huc12"])
  } else {
    NULL
  }
})

# Apply raster elevation to all point-source sf layers
sources <- lapply(sources, function(x) {
  if (!is.null(x) && nrow(x) > 0) {
    
    # Extract elevation values from raster
    elev_vals <- terra::extract(
      elevation,
      terra::vect(x)
    )
    # Add extracted elevation column to sf object
    x$elev <- elev_vals[, 2]
    
    # Remove impossible elevation values
    x$elev[x$elev < -1000] <- NA
    x
  } else {
    NULL
  }
})

# Helper function for impact score 
impact_score <- function(from, to, max_dist = 5000,
                         decay_function = function(d_km) 1 / exp(d_km)) {
  nn <- st_nn(
    from,
    to,
    k = min(100, nrow(to)),
    maxdist = max_dist,
    returnDist = TRUE
  )
  from_huc  <- from$huc12
  from_elev <- from$elev
  out <- numeric(length(nn$nn))
  
  for (i in seq_along(nn$nn)) {
    idx  <- nn$nn[[i]]
    dist <- nn$dist[[i]]
    
    if (length(idx) == 0) {
      out[i] <- 0
      next
    }
    cand <- to[idx, ]
    valid <- which(
      cand$huc12 == from_huc[i] &
        cand$elev >= from_elev[i]
    )
    if (length(valid) == 0) {
      out[i] <- 0
    } else {
      d_km <- as.numeric(dist[valid]) / 1000
      out[i] <- sum(decay_function(d_km))
    }
  }
  out
}

# Loop over all sources
for (nm in names(sources)) {
  Vuln0505[[paste0("impact_", nm)]] <-
    impact_score(Vuln0505_m, sources[[nm]], max_dist = 5000)
}


# Write out the final GeoPackage
output_file_v6 <- "/Users/ellenwei/Desktop/PFAS R CODE/Full_Impact/full_pfas_covars.gpkg"

st_write(Vuln0505, output_file_v6)
