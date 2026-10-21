################################################################################
# primary authors: jahred liddie
# purpose: create full CONUS grid
# date created: 10/21/2026
################################################################################

library(sf)
library(usmap)
library(here)
library(tidyverse)

###### grid characteristics:
target_crs <- 5070  # NAD83 / CONUS Albers (meters)

grid_size <- 1000 # 1 km

###### CONUS map:
CONUS <- usmap::us_map(exclude = c("AK", "HI"))

CONUS <- st_transform(CONUS, crs = target_crs)

###### create and subset grid:
exposure_surface_grid <- st_make_grid(
  x = CONUS,
  cellsize = 1000, 
  what = "centers"
)

# verifying grid size: dist between vertical/horizontal pts should be 1 km
exposure_surface_grid[1]; exposure_surface_grid[2]
st_distance(exposure_surface_grid[1], exposure_surface_grid[2])

exposure_surface_grid <- exposure_surface_grid[CONUS, ]

# create grid ID:
exposure_surface_grid <- st_sf(grid_id = paste0("point_", 
                                                sprintf("%06d", 
                                                        seq_along(exposure_surface_grid)), 
                                                sep = ""),
                               geometry = exposure_surface_grid)

st_write(exposure_surface_grid, here("data", 
                                     "07_SL_validation",
                                     "01_nonextracted_CONUS_grid",
                                     "nonextracted_CONUS_grid.geojson")
)

