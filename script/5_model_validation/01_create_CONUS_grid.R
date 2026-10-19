# ----------------------------------------
# load packages
# create CONUS grid
#   - set CRS
#   - set resolution
#   - create and save grid
# last edited: 2026-10-20
# ----------------------------------------

library(sf)
library(tidyverse)

## TODO: create grid at 1km of the CONUS and check

target_crs   <- 5070   # NAD83 / CONUS Albers (meters)
