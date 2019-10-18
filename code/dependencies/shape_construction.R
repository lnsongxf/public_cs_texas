#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
root <- getwd()
while(basename(root) != "texas") {
  root = dirname(root)
}

library(tidyverse)
library(lubridate)
library(sf)
library(lwgeom)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))


# ==========================================================================
# function to intersect leases with grids
# =========================================================================
st_texas_grid <- function(bb, cs){
  bb %>%
    st_make_grid(cellsize = c(cs, cs)) %>%
    st_sf(GridID = seq(1, length(.)), geometry = ., crs = utm_crs) %>%
    st_transform(main_crs)
}


#==============================================================================
# LOAD IN LEASES GEOMS AND INTERSECT WITH JITTERED GRIDS
#==============================================================================
load(file.path(ddir, "shape_files/PSF/psf.Rda"))

# use bounding box of parcels to create grids
parcel_bb <- 
  psf %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform(utm_crs)

texas_grid5 <- st_texas_grid(parcel_bb, mi_to_m2 * 5)
texas_grid10 <- st_texas_grid(parcel_bb, mi_to_m2 * 10)
texas_grid20 <- st_texas_grid(parcel_bb, mi_to_m2 * 20)

dir.create(file.path(shape, "Grids"), showWarnings = FALSE)
save(texas_grid5, file = file.path(int, "texas_grid5.Rda"))
save(texas_grid10, file = file.path(int, "texas_grid10.Rda"))
save(texas_grid20, file = file.path(int, "texas_grid20.Rda"))


# =============================================================================
# Converting surface water shapefile to sf
# =============================================================================
river_streams <-
  file.path(shape, "infrastructure",
    "usgs-rivers_tx/Rivers_Streams/Rivers_Streams.shp") %>%
  st_read(stringsAsFactors = F) %>%
  select(-Shape_Leng) %>%
  st_zm() %>%
  st_transform(utm_crs)

save(river_streams, file = file.path(int, "river_streams.Rda"))