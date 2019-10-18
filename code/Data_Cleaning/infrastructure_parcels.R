# Created by Yixin Sun October 2018
# Finding distance of parcels to infrastructure
root <- getwd()
while (basename(root) != "texas") {
  root <- dirname(root)
}

library(tidyverse)
library(lubridate)
library(sf)
library(furrr)
library(tictoc)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "texas_constants.R"))

start <- Sys.time()

# ===========================================================================
# running function in parallel
# ===========================================================================
# export one infrastructure object at a time, otherwise we run into 
# memory issues
#load(file.path(build_state_lands, "state_parcels_map.Rda"))
parcels <- st_transform(state_parcels_map, utm_crs)

load(file.path(shape, "roads.Rda"))
tic("roads")
roads <-
  infra_dist(parcels, roads, "DistRoads", "ParcelID")
toc()

gc()
load(file.path(shape, "waterbodies.Rda"))
tic("waterbodies")
waterbodies <-
  infra_dist(parcels, waterbodies, "DistWaterbodies", "ParcelID")
toc()


load(file.path(shape, "river_streams.Rda"))
tic("river_streams")
river_streams <-
  infra_dist(parcels, river_streams, "DistRiverStreams", "ParcelID")
toc()

infrastructure_parcels <-
  roads %>%
  full_join(waterbodies) %>%
  full_join(river_streams)

if(nrow(infrastructure_parcels) != nrow(state_parcels_map)){
  stop("rows do not match up")
}

print(paste("Finished in", Sys.time() - start))


parcels <- st_transform(state_parcels_map, main_crs)



