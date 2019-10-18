# Created by Yixin Sun October 2018
# Finding distance of di leases to infrastructure
root <- getwd()
while (basename(root) != "texas") {
  root <- dirname(root)
}

library(tidyverse)
library(lubridate)
library(sf)
library(tictoc)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code/functions/utils.R"))
source(file.path(root, "code", "texas_constants.R"))

start <- Sys.time()


# ===========================================================================
# Intersect leases with infrastructure
# ===========================================================================
# export one infrastructure object at a time, otherwise we run into 
# memory issues
leases_state <- st_transform(leases_state, utm_crs)

load(file.path(shape, "roads.Rda"))
tic("roads")
roads <- infra_dist(leases_state, roads, "DistRoads", "Lease_Number")
toc()

gc()
load(file.path(shape, "waterbodies.Rda"))
tic("waterbodies")
waterbodies <-
  infra_dist(leases_state, waterbodies, "DistWaterbodies", "Lease_Number")
toc()


load(file.path(shape, "river_streams.Rda"))
tic("river_streams")
river_streams <-
  infra_dist(leases_state, river_streams, "DistRiverStreams", "Lease_Number")
toc()

gc()

infrastructure_leases <-
  roads %>%
  full_join(waterbodies) %>%
  full_join(river_streams)

if(nrow(infrastructure_leases) != nrow(leases_state)){
  stop("rows do not match up")
}

print(paste("Finished in", Sys.time() - start))

leases_state <- st_transform(leases_state, main_crs)
