# created by Yixin Sun 11/29/2017
# Reading in landcover data and collapsing by lease
# this uses the raster package right now, but keep an eye on 
# http://r-spatial.org/r/2017/11/23/stars1.html to see if it's implemented
# takes 42 min on a computer with 16 GB of RAM
# updated by Thom Covert in August 2019
#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
root <- getwd()
while(basename(root) != "texas") {
  root = dirname(root)
}

library(tidyverse)
library(raster)
library(sf)
library(furrr)
library(purrr)
library(tictoc)
library(lubridate)

P <- 3
G <- 10

source(file.path(root, "code/texas_constants.R"))
source(file.path(root, "code/functions/utils.R"))
source(file.path(root, "code", "paths.R"))


#===========================================================================
# reading in raster data
#===========================================================================
landcover <-
  file.path(raw_shape, "Land_Cover/landcover") %>%
  raster 
main_proj <- projection(landcover)

load(file.path(gen, "leases_state.Rda"))
leases <- 
  leases_state %>%
  st_transform(main_proj) 

leases_df <- split(leases, sample(rep(1:(P * G), nrow(leases) / (P * G))))
plan(multisession, .init = P)

landcover_lease <- 
  leases_df %>%
  future_map_dfr(function(x) st_extract_group(x, Lease_Number, landcover), 
                 .options = future_options(seed = T)) %>%
  rename(value = Var1) %>%
  mutate(value = as.numeric(value)) %>%
  left_join(classification_dict) %>%
  dplyr::select(-value) %>%
  filter(!is.na(Classification)) %>%
  group_by(Lease_Number) %>%
  mutate(Total = sum(Freq)) %>%
  ungroup %>% 
  tidyr::spread(Classification, Freq) %>%
  dplyr::select(-contains("<NA>")) %>%
  mutate_if(is.numeric, funs(if_else(is.na(.), 0, as.numeric(.)))) %>%
  mutate_at(vars(-Lease_Number, -Total), funs(. / Total)) %>% 
  dplyr::select(-Total) 

# make sure percents actually add up to 1
landcover_lease <-
  landcover_lease %>%
  mutate(total = rowSums(.[2:ncol(landcover_lease)])) %>%
  mutate_at(vars(-Lease_Number, -total),
            ~ if_else(total != 1, . / total, .)) %>%
  dplyr::select(-total)



# aggregate landcover categories to more general categories
landcover_lease <-
  landcover_lease %>%
  mutate(CoverDeveloped = rowSums(.[c("Developed_OS", "Developed_LI", 
                                      "Developed_MI", "Developed_HI")]),
    CoverWater = Open_Water, 
    CoverBarren = Barren, 
    CoverForest = rowSums(.[c("Deciduous_Forest", "Evergreen_Forest", 
                              "Mixed_Forest")]),
    CoverShrub = Shrub_Scrub,
    CoverGrass = Grassland, 
    CoverCultivated = rowSums(.[c("Pasture", "Cultivated_Crops")]),
    CoverWetlands = rowSums(.[c("Woody_Wetlands", "Herbaceous_Wetlands")])) 

save(landcover_lease, 
     file = file.path(gen, "landcover_lease.Rda"))


