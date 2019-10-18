# created by Yixin Sun 11/29/2017
# Reading in landcover data and collapsing by parcel
# this uses the raster package right now, but keep an eye on 
# http://r-spatial.org/r/2017/11/23/stars1.html to see if it's implemented
# takes 1.07 hours on a computer with 16 GB of RAM
#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
root <- getwd()
while(basename(root) != "texas") {
  root = dirname(root)
}

library(tidyverse)
library(raster)
library(lubridate)
library(sf)
library(furrr)
library(tictoc)

source(file.path(root, "code/texas_constants.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "paths.R"))

P <- 3
G <- 10

#===========================================================================
# reading in raster data
#===========================================================================
landcover <-
  file.path(raw_shape, "Land_Cover/landcover") %>%
  raster 
main_proj <- projection(landcover)

parcels <- 
  state_parcels_map %>%
  st_transform(main_proj) 

parcels_df <- split(parcels, sample(rep(1:(P * G), nrow(parcels) / (P * G))))
plan(multisession, .init = P)

tic()
landcover_parcel <- 
  parcels_df %>%
  future_map_dfr(function(x) st_extract_group(x, ParcelID, landcover)) 
toc()

landcover_parcel <- 
  landcover_parcel %>%
  rename(value = Var1) %>%
  mutate(value = as.numeric(value)) %>%
  left_join(classification_dict) %>%
  dplyr::select(-value) %>%
  group_by(ParcelID) %>%
  mutate(Total = sum(Freq)) %>%
  ungroup %>% 
  tidyr::spread(Classification, Freq) %>%
  dplyr::select(-`<NA>`) %>%
  mutate_if(is.numeric, funs(if_else(is.na(.), 0, as.numeric(.)))) %>%
  mutate_at(vars(-ParcelID, -Total), funs(. / Total)) %>% 
  dplyr::select(-Total) 


# make sure percents actually add up to 1
landcover_parcel <-
  landcover_parcel %>%
  mutate(total = rowSums(.[2:ncol(landcover_parcel)])) %>%
  mutate_at(vars(-ParcelID, -total),
            funs(if_else(total != 1, . / total, .))) %>%
  dplyr::select(-total)



# aggregate landcover categories to more general categories
landcover_parcel <-
  landcover_parcel %>%
  mutate(CoverDeveloped = rowSums(.[c("Developed_OS",
                                      "Developed_LI", 
                                      "Developed_MI",
                                      "Developed_HI")]),
         CoverWater = Open_Water, 
         CoverBarren = Barren, 
         CoverForest = rowSums(.[c("Deciduous_Forest",
                                   "Evergreen_Forest", 
                                   "Mixed_Forest")]),
         CoverShrub = Shrub_Scrub,
         CoverGrass = Grassland, 
         CoverCultivated = rowSums(.[c("Pasture",
                                       "Cultivated_Crops")]),
         CoverWetlands = rowSums(.[c("Woody_Wetlands",
                                     "Herbaceous_Wetlands")])) 

save(landcover_parcel, 
     file = file.path(gen, "landcover_parcel.Rda"))


if(nrow(landcover_parcel) != nrow(state_parcels_map)){
  stop("Row lengths do not match up")
}
