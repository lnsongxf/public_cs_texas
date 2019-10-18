# Created by Thom Covert in September 2019
# combines all existing lease shapefile data to get definitive "status"
# information, useful in characterizing which delay rental payments were made

root <- getwd()
while (basename(root) != "texas") {
  root <- dirname(root)
}

library(tidyverse)
library(sf)
library(lubridate)


# ==============================================================================
# source utils.R and file paths 
# ==============================================================================
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))

# ==============================================================================
# Read in raw GLO data from new leases
# ==============================================================================
# new active and inactive files
active <-
  file.path(raw_lease, "New/Active/ActiveLeases.shp") %>%
  read_sf %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number = LEASE_NUMB, 
         Lease_Status = LEASE_STAT,
         Lease_Status_Date = LEASE_ST_1,
         FirstProdDate = FIRST_WE_1) %>%
  mutate(Lease_Status_Date = mdy(Lease_Status_Date),
         FirstProdDate = as_date(parse_date_time(FirstProdDate, "mdy HM"))) %>%
  mutate(source = "Active") %>%
  unique %>%
  filter(!is.na(Lease_Number))
  

inactive <-
  file.path(raw_lease, "New/Inactive/InactiveLeases.shp") %>%
  read_sf %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number = LEASE_NUMB, 
         Lease_Status = LEASE_STAT,
         Lease_Status_Date = LEASE_ST_1,
         FirstProdDate = FIRST_WE_1) %>%
  mutate(Lease_Status_Date = mdy(Lease_Status_Date),
         FirstProdDate = as_date(parse_date_time(FirstProdDate, "mdy HM"))) %>%
  mutate(source = "Inactive") %>%
  unique %>%
  filter(!is.na(Lease_Number))

lease_statuses <-
  bind_rows(active, inactive) %>%
  arrange(Lease_Number, source) %>%
  group_by(Lease_Number) %>%
  filter(row_number() == 1) %>%
  ungroup

save(lease_statuses, file = file.path(gen, "lease_statuses.Rda"))
  
