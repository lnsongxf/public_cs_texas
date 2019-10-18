# Created by Yixin Sun on January 8, 2019 to find GLO leases not in the 
# lease shapefiles
root <- getwd()
while (basename(root) != "texas") {
  root <- dirname(root)
}


library(tidyverse)
library(sf)
library(lubridate)
library(readr)


# =============================================================================
# source utils.R and file paths 
# =============================================================================
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))

# =============================================================================
# GLO lease info data
# =============================================================================
control <- 
  file.path(raw_lease, "LeaseLandFile.csv") %>%
  read_csv() %>%
  rename(Lease_Number = StateLease, 
  	ControlNum = Control) %>%
  mutate_all(funs(if_else(. == "NULL", NA_character_, .))) 

active <- 
  file.path(raw_lease, "Active/ActiveLeases.shp") %>%
  st_read(stringsAsFactors = F) %>%
  rename(Lease_Number = LEASE_NUMB)

inactive <- 
  file.path(raw_lease, "Inactive/InActive_OilGas_Leases.shp") %>%
  st_read(stringsAsFactors = F) %>%
  rename(Lease_Number = LEASE_NUMB)

# take out leases that we have data for
lease_info <-
  file.path(raw_lease, "tblMineralLeaseAllInfo.csv") %>%
  read_csv(col_types = cols(.default = "c")) %>%
  select(Lease_Number = varLeaseNumber, 
         Lease_Status = varMineralLeaseStatus,
         Lease_Status_Date = datLeaseStatusUpdateDate,
         Effective_Date = datMineralLeaseEffectiveDate , 
         Expire_Date = datMineralLeasePrimaryTermEndDate  , 
         Term =  intMineralLeasePrimaryTermYear, 
         Bonus = monMineralLeaseBonusAmount , 
         Original_Gross = decMineralLeaseOriginalGrossAcres, 
         Original_Net = decMineralLeaseOriginalNetAcres, 
         Current_Gross = decMineralLeaseCurrentGrossAcres, 
         Current_Net = decMineralLeaseCurrentNetAcres, 
         Lessee = varMineralLeaseLesseeLessorCompanyName, 
         Lessor = varMineralLeaseLessor , 
         Undivided = varMineralLeaseUndividedInterestIndicator, 
         Oil_Royalty = varOilMineralLeaseRoyaltyDecimal, 
         Gas_Royalty = varGasMineralLeaseRoyaltyDecimal, 
         Condensate_Royalty = varSulfurMineralLeaseRoyaltyDecimal, 
         Depth_From = intMineralLeaseDepthFromAmount, 
         Depth_To = intMineralLeaseDepthToAmount) %>%
  distinct() %>%
  anti_join(active) %>%
  anti_join(inactive) %>%
  left_join(select(control, Lease_Number, ControlNum)) %>%
  mutate_all(funs(if_else(. == "NULL", NA_character_, .))) %>%
  mutate_at(vars(contains("Date")), mdy) %>%
  mutate_at(vars(matches("Gross|Net|Royalty"), Bonus), as.numeric) %>%
  filter(str_detect(Lease_Number, "MF"))

# =============================================================================
# use control number to match to other leases and to psf data
# =============================================================================
# combine psf and lease shps into one shapefile
load(file.path(shape, "psf.Rda"))

control_shp <-
  rbind(active, inactive) %>%
  select(Lease_Number) %>%
  left_join(control) %>%
  select(ControlNum) %>%
  st_transform(main_crs) %>%
  rbind(select(psf, ControlNum)) %>%
  mutate(Acres = as.numeric(st_area(.)) * m2_to_acre) %>%
  filter(!is.na(ControlNum))

dropped <-
  inner_join(control_shp, lease_info) %>%
  mutate(AcreDiff = abs(Acres - Original_Gross) / Original_Gross) %>%
  group_by(Lease_Number) %>%
  arrange(AcreDiff) %>%
  filter(row_number() == 1) %>%
  ungroup  %>%
  mutate(Dropped = TRUE) %>%
  select(-AcreDiff, -ControlNum, -Acres) %>%
  mutate(id = row_number())

dropped_geom <-
  dropped %>%
  select(id)

dropped_fields <-
  dropped %>%
  st_set_geometry(NULL)

write_sf(dropped_geom,
         file.path(raw, "leases/dropped_leases/dropped_leases.shp"))

write_csv(dropped_fields,
          file.path(raw, "leases/dropped_leases/dropped_leases_fields.csv"))
