#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
root = getwd()
while(basename(root) != "texas") {
  root = dirname(root)
}

library(tidyverse)
library(lubridate)
library(readxl)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))

options(scipen = 99999)

#=======================================================================
# Read in bonus/rental payment data
#=======================================================================
all <- 
  read_excel(file.path(raw_payments, "rentals.xlsx")) %>%
  select(Lease_Number = MFN, 
         Lease_Type = `Lease Type`,
         Effective_Date = `Effective Date`,
         Bonus = `Original Bonus AMT`,
         varDescription, 
         PaymentDate = `datReceivedDate`,
         PaymentAmount = `curPaymentAmount`) 

#=======================================================================
# leases that have a negative payment
#=======================================================================
negPayments <- 
  all %>%
  filter(PaymentAmount < 0) %>%
  select(Lease_Number, PaymentAmount, PaymentDate) %>%
  mutate(NegID = row_number())

offset <- 
  negPayments %>%
  mutate(PaymentAmount = -PaymentAmount, 
         IsOffset = T) %>%
  distinct() %>%
  inner_join(select(negPayments, Lease_Number, PaymentAmount, PaymentDate) %>% 
             distinct)

negPayments <-
  negPayments %>%
  left_join(select(offset, NegID, IsOffset)) %>%
  select(-NegID)


# flag offset values
glo_rentals <- 
  all %>% 
  left_join(negPayments) %>%
  mutate(NegPayment = if_else(PaymentAmount < 0, TRUE, FALSE), 
         IsOffset = if_else(!is.na(IsOffset), TRUE, FALSE))

save(glo_rentals, file = file.path(gen, "glo_rentals.Rda"))

glo_rentals %>%
  map(~sum(is.na(.))) %>%
  unlist %>%
  print()

