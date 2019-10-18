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

#==============================================================================
# read in royalty data
#==============================================================================
# From PIA on negative payments: The “negative payment” isn’t a payment at all.
# It’s actually the result of the company overpaying in the past 
# (something that happens quite often, in fact, and sometimes they only 
# catch it after a long time), so it’s an adjustment. If you look at the 
# records, you’ll see that even when it happens several times to the same 
# company, they usually end up with a positive amount.  There are exceptions, 
# such as if they overpaid, but now the lease has expired, and the last
# recorded transaction was a refund, then it shows as negative.

process_sheet <- function(filename, sheet, debug = FALSE) {
  if(debug) {
    print(paste("doing file:", filename))
    print(paste("doing sheet:", sheet))
  }
  y <-
    read_excel(filename, sheet = sheet, col_types = c("text",
                                                      "text",
                                                      "text",
                                                      "text",
                                                      "date",
                                                      "text",
                                                      "numeric")) %>%
    mutate(sheet = sheet)
  return(y)
}

process_file <- function(filename) {
  excel_sheets(filename) %>%
    map_dfr(~ process_sheet(filename, .))
}

glo_royalties <-
  list.files(raw_payments,
             pattern = "Royalty_Payments",
             full.names = TRUE) %>%
  map_dfr(process_file) %>%
  extract("sheet",
          c("Production_Type", "Period"),
          "(OIL|GAS) ([[:alnum:]]+)",
          remove = TRUE) %>%
  mutate(Year = as.numeric(str_sub(varProductionYearMonth, 1, 4)),
         Month = as.numeric(str_sub(varProductionYearMonth, 5, 6))) %>%
  mutate(PaymentDate = make_date(Year, Month, 1)) %>%
  select(Lease_Number = varLeaseNumber,
         Production_Type,
         PaymentDate,
         PaymentAmount = curPaymentAmount) %>%
  group_by(Lease_Number, PaymentDate, Production_Type) %>%
  summarize(PaymentAmount = sum(PaymentAmount)) %>%
  group_by(Lease_Number) %>%
  mutate(StartPaymentDate = min(PaymentDate),
         EndPaymentDate = max(PaymentDate)) %>%
  ungroup %>%
  mutate(Production_Type = if_else(Production_Type == "GAS", "Gas", "Oil"))

# IMPORTANT: since we believe the GLO royalty payment file contains just the
# payments made to the state of texas, we need to *double* the RAL royalty
# revenue numbers, so that we are including the unmeasured
# surface owner share.  here we also backfill oil royalty rates with their gas
# counterparts if missing, and vice versa.
load(file.path(gen, "leases_state.Rda"))
leases <-
  leases_state %>%
  as.data.frame %>%
  select(Lease_Number, Type, Effective_Date, Oil_Royalty, Gas_Royalty) %>%
  as_tibble %>%
  filter(!(is.na(Oil_Royalty) & is.na(Gas_Royalty))) %>%
  mutate(Oil_Royalty = if_else(is.na(Oil_Royalty), Gas_Royalty, Oil_Royalty),
         Gas_Royalty = if_else(is.na(Gas_Royalty), Oil_Royalty, Gas_Royalty))

glo_royalties <-
  glo_royalties %>%
  inner_join(select(leases, Lease_Number, Type)) %>%
  mutate(PaymentAmount = if_else(Type == "RAL",
                                 PaymentAmount * 2,
                                 PaymentAmount)) %>%
  ungroup %>%
  select(-Type)

#==============================================================================
# turn royalty stream into inferred production stream using royalty rate
# information and contemporaneous price information
#==============================================================================
load(file.path(gen, "prices.Rda"))
prices <-
  prices %>%
  rename(Gas_Price = gas, Oil_Price = oil) %>%
  filter(Date <= LastProductionDate)

# idea: gas revenue in month t = gas_royalty * price_gas * gas_production, so
# gas_production = gas_revenue / (gas_price * gas_royalty)
# right now dropping implied production that occurs before a lease's
# effective date

# note: the royalty data reliably goes through March 2019, so drop payments
# thereafter.  also drop leases that never have a positive payment
glo_royalties <-
  glo_royalties %>%
  filter(PaymentDate <= LastProductionDate) 

inferred_lease_revenue <-
  glo_royalties %>%
  ungroup %>%
  select(Lease_Number, PaymentDate, Production_Type, PaymentAmount) %>%
  spread(Production_Type, PaymentAmount) %>%
  replace_na(list(Gas = 0, Oil = 0)) %>%
  rename(OilRoyaltyRevenue = Oil, GasRoyaltyRevenue = Gas) %>%
  inner_join(leases, by = "Lease_Number") %>%
  inner_join(prices, by = c("PaymentDate" = "Date")) %>%
  mutate(OilLeaseRevenue = OilRoyaltyRevenue / Oil_Royalty,
         GasLeaseRevenue = GasRoyaltyRevenue / Gas_Royalty,
         OilLeaseProduction = OilLeaseRevenue / Oil_Price,
         GasLeaseProduction = GasLeaseRevenue / Gas_Price) %>%
  group_by(Lease_Number) %>%
  mutate(HasEarlyProduction = max(PaymentDate < Effective_Date)) %>%
  ungroup %>%
  filter(PaymentDate >= Effective_Date) %>%
  group_by(Lease_Number) %>%
  filter(sum(!((GasRoyaltyRevenue <= 0) & (OilRoyaltyRevenue <= 0))) > 0) %>%
  ungroup

# compute date of first non-zero payment and merge back in
first_date <-
  inferred_lease_revenue %>%
  filter(!(GasRoyaltyRevenue == 0 & OilRoyaltyRevenue == 0)) %>%
  group_by(Lease_Number) %>%
  summarize(FirstRoyaltyDate = min(PaymentDate))

inferred_lease_revenue <-
  inferred_lease_revenue %>%
  left_join(first_date)


# discount, sum things up, cap some floating zeros at 0 and save
glo_production_revenues <-
  inferred_lease_revenue %>%
  mutate(delay = round(as.numeric(PaymentDate - Effective_Date) / 30),
         discount_factor = 1 / ((1 + modiscount) ^ delay),
         DiscountedOilRoyaltyRevenue = OilRoyaltyRevenue * discount_factor,
         DiscountedGasRoyaltyRevenue = GasRoyaltyRevenue * discount_factor,
         DiscountedOilLeaseProduction = OilLeaseProduction * discount_factor,
         DiscountedGasLeaseProduction = GasLeaseProduction * discount_factor,
         DiscountedOilLeaseRevenue = OilLeaseRevenue * discount_factor,
         DiscountedGasLeaseRevenue = GasLeaseRevenue * discount_factor) %>%
  group_by(Lease_Number, HasEarlyProduction, FirstRoyaltyDate) %>%
  summarize(OilRoyaltyRevenue = sum(DiscountedOilRoyaltyRevenue),
            GasRoyaltyRevenue = sum(DiscountedGasRoyaltyRevenue),
            OilLeaseRevenue = sum(DiscountedOilLeaseRevenue),
            GasLeaseRevenue = sum(DiscountedGasLeaseRevenue),
            OilLeaseProduction = sum(DiscountedOilLeaseProduction),
            GasLeaseProduction = sum(DiscountedGasLeaseProduction)) %>%
  ungroup %>%
  mutate_at(vars(OilRoyaltyRevenue, GasRoyaltyRevenue,
                 OilLeaseRevenue, GasLeaseRevenue,
                 OilLeaseProduction, GasLeaseProduction),
            ~ pmax(., 0))

save(glo_production_revenues,
     file = file.path(gen, "glo_production_revenues.Rda"))
