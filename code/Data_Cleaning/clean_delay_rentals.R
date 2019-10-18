# reading in auction delay rental data, entered by Thom Covert and Devin McNulty
# on 9/9/2019

#load packages we need 
library(tidyverse)
library(lubridate)
library(readxl)

root = getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))

# discount rate for delay rentals and extensions payments
discount_rate <- 1.1


# reformat in a useful way
auction_delay_rentals <-
  file.path(raw, "auction_delay_rentals.xlsx") %>%
  read_excel %>%
  mutate(Date = as_date(date)) %>%
  select(-date, -type) %>%
  pmap_dfr(function(Date,
                    mgl_start, mgl_end,
                    delay2, delay3, delay4, delay5) {
    tibble(Date = Date,
           mgl = seq(mgl_start, mgl_end),
           delay2 = delay2,
           delay3 = delay3,
           delay4 = delay4,
           delay5 = delay5)
  }) %>%
  mutate(Tract = as.character(mgl)) %>%
  select(-mgl)

# bring in bid data to merge in with lease numbers
load(file.path(gen, "glo_bids.Rda"))

auction_lease_delay_rentals <-
  glo_bids %>%
  filter(Winner, Lease_Number != "MF") %>%
  select(Date, Lease_Number, Tract) %>%
  inner_join(auction_delay_rentals) %>%
  select(Lease_Number, delay2, delay3, delay4, delay5) %>%
  gather(Rental_Year, Rental, -Lease_Number) %>%
  extract(Rental_Year,
          "Rental_Year",
          regex = "(\\d)",
          remove = TRUE,
          convert = TRUE) %>%
  replace_na(list(Rental = 0)) %>%
  mutate(Rentals_Discounted = Rental / (discount_rate ^ (Rental_Year - 1))) %>%
  select(Lease_Number, Rental_Year, Rentals_Discounted) %>%
  mutate(Rental_Year = paste0("Rental_Year", Rental_Year)) %>%
  spread(Rental_Year, Rentals_Discounted)

# then bring in the summer of 2017 pre-cleaned RAL delay rental data and merge
# it with the stragglers that TC found in September 2019
load(file.path(gen, "recommended_rentals.Rda"))
extra_and_modified_ral_delay_rentals <-
  file.path(raw, "extra_ral_delay_rentals.xlsx") %>%
  read_excel

extra_ral_delay_rentals <-
  extra_and_modified_ral_delay_rentals %>%
  filter(newfix == "new") %>%
  select(-newfix) %>%
  gather(Rental_Year, Rental, -Lease_Number) %>%
  extract(Rental_Year,
          "Rental_Year",
          regex = "(\\d)",
          remove = TRUE,
          convert = TRUE) %>%
  replace_na(list(Rental = 0)) %>%
  mutate(Rentals_Discounted = Rental / (discount_rate ^ (Rental_Year - 1))) %>%
  select(Lease_Number, Rental_Year, Rentals_Discounted) %>%
  mutate(Rental_Year = paste0("Rental_Year", Rental_Year)) %>%
  spread(Rental_Year, Rentals_Discounted)

modified_ral_delay_rentals <-
  extra_and_modified_ral_delay_rentals %>%
  filter(newfix == "fix") %>%
  select(-newfix) %>%
  gather(Rental_Year, Rental, -Lease_Number) %>%
  extract(Rental_Year,
          "Rental_Year",
          regex = "(\\d)",
          remove = TRUE,
          convert = TRUE) %>%
  replace_na(list(Rental = 0)) %>%
  mutate(Rentals_Discounted = Rental / (discount_rate ^ (Rental_Year - 1))) %>%
  select(Lease_Number, Rental_Year, Rentals_Discounted) %>%
  mutate(Rental_Year = paste0("Rental_Year", Rental_Year)) %>%
  spread(Rental_Year, Rentals_Discounted)

ral_lease_delay_rentals <-
  recommended_rentals %>%
  mutate(Lease_Number = paste0("MF", Lease_Number),
         Rental_Year = paste0("Rental_Year", Rental_Year)) %>%
  select(Lease_Number, Rentals_Discounted, Rental_Year) %>%
  spread(Rental_Year, Rentals_Discounted) %>%
  anti_join(modified_ral_delay_rentals, by = "Lease_Number") %>%
  bind_rows(extra_ral_delay_rentals) %>%
  bind_rows(modified_ral_delay_rentals)

delay_rentals <-
  bind_rows(ral_lease_delay_rentals, auction_lease_delay_rentals)

save(delay_rentals, file = file.path(gen, "delay_rentals.Rda"))

