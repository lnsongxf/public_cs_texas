# ===========================================================================
#  BASIC TEXAS SETUP
# ===========================================================================
root <- getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}

library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
library(stringi)
library(tidyr)
library(readxl)
library(sf)
library(fuzzyjoin)
library(fst)

source(file.path(root, "code/paths.R"))
source(file.path(root, "code/functions/utils.R"))

# ==============================================================================
# load in all of the "raw" data
# ==============================================================================
# Read in raw GLO data
active <- 
  file.path(raw_lease, "Active/ActiveLeases.shp") %>%
  st_read(stringsAsFactors = F)

inactive <- 
  file.path(raw_lease, "Inactive/InActive_OilGas_Leases.shp") %>%
  st_read(stringsAsFactors = F)

leases_state <-
  rbind(active, inactive) %>%
  st_set_geometry(NULL) %>%
  as_tibble() %>%
  select(Lessee = ORIGINAL_L) %>%
  unique()

# loads glo bids
load(file.path(gen, "glo_bids.Rda"))

# loads prod
# note obnoxious string encoding warnings that we'll just have to live with
load(file.path(raw_prod, "index_key.Rda"))
tx_inds <-
  key %>%
  filter(state == "TX")

prod <-
  file.path(raw_prod, "pden_desc_arranged.fst") %>%
  read_fst(from = tx_inds$desc_begin,
           to = tx_inds$desc_end,
           as.data.table = T,
           columns = c("api_no", "reported_oper_name")) %>%
  mutate(oper_name = fixstrings(reported_oper_name)) 

# loads di_lease
load(file.path(raw_lease, "DI", "di_landtrac.Rda"))
di_lease <-
  di_landtrac %>%
  as.data.frame %>%
  select(-geometry) %>%
  as_tibble %>%
  mutate(Grantee = fixstrings(as.character(grantee)),
         AlsGrantee = fixstrings(as.character(grnte_al))) %>%
  filter(!str_detect(Grantee, 'NAME UNKNOWN')) %>%
  filter(!str_detect(Grantee, "\\(N/A\\)")) 

# Loads wells
load(file.path(raw_wells, "wells.Rda"))
wells <-
  wells %>%
  as.data.frame %>%
  select(-geometry) %>%
  as_tibble

# load assignments
load(file.path(int, "assignments.Rda"))

# load permits
load(file.path(ddir, "raw_data/permits/permits_flatfile.Rda"))

# company aliases we'll fill in later
# note obnoxious string encoding warnings that we'll just have to live with
company_list <-
  file.path(int, "company_list.xlsx") %>%
  read_excel() %>%
  rename(newAlias = Alias, RegexCompany = Company) %>%
  mutate(newAlias = fixstrings(newAlias),
         RegexCompany = fixstrings(RegexCompany)) %>%
  mutate(newAlias = str_replace(newAlias, "\032", " "))

# ==============================================================================
# Make dictionary out of di lease and di prod
# ==============================================================================
di_dict <-
  select(di_lease, company = Grantee, alias = AlsGrantee) %>%
  group_by(company, alias) %>%
  summarize(n = n()) %>%
  ungroup

# ==============================================================================
# Select just names and data sources
# ==============================================================================
glo_bids <-
  glo_bids %>%
  filter(Date >= cutoff_date, !str_detect(Bidder, '0')) %>%
  select(company = Bidder) %>%
  mutate(data_location = "bids") %>%
  group_by(company, data_location) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

wells <-
  wells %>%
  filter(SpudDate >= cutoff_date) %>%
  select(company = Company) %>%
  filter(!str_detect(company, 'NAME UNKNOWN')) %>%
  mutate(data_location = "DI_Wells") %>%
  group_by(company, data_location) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

prod <-
  prod %>%
  select(company = oper_name) %>%
  mutate(data_location = "DI_Prod") %>%
  group_by(company, data_location) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

leases_state <-
  leases_state %>%
  select(company = Lessee) %>%
  filter(!is.na(company)) %>%
  mutate(data_location = "TX_Leases") %>%
  group_by(company, data_location) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

di_lease <-
  di_lease %>%
  select(company = Grantee) %>%
  mutate(company = as.character(company)) %>%
  group_by(company) %>%
  summarize(n = n()) %>%
  mutate(data_location = "DI_Leases") %>%
  ungroup %>%
  arrange(desc(n))

assignments <-
  assignments %>%
  select(company = Assignee) %>%
  group_by(company) %>%
  summarise(n = n()) %>%
  mutate(data_location = "Assignments") %>% 
  ungroup %>%
  arrange(desc(n))

permits <- 
  permits_flatfile %>%
  select(company = oper_name) %>%
  group_by(company) %>%
  summarise(n = n()) %>%
  mutate(data_location = "Permits") %>%
  ungroup %>%
  arrange(desc(n))

# ==============================================================================
# clean DI dictionary
# ==============================================================================
di_dict <-
  di_dict %>%
  mutate(tcompany = str_trim(company)) %>%
  mutate(
    tcompany = srat(tcompany, regex("\\*", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("\\,", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("\\.", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("L[ \\.\\,]*?L[ \\.\\,]*?C[ \\.\\,]*?",
                                    ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("L[ \\.\\,]*?L[ \\.\\,]*?P[ \\.\\,]*?",
                                    ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" U[ \\.\\,]*?S[ \\.\\,]*?A[ \\.\\,]*?$",
                                    ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("Partnership$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("Limited$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("LP$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("LTD$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" CO$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" INC$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("ET AL", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" INCORPORATED$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" CORP$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" CORPORATION$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" TCOMPANY$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" RESOURCES.*", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" NATURAL RES.*", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" ENERGY TCOMPANY.*", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" PERMIAN", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" U[\\s]?S[\\s]?A", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" {2,}" , ignore_case = TRUE), " "),
    tcompany = str_replace_all(tcompany, regex("&" ,
                                               ignore_case = TRUE), "AND"),
    tcompany = str_replace_all(tcompany,"EXPL ","EXPLORATION "),
    tcompany = str_replace_all(tcompany,"PROD$","PRODUCTION")) %>%
  mutate(tcompany = str_trim(tcompany)) %>%
  mutate(tcompany = str_to_upper(tcompany)) %>% 
  mutate(talias = str_trim(alias)) %>%
  mutate(
    talias = srat(talias, regex("\\*", ignore_case = TRUE)),
    talias = srat(talias, regex("\\,", ignore_case = TRUE)),
    talias = srat(talias, regex("\\.", ignore_case = TRUE)),
    talias = srat(talias, regex("L[ \\.\\,]*?L[ \\.\\,]*?C[ \\.\\,]*?",
                                ignore_case = TRUE)),
    talias = srat(talias, regex("L[ \\.\\,]*?L[ \\.\\,]*?P[ \\.\\,]*?",
                                ignore_case = TRUE)),
    talias = srat(talias, regex(" U[ \\.\\,]*?S[ \\.\\,]*?A[ \\.\\,]*?$",
                                ignore_case = TRUE)),
    talias = srat(talias, regex("ET AL", ignore_case = T)),
    talias = srat(talias, regex("Partnership$", ignore_case = TRUE)),
    talias = srat(talias, regex("Limited$", ignore_case = TRUE)),
    talias = srat(talias, regex("LP$", ignore_case = TRUE)),
    talias = srat(talias, regex(" OPERATING$", ignore_case = TRUE)),
    talias = srat(talias, regex(" ENERGY$", ignore_case = TRUE)),
    talias = srat(talias, regex(" ENG$", ignore_case = TRUE)),
    talias = srat(talias, regex(" ENGINEERING$", ignore_case = TRUE)),
    talias = srat(talias, regex(" O&G$", ignore_case = TRUE)),
    talias = srat(talias, regex(" EXPLORATION LTD", ignore_case = TRUE)),
    talias = str_replace_all(talias, "PIONEER[\\s].*", 
                             "PIONEER NATURAL RESOURCES"),
    talias = str_replace_all(talias, "EOG[\\s].*", "EOG RESOURCES"),
    talias = str_replace_all(talias, "ENERVEST[\\s].*", "ENERVEST"),
    talias = str_replace_all(talias, "SHERIDAN[\\s].*", "SHERIDAN"),
    talias = str_replace_all(talias, "HILCORP[\\s].*", "HILCORP"),
    talias = str_replace_all(talias, "MGMT", ""),
    talias = str_replace_all(talias, "PROD$", ""),
    talias = str_replace_all(talias, "SAMSON[\\s].*", "SAMSON"),
    talias = str_replace_all(talias, "[\\s]PETR$", ""),
    talias = str_replace_all(talias,"RESOURCES.*",""),
    talias = str_replace_all(talias,"EXXON MOBIL","EXXON"),
    talias = str_replace_all(talias,"OPERATING.*",""),
    talias = str_replace_all(talias,"[\\s]OIL.*",""),
    talias = str_replace_all(talias,"[\\s]EXPL.*",""),
    talias = str_replace_all(talias,"[\\s]EQUITY",""),
    talias = str_replace_all(talias,"[\\s]ACQUISITION.*",""),
    talias = str_replace_all(talias,"[\\s]OPERATIONS.*",""),
    talias = str_replace_all(talias,"[\\s]E&P.*",""),
    talias = str_replace_all(talias,"[\\s]TX$",""),
    talias = str_replace_all(talias, "^J$", "JRJ"),
    talias = str_replace_all(talias, ".*H ARMSTRONG", "WILLIAM H ARMSTRONG"),
    talias = str_replace_all(talias,".*F ARMSTRONG", "STEVE ARMSTRONG"),
    talias = str_replace_all(talias,"KCS.*", "KCS"),
    talias = str_replace_all(talias, "AMERICAN RES.*",
                             "AMERICAN RESOURCE DEVELOPMENT UPSTREAM"),
    talias = str_replace_all(talias, "^MESA.*","MESA"),
    talias = str_replace_all(talias, "^MAGNOLIA.*","MAGNOLIA"),
    talias = str_replace_all(talias, "MAXIM[\\s].*","MAXIM"),
    talias = str_replace_all(talias, "MAXIMA[\\s].*","MAXIMA"),
    talias = str_replace_all(talias, "CORDILLERA.*","CORDILLERA"),
    talias = str_replace_all(talias, ".*SUPERIOR.*","SUPERIOR"),
    talias = str_replace_all(tcompany, "^STEWARD[\\s].*","STEWARD ENERGY"),
    talias = str_replace_all(tcompany, "^STEWART[\\s].*","STEWART ENERGY"),
    talias = str_replace_all(tcompany, "NATURAL RESOURCES CORP OF TEXAS",
                             "NATURAL RESOURCES CORP OF TEXAS"),
    talias = str_replace_all(talias, "ROCK", "ROCK ENERGY"),
    talias = str_replace_all(talias, "ENRE CORP", "ENRE"),
    talias = str_replace_all(tcompany, "LONGBRANCH.*","LONGBRANCH"),
    talias = str_replace_all(tcompany, "POG$","POG"),
    talias = str_replace_all(tcompany,"TRIPLE J INVESTMENTS.*",
                            "TRI-POWER"),
    talias = str_replace_all(talias,"PALOMA.*","PALOMA"),
    talias = str_replace_all(talias,"W&T.*","W&T OFFSHORE"),
    talias = str_replace_all(tcompany,"CHEVRON.*","CHEVRON"),
    talias = str_replace_all(tcompany,"^ATLAS.*","ATLAS")) %>%
  mutate(talias = str_trim(talias)) %>%
  mutate(talias = str_to_upper(talias)) 

  
combined_company_data <-
  bind_rows(glo_bids, wells, prod, leases_state, di_lease, assignments, 
            permits) %>%
  mutate(tcompany = str_trim(company)) %>%
  mutate(tcompany = str_to_upper(tcompany))%>%
  mutate(
    tcompany = srat(tcompany, regex("\\*", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("\\,", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("\\.", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("L[ \\.\\,]*?L[ \\.\\,]*?C[ \\.\\,]*?",
                                    ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("L[ \\.\\,]*?L[ \\.\\,]*?P[ \\.\\,]*?",
                                    ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" U[ \\.\\,]*?S[ \\.\\,]*?A[ \\.\\,]*?$",
                                    ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("Partnership$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("Limited$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("LP$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex("LTD$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" CO$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" INC$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" INCORPORATED$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" CORP$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" CORPORATION$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" TCOMPANY$", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" RESOURCES.*", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" NATURAL RES.*", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" ENERGY TCOMPANY.*", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" PERMIAN", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" U[\\s]?S[\\s]?A", ignore_case = TRUE)),
    tcompany = srat(tcompany, regex(" {2,}" , ignore_case = TRUE), " "),
    tcompany = str_replace_all(tcompany,"&","AND"),
    tcompany = str_replace_all(tcompany,"EXPL ","EXPLORATION "),
    tcompany = str_replace_all(tcompany,"PROD$","PRODUCTION"),
    tcompany = str_replace_all(tcompany,
                               "ATLAS PL MID-CONT. WESTTEX, LLC","ATLAS")) 

di_dict_slim <-
  di_dict %>%
  ungroup() %>%
  select(tcompany, talias) %>%
  group_by(tcompany) %>%
  filter(row_number() == 1) %>%
  unique()

company_names <-
  combined_company_data %>%
  left_join(di_dict_slim, by = c("tcompany")) %>%
  mutate(
    tcompany = str_replace_all(tcompany,"KINDER MORGAN.*", "KINDER MORGAN"),
    tcompany = str_replace_all(tcompany,"FOUR C.*", "FOUR C OIL AND GAS"),
    tcompany = str_replace_all(tcompany,"SN .*", "SANCHEZ OIL AND GAS"),
    tcompany = str_replace_all(tcompany,"LEWIS[\\s]P.*", "LEWIS OPERATING"),
    tcompany = str_replace_all(tcompany,"TRINITY[\\s]R.*",
                               "TRINITY ENERGY SERVICES"),
    tcompany = str_replace_all(tcompany,"SAMSON.*", "SAMSON"),
    talias = str_replace_all(tcompany, ".*SHELL[\\s].*","SHELL"),
    talias = str_replace_all(tcompany, ".*D SHELL.*","SHELL")) %>%
  regex_left_join(company_list,
                  by = c(tcompany = "RegexCompany"),
                  ignore_case = T) %>%
  mutate(FinalAlias = if_else(is.na(newAlias), talias, newAlias)) %>%
  mutate(FinalAlias = if_else(is.na(FinalAlias), tcompany, FinalAlias)) %>%
  select(company, data_location, alias = FinalAlias, n) %>%
  filter(!(company == "CHEVRON MIDCONTINENT, L.P." & alias =="MIDCON")) %>%
  filter(!(company == "ATLAS PL MID-CONT. WESTTEX, LLC" & alias =="MIDCON")) %>%
  mutate(company = str_to_upper(company)) %>%
  unique() 

# get rid of duplicates
company_names <-
  company_names %>% 
  arrange(company, n) %>%
  group_by(company) %>%
  filter(row_number() == 1) %>%
  ungroup()
  
# diagnostics
check_di_prod <-
  company_names %>%
  filter(data_location == "DI_Prod", is.na(alias)) %>%
  arrange(desc(n)) %>%
  select(company, n)

check_di_wells <-
  company_names %>%
  filter(data_location == "DI_Wells", is.na(alias)) %>%
  arrange(desc(n)) %>%
  select(company, n)

all_aliases <-
  company_names %>%
  filter(!is.na(alias)) %>%
  group_by(alias) %>%
  summarize(n = sum(n)) %>%
  arrange(desc(n))

# delete entry that looks like NA
company_names <-
  company_names %>%
  filter(alias != "NA", alias != "N/A", alias != "", 
         alias != "?588985 - NAME UNKNOWN") %>%
  mutate_if(is.character, toupper)

# get this ready for publication
company_names <-
  company_names %>% 
  select(-data_location, -n)

save(company_names, 
  file = file.path(int, "company_names.Rda"))
