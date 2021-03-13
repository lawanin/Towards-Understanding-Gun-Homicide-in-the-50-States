library(tidyverse)
library(tidycensus)
library(readxl)
racevars <- c(white = "P007003", 
              black = "P007004", 
               indian = "P007005",
               asian = "P007006", 
               pacific_islander = "P007007", 
               some_other = "P007008", 
               mixed = "P007009")

race_census <- get_decennial(geography = "state",
                        variables = racevars, 
                        year = 2010,
                        geometry = TRUE,
                        summary_var = "P007001") 

racevars_acs <- c(white = "B03002_003", 
              black = "B03002_004", 
              indian = "B03002_005",
              asian = "B03002_006",
              pacific_islander = "B03002_007", 
              some_other = "B03002_008", 
              mixed = "B03002_009", 
              hispanic = "B03002_012")

race_acs <- get_acs(geography = "state",
                    variables = racevars_acs, 
                    year = 2016,
                    geometry = TRUE,
                    summary_var = "B03002_001") 

vars_urban <- c(urbanized = "H002003", 
                urban_cluster = "H002004", 
                rural = "H002005", 
                undefined = "H002006") 

urban <- get_decennial(geography = "state",
                       variables = vars_urban, 
                       year = 2010,
                       geometry = TRUE,
                       summary_var = "H002001") 

fbi_state <- read_excel("raw_data/fbi/primary/table-3.xls", skip = 3) %>% 
  fill(State) %>% 
  filter(Area == "State Total") %>% 
  select(-c("...3", "...15", "...16"))

murder_weapon <- read_excel("raw_data/fbi/primary/table-12.xls", skip = 3)

robbery_weapon <- read_excel("raw_data/fbi/primary/table-13.xls", skip = 3)

assault_weapon <- read_excel("raw_data/fbi/primary/table-14.xls", skip = 3)

gun_ownership <- read_excel("raw_data/rand/TL-354-State-Level Estimates of Household Firearm Ownership.xlsx", 
                            sheet = 2) %>% 
  filter(Year == "2016") %>% 
  select(Year:HFR_se)

state_gdp <- read_csv("raw_data/bea/gdp_state/qgsp_all_R.csv") %>% 
  select(c("GeoName", "Description", "2016Q4")) %>% 
  filter(Description == "All industry total")

state_income <- read_csv("raw_data/bea/income_state/SA27N_1998_2016__ALL_AREAS.csv")

write_rds(file = "data", data)



