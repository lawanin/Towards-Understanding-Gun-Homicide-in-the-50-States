library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)
library(rstanarm)
options(tigris_use_cache = TRUE)

state_crosswalk <- read_csv("raw_data/state_crosswalk.csv") %>%
  clean_names() %>%
  mutate(state = tolower(state))

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

#FBI CRIME

crime_2016 <- read_excel("raw_data/fbi/fbi_primary/crime_2016.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>%
  select(-c("x3", "x15", "x16")) %>%

  # removing footnotes

   mutate(state = str_replace(string = state, pattern = "5", replacement = "")) %>%
   mutate(state = str_replace(string = state, pattern = "6", replacement = "")) %>%

  # beautiful

   inner_join(state_crosswalk) %>%
   select(-c("state", "abbrev")) %>%
   rename(state = code) %>% 
  mutate(year = 2016)

crime_2015 <- read_excel("raw_data/fbi/fbi_primary/crime_2015.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>%
  select(-x3) %>% 
  mutate(state = str_replace(string = state, pattern = "5", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "6", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2015)

crime_2014 <- read_excel("raw_data/fbi/fbi_primary/crime_2014.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3) %>% 
  mutate(state = str_replace(string = state, pattern = "4", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "5", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "6", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = "7", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2014)

crime_2013 <- read_excel("raw_data/fbi/fbi_primary/crime_2013.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3) %>% 
  mutate(state = str_replace(string = state, pattern = "4", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "5", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "6", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = "7", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = ", ", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = ",", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2013)

crime_2012 <- read_excel("raw_data/fbi/fbi_primary/crime_2012.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3) %>%
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2012)

crime_2011 <- read_excel("raw_data/fbi/fbi_primary/crime_2011.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3) %>% 
  mutate(state = str_replace(string = state, pattern = "1", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>%
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2011)

crime_2010 <- read_excel("raw_data/fbi/fbi_primary/crime_2010.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3, -x14) %>% 
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2010)

crime_2009 <- read_excel("raw_data/fbi/fbi_primary/crime_2009.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3, -x14) %>% 
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = "4", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = ", ", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2009)

crime_2008 <- read_excel("raw_data/fbi/fbi_primary/crime_2008.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3, -x14) %>% 
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = ", ", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2008)
  
crime_2007 <- read_excel("raw_data/fbi/fbi_primary/crime_2007.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3, -x14) %>% 
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = ", ", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2007)
  
crime_2006 <- read_excel("raw_data/fbi/fbi_primary/crime_2006.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3, -x14) %>% 
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = ", ", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2006)

crime_2005 <- read_excel("raw_data/fbi/fbi_primary/crime_2006.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  select(-x3, -x14) %>% 
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = str_replace(string = state, pattern = ", ", replacement = "")) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = 2005)

crime_2004 <- read_excel("raw_data/fbi/fbi_primary/crime_2004.xls", skip = 3)  %>% 
  mutate(State = Area) %>% 
  clean_names() %>% 
  select(-x12, -x13) %>%
  mutate(state = tolower(state)) %>% 
  mutate(state = str_replace(string = state, pattern = "2", replacement = "")) %>% 
  left_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>% 
  fill(code) %>% 
  rename(state = code) %>% 
  filter(area == "State Total") %>% 
  mutate(year = 2004)

crime_all <- bind_rows(crime_2016, crime_2015, crime_2014, crime_2013, crime_2012, 
                       crime_2011, crime_2010, crime_2009, crime_2008, crime_2007, 
                       crime_2006, crime_2005, crime_2004) %>% 
  select(population, murder_and_nonnegligent_manslaughter, state, year)

#FBI WEAPON

weapon_2016 <- read_excel("raw_data/fbi/fbi_primary/weapon_2016.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2016)

weapon_2015 <- read_excel("raw_data/fbi/fbi_primary/weapon_2015.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2015)

weapon_2014 <- read_excel("raw_data/fbi/fbi_primary/weapon_2014.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2014)

weapon_2013 <- read_excel("raw_data/fbi/fbi_primary/weapon_2013.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2013)

weapon_2012 <- read_excel("raw_data/fbi/fbi_primary/weapon_2012.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2012)

weapon_2011 <- read_excel("raw_data/fbi/fbi_primary/weapon_2011.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2011)

weapon_2010 <- read_excel("raw_data/fbi/fbi_primary/weapon_2010.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2010)

weapon_2009 <- read_excel("raw_data/fbi/fbi_primary/weapon_2009.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2009)

weapon_2008 <- read_excel("raw_data/fbi/fbi_primary/weapon_2008.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2008)

weapon_2007 <- read_excel("raw_data/fbi/fbi_primary/weapon_2007.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2007)

weapon_2006 <- read_excel("raw_data/fbi/fbi_primary/weapon_2006.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2006)

weapon_2005 <- read_excel("raw_data/fbi/fbi_primary/weapon_2005.xls", skip = 3) %>% 
  clean_names() %>% 
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>% 
  mutate(state = tolower(state)) %>% 
  rename(total_murders = total_murders1) %>% 
  inner_join(state_crosswalk) %>% 
  select(total_murders, total_firearms, code) %>% 
  rename(state = code) %>% 
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>% 
  mutate(year = 2005)

weapon_2004 <- read_excel("raw_data/fbi/fbi_primary/weapon_2004.xls", skip = 4) %>% 
  clean_names() %>%
  mutate(state = str_replace(string = state, pattern = "3", replacement = "")) %>%
  mutate(state = tolower(state)) %>%
  rename(total_murders = total_murders1) %>%
  inner_join(state_crosswalk) %>%
  select(total_murders, total_firearms, code) %>%
  rename(state = code) %>%
  filter(state != "DC") %>% 
  mutate(prop_firearm = total_firearms / total_murders) %>%  
  mutate(year = 2004)

weapon_florida <- read_excel("raw_data/weapon_florida.xlsx", skip = 4) %>% 
  clean_names() %>% 
  select(year, total_offenses, firearm) %>% 
  rename(total_firearms = firearm, 
         total_murders = total_offenses) %>% 
  mutate(state = "FL") %>% 
  filter(!(year %in% c("1995", "1996", "1997", "1998", "1999", "2000", "2001",
                       "2002", "2003", "2017", "2018"))) %>% 
  drop_na(total_murders) %>% 
  mutate(total_murders = str_replace(string = total_murders, pattern = "1,108", replacement = "1108")) %>% 
  mutate(total_murders = str_replace(string = total_murders, pattern = '\\*', replacement = "")) %>% 
  mutate(year = as.numeric(year)) %>%
  mutate(total_murders = as.numeric(total_murders))  %>% 
  mutate(prop_firearm = total_firearms / total_murders)


weapon_all <- bind_rows(weapon_2016, weapon_2015, weapon_2014, weapon_2013, weapon_2012,
          weapon_2011, weapon_2010, weapon_2009, weapon_2008, weapon_2007, 
          weapon_2006, weapon_2005, weapon_2004, weapon_florida)

# Combined crime and weapons. For now, we will use any row whose supplemental
# contains at least 75% of the main crime tally to caluclate a gun death per 
# capita

crime_weapon <- left_join(crime_all, weapon_all, by = c("state", "year")) %>% 
  mutate(population = as.numeric(population)) %>% 
  mutate(prop_supplement = total_murders / murder_and_nonnegligent_manslaughter) %>% 
  mutate(good_prop = ifelse(prop_supplement > .75, TRUE, FALSE)) %>% 
  mutate(great_prop = ifelse(prop_supplement > .95, TRUE, FALSE)) %>% 
  filter(good_prop == TRUE) %>% 
  mutate(gun_murder = murder_and_nonnegligent_manslaughter * prop_firearm) %>% 
  mutate(gun_murder_capita = gun_murder / population)


murder_weapon <- read_excel("raw_data/fbi/primary/table-12.xls", skip = 3)

robbery_weapon <- read_excel("raw_data/fbi/primary/table-13.xls", skip = 3)

assault_weapon <- read_excel("raw_data/fbi/primary/table-14.xls", skip = 3)

gun_ownership <- read_excel("raw_data/rand/TL-354-State-Level Estimates of Household Firearm Ownership.xlsx",
                            sheet = 2) %>%
  select(Year:HFR_se)

# Annaual GDP for all states from 1997 to 2020 in 2012 dollars
state_gdp <- read_csv("raw_data/bea/gdp_state/gdp_1997_2020.csv", skip = 4) %>% 
  pivot_longer(cols = `1997`:`2020`, 
               names_to = "year", 
               values_to = "gdp") %>% 
  select(-GeoFips) %>% 
  filter(GeoName != "United States") %>% 
  filter(!(GeoName %in% c("New England", "Mideast", "Great Lakes", "Plains", 
                         "Southeast", "Southwest", "Rocky Mountain", "Far West"))) %>% 
  rename(state = GeoName) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  mutate(year = as.numeric(year)) 

# Annual GDP for all states from 1995 to 1997 in 1997 dollars, converted to 2012
# dollars using the shared value of 1997. Simple Conversion was not successful, 
# may ignore
old_gdp <- read_csv("raw_data/bea/gdp_state/gdp_1995_1997.csv", skip = 4) %>% 
  pivot_longer(cols = `1995`:`1997`, 
               names_to = "year",
               values_to = "gdp") %>% 
  filter(Description == "All industry total") %>% 
  filter(GeoName != "United States") %>% 
  filter(!(GeoName %in% c("New England", "Mideast", "Great Lakes", "Plains", 
                          "Southeast", "Southwest", "Rocky Mountain", "Far West"))) %>% 
  select(-GeoFips, -LineCode, -Description) %>% 
  mutate(gdp = as.numeric(gdp)) %>% 
  mutate(gdp = gdp * 144457.3/104805.2)

state_income <- read_csv("raw_data/bea/income_state/SA27N_1998_2016__ALL_AREAS.csv")

# Graph with Variable representing ratio of gdp/per capita to cime per capita.
# Essentially the same as dividing gdp by crime, but this feels better.

crime_gdp <- inner_join(crime_weapon, state_gdp, by = c("state", "year")) %>%
  mutate(gdp_capita = gdp / population) %>%
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(gdp_capita) %>% 
  group_by(year) %>% 
  mutate(gdp_id = c(1:length(gdp)))

write_rds(crime_gdp, "crimegdpfile.rds")
crimegdpfile <- read_rds("crimegdpfile.rds")


fit_1 <- stan_glm(data = crimegdpfile, 
                  formula = big_gun_per_capita ~ gdp_id, 
                  refresh = 0, 
                  seed = 87) %>% 

write_rds(fit_1, "fit1.rds")






