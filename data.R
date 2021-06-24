library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)
library(rstanarm)
library(tidybayes)
library(gt)
library(gtsummary)
library(broom.mixed)
options(tigris_use_cache = TRUE) 

variables <- load_variables(2016, "acs1")

state_crosswalk <- read_csv("raw_data/state_crosswalk.csv") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>% 
  filter(code != "DC")

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
# POVERTY 

poverty_acs <- c(below = "B06012_002")

poverty_2016 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2016,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2016) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 


poverty_2015 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs,
                        year = 2015,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2015) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2014 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2014,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2014) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2013 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2013,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2013) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2012 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2012,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2012) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2011 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2011,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2011) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2010 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2010,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2010) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2009 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2009,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2009) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2008 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2008,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2008) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2007 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2007,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2007) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2006 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2006,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2006) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_2005 <- get_acs(geography = "state", 
                        survey = "acs1", 
                        variables = poverty_acs, 
                        year = 2005,
                        geometry = FALSE, 
                        summary_var = "B06012_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2005) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev", "GEOID")) %>%
  rename(state = code) 

poverty_all <- bind_rows(poverty_2005, poverty_2006, poverty_2007, poverty_2008, poverty_2009, poverty_2010, poverty_2011, poverty_2012, poverty_2013, poverty_2014, poverty_2015, poverty_2016)

# RACE 

racevars_acs <- c(white = "B03002_003",
              black = "B03002_004",
              indian = "B03002_005",
              asian = "B03002_006",
              pacific_islander = "B03002_007",
              some_other = "B03002_008",
              mixed = "B03002_009",
              hispanic = "B03002_012") 


race_2016 <- get_acs(geography = "state",
                     survey = "acs1",
                    variables = racevars_acs,
                    year = 2016,
                    geometry = FALSE,
                    summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2016) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2015 <- get_acs(geography = "state",
                     survey = "acs1",
                    variables = racevars_acs,
                    year = 2015,
                    geometry = FALSE,
                    summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2015) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2014 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2014,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2014) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2013 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2013,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2013) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2012 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2012,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2012) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2011 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2011,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2011) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2010 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2010,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2010) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2009 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2009,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2009) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2008 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2008,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2008) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2007 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2007,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2007) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2006 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2006,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2006) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_2005 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = racevars_acs,
                     year = 2005,
                     geometry = FALSE,
                     summary_var = "B03002_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2005) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

race_all <- bind_rows(race_2005, race_2006, race_2007, race_2008, race_2009, race_2010, race_2011, race_2012, race_2013, race_2014, race_2015, race_2016)

# SINGLE PARENT

parent_acs <- c(single_6 = "B05009_013",
                single_18 = "B05009_031") 

parent_oldacs <- c(single_6 = "B23008_008",
                   single_18 = "B23008_015") 

parent_2016 <- get_acs(geography = "state",
                     survey = "acs1",
                     variables = parent_acs,
                     year = 2016,
                     geometry = FALSE,
                     summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2016) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2015 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2015,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2015) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2014 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2014,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2014) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2013 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2013,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2013) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2012 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2012,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2012) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2011 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2011,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2011) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2010 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2010,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2010) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2009 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2009,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2009) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2008 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2008,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2008) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2007 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_acs,
                       year = 2007,
                       geometry = FALSE,
                       summary_var = "B05009_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2007) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2006 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_oldacs,
                       year = 2006,
                       geometry = FALSE,
                       summary_var = "B23008_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2006) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_2005 <- get_acs(geography = "state",
                       survey = "acs1",
                       variables = parent_oldacs,
                       year = 2005,
                       geometry = FALSE,
                       summary_var = "B23008_001") %>% 
  rename(state = NAME) %>% 
  mutate(year = 2005) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) 

parent_all <- bind_rows(parent_2005, parent_2006, parent_2007, parent_2008, parent_2009, parent_2010, parent_2011, parent_2012, parent_2013, parent_2014, parent_2015, parent_2016) %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable, 
              values_from = estimate) %>% 
  mutate(single = single_6 + single_18) 

#READING SCORES 

grade4_real <- read_csv("raw_data/grade_4.csv") %>% 
  na_if("—") %>%
  na_if("‡") %>% 
  drop_na() %>% 
  clean_names() %>% 
  filter(!(year == "2000")) %>% 
  filter(!(year == "2000¹")) %>% 
  filter(!(year == "1998")) %>% 
  mutate(year = str_replace_all(year, "¹", "")) %>% 
  mutate(year = as.numeric(year)) %>%
  rename(state = jurisdiction) %>% 
  rename(scores = average_scale_score) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>%
  select(year, state, scores) %>%

# YEAR FOURTH GRADERS ARE MOST LIKELY TO BE MURDERERS

  mutate(year = year + 14) %>%
  mutate(scores = as.numeric(scores))

grade4_minus <- read_csv("raw_data/grade_4.csv") %>% 
  na_if("—") %>%
  na_if("‡") %>% 
  drop_na() %>% 
  clean_names() %>% 
  filter(!(year == "2000")) %>% 
  filter(!(year == "2000¹")) %>% 
  filter(!(year == "1998")) %>% 
  mutate(year = str_replace_all(year, "¹", "")) %>% 
  mutate(year = as.numeric(year)) %>%
  rename(state = jurisdiction) %>% 
  rename(scores = average_scale_score) %>% 
  mutate(state = tolower(state)) %>%
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>%
  select(year, state, scores) %>% 
  
  # ADJUSTING YEAR
  
  mutate(year = year + 14) %>%
  mutate(year = year -1) %>% 
  mutate(scores = as.numeric(scores))

grade4_minus2 <- read_csv("raw_data/grade_4.csv") %>% 
  na_if("—") %>%
  na_if("‡") %>% 
  drop_na() %>% 
  clean_names() %>% 
  filter(!(year == "2000")) %>% 
  filter(!(year == "2000¹")) %>% 
  filter(!(year == "1998")) %>% 
  mutate(year = str_replace_all(year, "¹", "")) %>% 
  mutate(year = as.numeric(year)) %>%
  rename(state = jurisdiction) %>% 
  rename(scores = average_scale_score) %>% 
  mutate(state = tolower(state)) %>%
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>%
  select(year, state, scores) %>% 
  
  # ADJUSTING YEAR
  
  mutate(year = year + 14) %>%
  mutate(year = year -2) %>% 
  mutate(scores = as.numeric(scores)) %>% 
  filter(year %in% c(2004, 2010, 2014))

grade4_plus1 <- read_csv("raw_data/grade_4.csv") %>% 
  na_if("—") %>%
  na_if("‡") %>% 
  drop_na() %>% 
  clean_names() %>% 
  filter(!(year == "2000")) %>% 
  filter(!(year == "2000¹")) %>% 
  filter(!(year == "1998")) %>% 
  mutate(year = str_replace_all(year, "¹", "")) %>% 
  mutate(year = as.numeric(year)) %>%
  rename(state = jurisdiction) %>% 
  rename(scores = average_scale_score) %>% 
  mutate(state = tolower(state)) %>%
  inner_join(state_crosswalk) %>%
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>%
  select(year, state, scores) %>% 
  
  # ADJUSTING YEAR
  
  mutate(year = year + 14) %>%
  mutate(year = year + 1) %>% 
  mutate(scores = as.numeric(scores)) %>% 
  filter(year %in% c(2009, 2013))

grade4 <- bind_rows(grade4_real, grade4_minus, grade4_minus2, grade4_plus1)


#GRADUATION RATE

graduation <- read_xls("raw_data/graduation.xls", skip = 1, col_types = "text") %>% 
  rename(state = `State or jurisdiction`) %>% 
  filter(!(state == "1")) %>% 
  mutate(state = str_replace_all(state, "\\.", "")) %>% 
  mutate(state = trimws(state)) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  select(-c("...11", "...14", "...17", "...19", "...21")) %>% 
  pivot_longer(cols = -state, 
               names_to = c("year", "discard"),
               names_sep = "-", 
               values_to = "graduation") %>% 
  select(-discard) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(graduation = as.numeric(graduation)) %>% 
  
# YEAR IN WHICH GRADUATES WILL BE MOST LIKELY TO BE MURDERERS
  
  mutate(year = year + 6)


# URBAN

vars_urban <- c(urbanized = "H002003",
                urban_cluster = "H002004",
                rural = "H002005",
                undefined = "H002006")

urban <- get_decennial(geography = "state",
                       variables = vars_urban,
                       year = 2010,
                       geometry = FALSE,
                       summary_var = "H002001") %>% 
  rename(state = NAME) %>% 
  mutate(state = tolower(state)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  filter(state != "DC")
  
#FBI CRIME

crime_2016 <- read_excel("raw_data/fbi/fbi_primary/crime_2016.xls", skip = 3) %>%
  fill(State) %>%
  filter(Area == "State Total") %>%
  clean_names() %>%
  mutate(state = tolower(state)) %>%
  select(-c("x3", "x15", "x16")) %>%

  # removing footnotes, this technique is used hereafter. See "data_notes" for info on some footnoes.

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

# Florida does not provide supplementary info in a format acceptable 
# to the FBI data. Taken directly from Florida state website.

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

# No good prop in order to make missing table. 

crime_full <- left_join(crime_all, weapon_all, by = c("state", "year")) %>% 
  mutate(population = as.numeric(population)) %>% 
  mutate(prop_supplement = total_murders / murder_and_nonnegligent_manslaughter) %>% 
  mutate(good_prop = ifelse(prop_supplement > .75, TRUE, FALSE)) %>% 
  mutate(great_prop = ifelse(prop_supplement > .95, TRUE, FALSE)) %>% 
  mutate(gun_murder = murder_and_nonnegligent_manslaughter * prop_firearm) %>% 
  mutate(gun_murder_capita = gun_murder / population)

# GUN OWNERSHIP

gun_ownership <- read_excel("raw_data/rand/TL-354-State-Level Estimates of Household Firearm Ownership.xlsx",
                            sheet = 2) %>%
  select(Year:HFR_se) %>% 
  rename(state = STATE) %>% 
  mutate(state = tolower(state)) %>% 
  rename(gun = HFR) %>% 
   inner_join(state_crosswalk) %>% 
  select(-c("state", "abbrev")) %>%
  rename(state = code) %>% 
  rename(year = Year)

# Annaual GDP for all states from 1997 to 2020 in 2012 dollars"

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

abortion <- read_rds("raw_data/Data set/R/NationalAndStatePregnancy_PublicUse.rds") %>% 
  select(state, year, abortionrate2024) 

# Combine each variable with a crime graph, relic of old system but may be used in
# "test.Rmd" so left here. 

crime_abortion <- inner_join(crime_weapon, abortion, by = c("state", "year")) %>% 
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(abortionrate2024) %>% 
  group_by(year) %>% 
  mutate(abortion_id = c(1:length(abortionrate2024))) 

crime_gdp <- inner_join(crime_weapon, state_gdp, by = c("state", "year")) %>%
  mutate(gdp_capita = 1000 * gdp / population) %>%
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(gdp_capita) %>% 
  group_by(year) %>% 
  mutate(gdp_id = c(1:length(gdp))) 

crime_poverty <- inner_join(crime_weapon, poverty_all) %>% 
  mutate(poverty_perc = estimate / population) %>% 
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(poverty_perc) %>% 
  group_by(year) %>% 
  mutate(poverty_id = c(1:length(poverty_perc))) %>% 
  select(-c(variable, estimate, summary_est, summary_moe, moe))

crime_race <- inner_join(crime_weapon, race_all) %>% 
  filter(variable == "black") %>% 
  mutate(black_perc = estimate / population) %>% 
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(black_perc) %>% 
  group_by(year) %>% 
  mutate(black_id = c(1:length(black_perc))) %>% 
  select(-c(variable, estimate, summary_est, summary_moe, moe))

crime_parent <- inner_join(crime_weapon, parent_all) %>% 
  mutate(single_perc = single / population) %>% 
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(single_perc) %>% 
  group_by(year) %>% 
  mutate(single_id = c(1:length(single_perc)))  %>% 
  select(-c(summary_est, summary_moe))

crime_gun <- inner_join(crime_weapon, gun_ownership, by = c("year", "state")) %>% 
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(gun) %>% 
  group_by(year) %>% 
  mutate(gun_id = c(1:length(gun))) 

crime_graduation <- inner_join(crime_weapon, graduation, by = c("year", "state")) %>% 
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(graduation) %>% 
  group_by(year) %>% 
  mutate(graduation_id = c(1:length(graduation))) 

crime_reading4 <- inner_join(crime_weapon, grade4) %>% 
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>% 
  arrange(scores) %>% 
  group_by(year) %>% 
  mutate(reading4_id = c(1:length(scores))) 

# Urbanized and cluster are different Census measures of urbanity I have added
# both to get the urban population used in my analysis. Interestingly, 
# gun death fits a negative correlation with gun homicide.

crime_urbanized <- inner_join(crime_weapon, urban, by = "state") %>% 
  filter(variable == "urbanized") %>% 
  mutate(urbanized_perc = value / summary_value) %>%
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>%
  arrange(urbanized_perc) %>%
  group_by(year) %>%
  mutate(urbanized_id = c(1:length(value))) %>% 
  select(-c(variable, value, summary_value))

crime_cluster <- inner_join(crime_weapon, urban, by = "state") %>% 
  filter(variable == "urban_cluster") %>% 
  mutate(urban_cluster_perc = value / summary_value) %>%
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>%
  arrange(urban_cluster_perc) %>%
  group_by(year) %>%
  mutate(urban_cluster_id = c(1:length(value))) %>% 
  select(-c(variable, value, summary_value))

crime_urban <- inner_join(crime_weapon, urban, by = "state") %>% 
  filter(variable == "rural") %>% 
  mutate(urban_perc = 1 - value / summary_value) %>%
  mutate(big_crime_per_capita = 100000 * gun_murder_capita) %>%
  arrange((urban_perc)) %>%
  group_by(year) %>%
  mutate(urban_id = c(1:length(value))) %>% 
  select(-c(variable, value, summary_value))

crime_everything <- left_join(crime_urban, crime_cluster) %>% 
  left_join(crime_urbanized) %>% 
  left_join(crime_graduation) %>% 
  left_join(crime_gun) %>% 
  left_join(crime_parent) %>% 
  left_join(crime_race) %>% 
  left_join(crime_gdp) %>% 
  left_join(crime_abortion) %>% 
  left_join(crime_reading4) %>% 
  left_join(crime_poverty)

# Variables which are not used in the regression are removed. These include both of our 
# varibales with missing values.

all_noreading <- crime_everything %>% 
  select(-c(scores, reading4_id)) %>% 
  select(-c(abortionrate2024, abortion_id)) %>% 
  select(-c(graduation, graduation_id)) %>% 
  drop_na() 

all <- crime_everything %>% 
  drop_na() 

# An RDS for each is a relic of old set up with R scripts. These files have not been uploaded to github,
# but are included as potentially useful. 

write_rds(crime_full, "crimefullfile.rds")

write_rds(crime_everything, "crimeeverythingfile.rds")

write_rds(crime_abortion, "crimeabortionfile.rds")
  
write_rds(crime_gdp, "crimegdpfile.rds")

write_rds(crime_poverty, "crimepovertyfile.rds")

write_rds(crime_race, "crimeracefile.rds")

write_rds(crime_parent, "crimeparentfile.rds")

write_rds(crime_gun, "crimegunfile.rds")

write_rds(crime_reading4, "crimereading4file.rds")

write_rds(crime_graduation, "crimegraduationfile.rds")

write_rds(crime_urbanized, "crimeurbanizedfile.rds")

write_rds(crime_cluster, "crimeclusterfile.rds")

write_rds(crime_urban, "crimeurbanfile.rds")



fit_abortion <- stan_glm(data = crime_abortion, 
                    formula = big_crime_per_capita ~ abortion_id, 
                    refresh = 0, 
                    seed = 87) 

fit_gdp <- stan_glm(data = crime_gdp, 
                  formula = big_crime_per_capita ~ gdp_id, 
                  refresh = 0, 
                  seed = 87) 

fit_poverty <- stan_glm(data = crime_poverty, 
                     formula = big_crime_per_capita ~ poverty_id, 
                     refresh = 0, 
                     seed = 87) 
  
fit_race <- stan_glm(data = crime_race, 
                      formula = big_crime_per_capita ~ black_id, 
                      refresh = 0, 
                      seed = 87) 

fit_parent <- stan_glm(data = crime_parent, 
                     formula = big_crime_per_capita ~ single_id, 
                     refresh = 0, 
                     seed = 87) 

fit_gun <- stan_glm(data = crime_gun, 
                     formula = big_crime_per_capita ~ gun_id, 
                     refresh = 0, 
                     seed = 87) 

fit_reading4 <- stan_glm(data = crime_reading4, 
                    formula = big_crime_per_capita ~ reading4_id, 
                    refresh = 0, 
                    seed = 87) 

fit_graduation <- stan_glm(data = crime_graduation, 
                    formula = big_crime_per_capita ~ graduation_id, 
                    refresh = 0, 
                    seed = 87) 

fit_urbanized <- stan_glm(data = crime_urbanized, 
                    formula = big_crime_per_capita ~ urbanized_id, 
                    refresh = 0, 
                    seed = 87) 

fit_cluster <- stan_glm(data = crime_cluster, 
                    formula = big_crime_per_capita ~ urban_cluster_id, 
                    refresh = 0, 
                    seed = 87) 

fit_urban <- stan_glm(data = crime_urban, 
                        formula = big_crime_per_capita ~ urban_id, 
                        refresh = 0, 
                        seed = 87) 

fit_final <- stan_glm(data = all_noreading, 
                   formula = big_crime_per_capita ~ urban_id + gun_id  + single_id + black_id + poverty_id  + urban_id + single_id*urban_id, 
                   refresh = 0, 
                   seed = 87) 


fit_alt <- stan_glm(data = all,
                     formula = big_crime_per_capita ~ graduation_id + black_id + reading4_id + urban_id + single_id + gun_id, 
                     refresh = 0, 
                     seed = 87)


newobs <- expand_grid(gun_id = 25, 
                      poverty_id = 25, 
                      black_id = 25, 
                      single_id = c(1, 11, 21, 31, 41, 50),
                      urban_id = c(1, 11, 21, 31, 41, 50)) 

fit_model <- add_fitted_draws(newdata = newobs, 
                                     model = fit_final) 
# Rgression table for fit on Shiny App.

regression_table <- as_gt(tbl_regression(fit_final, intercept = TRUE, estimate_fun = function(x) style_sigfig(x, digits = 4)) %>% 
                            modify_header(estimate = "**Paramter**")) %>% 
  tab_header(title = md("**Influence of 
                        Gun Deaths in a State per 1OOK**"), 
             subtitle = "How Rankings of Urbanicity, Gun Ownership, 
Proportion of Children with Single Parents, 
Proportion of Blacks, and Proportion of Poor 
are correlated with likelihood of Voting") %>% 
  tab_source_note("Sources: U.S. Census, American Community Survey (2005-2016), RAND (2020)")

# Regression table for alternate fit not yet included in Shiny App

regression_table_alt <- as_gt(tbl_regression(fit_alt, intercept = TRUE, estimate_fun = function(x) style_sigfig(x, digits = 4)) %>% 
                            modify_header(estimate = "**Paramter**")) %>% 
  tab_header(title = md("**Influence of 
                        Gun Deaths in a State per 1OOK**"), 
             subtitle = "How Rankings of Urbanicity, Gun Ownership, 
Proportion of Children with Single Parents, 
Proportion of Blacks, and Proportion of Poor,
4th Grade Reading Scores and Averaged Freshman 
Graduation Rate are correlated with likelihood of Voting") %>% 
  tab_source_note("Sources: U.S. Census, American Community Survey (2005-2016), RAND (2020)")

# An RDS for each is a relic of old set up with R scripts. These files have not been uploaded to github,
# but are included as potentially useful. 

write_rds(fit_abortion, "fitabortion.rds")

write_rds(fit_gdp, "fitgdp.rds")

write_rds(fit_poverty, "fitpoverty.rds")

write_rds(fit_race, "fitrace.rds")

write_rds(fit_parent, "fitparent.rds")

write_rds(fit_gun, "fitgun.rds")

write_rds(fit_reading4, "fitreading4.rds")

write_rds(fit_graduation, "fitgraduation.rds")

write_rds(fit_urbanized, "fiturbanized.rds")

write_rds(fit_cluster, "fitcluster.rds")

write_rds(fit_urban, "fiturban.rds")

write_rds(regression_table, "regressiontable.rds")

write_rds(regression_table_alt, "regressiontablealt.rds")

write_rds(fit_model, "fitmodel.rds")







