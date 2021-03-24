library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)

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

fbi_state <- read_excel("raw_data/fbi/primary/table-3.xls", skip = 3) %>% 
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
   mutate(violent_crime1 = as.numeric(violent_crime1)) %>% 
   mutate(population = as.numeric(population)) %>% 
   mutate(crime_per_capita = violent_crime1 / population)
  
murder_weapon <- read_excel("raw_data/fbi/primary/table-12.xls", skip = 3)

robbery_weapon <- read_excel("raw_data/fbi/primary/table-13.xls", skip = 3)

assault_weapon <- read_excel("raw_data/fbi/primary/table-14.xls", skip = 3)

gun_ownership <- read_excel("raw_data/rand/TL-354-State-Level Estimates of Household Firearm Ownership.xlsx", 
                            sheet = 2) %>% 
  filter(Year == "2016") %>% 
  select(Year:HFR_se)

state_gdp <- read_csv("raw_data/bea/gdp_state/qgsp_all_R.csv") %>% 
  janitor::clean_names() %>% 
  select(c("geo_name", "description", "x2016q4")) %>% 
  filter(description == "All industry total") %>% 
  mutate(x2016q4 = as.numeric(x2016q4)) %>% 
  arrange(desc(x2016q4)) %>% 
  mutate(state = tolower(geo_name)) %>% 
  inner_join(state_crosswalk) %>% 
  select(-c("abbrev", "state")) %>% 
  rename(state = code)

state_income <- read_csv("raw_data/bea/income_state/SA27N_1998_2016__ALL_AREAS.csv")

# Graph with Variable representing ratio of gdp/per capita to cime per capita. 
# Essentially the same as dividing gdp by crime, but this feels better. 

crime_gdp <- inner_join(fbi_state, state_gdp, by = "state") %>% 
  mutate(gdp_capita = x2016q4 / population) %>% 
  arrange(gdp_capita) %>% 
  mutate(gdp_id = c(1:50)) 

crime_gdp_graph <- crime_gdp %>%
  ggplot(aes(x = gdp_id, y = crime_per_capita, fill = state)) +
  geom_col() + 
  labs(title = "Violent crime by states in Order of asecending GDP per capita", 
       subtitle = "More Poor States seem to have higher violence", 
       x = "States in order of ascending GDP per capita", 
       y = "Crime per capita") +
  geom_smooth()

ggsave("crime_gdp.png", crime_gdp_graph)
