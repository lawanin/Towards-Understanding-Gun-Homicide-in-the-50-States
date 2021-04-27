crimeeverythingfile <- read_rds("crimeeverythingfile.rds") 

gun_order <- crimeeverythingfile %>%
  drop_na() %>% 
  select(state, gun_id, gun) %>% 
  group_by(state) %>% 
  summarize(mean_gun_id = mean(gun_id), 
            mean_gun_ownership_rate = round(mean(gun), digits = 3)) %>% 
  arrange(mean_gun_id)

black_order <- crimeeverythingfile %>%
  select(state, black_id, black_perc) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarize(mean_black_id = mean(black_id), 
            mean_black_perc = round(mean(black_perc), digits = 3)) %>% 
  arrange(mean_black_id)

urban_order <- crimeeverythingfile %>%
  select(state, urban_id, urban_perc) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarize(mean_urban_id = mean(urban_id), 
            mean_urban_perc = round(mean(urban_perc), digits = 3)) %>% 
  arrange(mean_urban_id)

single_order <- crimeeverythingfile %>%
  select(state, single_id, single_perc) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarize(mean_single_id = mean(single_id), 
            mean_single_perc = round(mean(single_perc), digits = 3)) %>% 
  arrange(mean_single_id)

poverty_order <- crimeeverythingfile %>%
  select(state, poverty_id, poverty_perc) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarize(mean_poverty_id = mean(poverty_id), 
            mean_poverty_perc = round(mean(poverty_perc), digits = 3)) %>% 
  arrange(mean_poverty_id)

reading4_order <- crimeeverythingfile %>%
  select(state, reading4_id, scores) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarize(mean_reading4_id = mean(reading4_id),
    mean_reading4_score = round(mean(scores), digits = 3)) %>% 
  arrange(mean_reading4_id)

graduation_order <- crimeeverythingfile %>%
  select(state, graduation_id, graduation) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarize(mean_graduation_id = mean(graduation_id), 
            mean_graduation_rate = round(mean(graduation), digits = 3)) %>% 
  arrange(mean_graduation_id)

gdp_order <- crimeeverythingfile %>%
  select(state, gdp_id, gdp_capita) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarize(mean_gdp_id = mean(gdp_id), 
            mean_gdp_capita = round(mean(gdp_capita), digits = 3)) %>% 
  arrange(mean_gdp_id)

abortion_order <- crimeeverythingfile %>%
  select(state, abortion_id, abortionrate2024) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarize(mean_abortion_id = mean(abortion_id), 
            mean_abortion_rate = round(mean(abortionrate2024), digits = 3)) %>% 
  arrange(mean_abortion_id)
