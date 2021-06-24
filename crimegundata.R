crimegunfile <- read_rds("crimegunfile.rds")

gun_order <- crimegunfile %>%
  select(state, gun_id, gun) %>% 
  group_by(state) %>% 
  summarize(mean_gun_id = mean(gun_id), 
            mean_gun = mean(gun)) %>% 
  arrange(mean_gun_id)
