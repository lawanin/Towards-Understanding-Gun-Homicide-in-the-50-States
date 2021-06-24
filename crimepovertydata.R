crimepovertyfile <- read_rds("crimepovertyfile.rds")

black_order <- crimepovertyfile %>%
  select(state, poverty_id, poverty_perc) %>% 
  group_by(state) %>% 
  summarize(mean_poverty_id = mean(poverty_id), 
            mean_poverty_perc = mean(poverty_perc)) %>% 
  arrange(mean_poverty_id)



