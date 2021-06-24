crimeracefile <- read_rds("crimeracefile.rds")

black_order <- crimeracefile %>%
  select(state, black_id, black_perc) %>% 
  group_by(state) %>% 
  summarize(mean_black_id = mean(black_id), 
            mean_black_perc = mean(black_perc)) %>% 
  arrange(mean_black_id)