crimereading4file <- read_rds("crimereading4file.rds")
fitreading4 <- read_rds("fitreading4.rds")

reading4_order <- crimereading4file %>%
  select(state, reading4_id, scoress) %>% 
  group_by(state) %>% 
  summarize(mean_reading4 = mean(scores), 
            mean_reading4_id = mean(reading4_id)) %>% 
  arrange(mean_reading4_id)


fit_reading4_plot <- fitreading4 %>% 
  as_tibble() %>% 
  ggplot(aes(x = reading4_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter reading4_id", 
       x = "Value for Parameter reading4_id", 
       y = "Probability")
