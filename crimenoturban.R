crimenoturbanfile <- read_rds("crimenoturbanfile.rds")
fitnoturban <- read_rds("fitnoturban.rds")

noturban_order <- crimenoturbanfile %>%
  select(state, noturban_id, value) %>% 
  group_by(state) %>% 
  summarize(mean_noturban_id = mean(noturban_id), 
            mean_noturban = mean(value)) %>% 
  arrange(mean_noturban_id)


fit_noturban_plot <- fitnoturban %>% 
  as_tibble() %>% 
  ggplot(aes(x = noturban_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter noturban_id", 
       x = "Value for Parameter gun_id", 
       y = "Probability")