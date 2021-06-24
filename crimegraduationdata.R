crimegraduationfile <- read_rds("crimegraduationfile.rds")
fitgraduation <- read_rds("fitgraduation.rds")

graduation_order <- crimegraduationfile %>%
  select(state, graduation_id, graduation) %>% 
  group_by(state) %>% 
  summarize(mean_graduation = mean(graduation), 
            mean_graduation_id = mean(graduation_id)) %>% 
  arrange(mean_graduation_id)


fit_graduation_plot <- fitgraduation %>% 
  as_tibble() %>% 
  ggplot(aes(x = graduation_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter graduation_id", 
       x = "Value for Parameter graduation_id", 
       y = "Probability")
