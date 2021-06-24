crimeabortionfile <- read_rds("crimeabortionfile.rds")
fitabortion <- read_rds("fitabortion.rds")

abortion_order <- crimeabortionfile %>%
  select(state, abortion_id, abortionrate1819) %>% 
  group_by(state) %>% 
  summarize(mean_abortion_id = mean(abortion_id), 
            mean_abortion_rate = mean(abortionrate1819)) %>% 
  arrange(mean_abortion_id)


fit_abortion_plot <- fitabortion %>% 
  as_tibble() %>% 
  ggplot(aes(x = abortion_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter single_id", 
       x = "Value for Parameter parent_id", 
       y = "Probability")
