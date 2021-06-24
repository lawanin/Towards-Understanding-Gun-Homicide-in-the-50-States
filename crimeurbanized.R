crimeurbanizedfile <- read_rds("crimeurbanizedfile.rds")
fiturbanized <- read_rds("fiturbanized.rds")

urbanized_order <- crimeurbanizedfile %>%
  select(state, urbanized_id, value) %>% 
  group_by(state) %>% 
  summarize(mean_urbanized_id = mean(urbanized_id), 
            mean_urbanized = mean(value)) %>% 
  arrange(mean_urbanized_id)


fit_urbanized_plot <- fiturbanized %>% 
  as_tibble() %>% 
  ggplot(aes(x = urbanized_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter urbanized_id", 
       x = "Value for Parameter gun_id", 
       y = "Probability")