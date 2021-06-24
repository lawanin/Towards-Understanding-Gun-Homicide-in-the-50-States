crimeparentfile <- read_rds("crimeparentfile.rds")
fitparent <- read_rds("fitparent.rds")

single_order <- crimeparentfile %>%
  select(state, single_id, single_perc) %>% 
  group_by(state) %>% 
  summarize(mean_single_id = mean(single_id), 
            mean_single_perc = mean(single_perc)) %>% 
  arrange(mean_single_id)


fit_parent_plot <- fitparent %>% 
  as_tibble() %>% 
  ggplot(aes(x = single_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter single_id", 
       x = "Value for Parameter parent_id", 
       y = "Probability")
