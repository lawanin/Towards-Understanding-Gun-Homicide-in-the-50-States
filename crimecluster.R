crimeclusterfile <- read_rds("crimeclusterfile.rds")
fitcluster <- read_rds("fitcluster.rds")

urban_cluster_order <- crimeclusterfile %>%
  select(state, urban_cluster_id, value) %>% 
  group_by(state) %>% 
  summarize(mean_urban_cluster_id = mean(urban_cluster_id), 
            mean_urban_cluster = mean(value)) %>% 
  arrange(mean_urban_cluster_id)


fit_urban_cluster_plot <- fitcluster %>% 
  as_tibble() %>% 
  ggplot(aes(x = urban_cluster_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter urban_cluster_id", 
       x = "Value for Parameter gun_id", 
       y = "Probability")