library(tidyverse) 
crimegdpfile <- read_rds("crimegdpfile.rds")
fitgdp <- read_rds("fitgdp.rds")

gdp_order <- crimegdpfile %>%
  select(state, gdp_id, gdp_capita) %>% 
  group_by(state) %>% 
  summarize(mean_gdp_id = mean(gdp_id), 
            mean_gdp_capita = mean(gdp_capita)) %>% 
  arrange(mean_gdp_id)


fit_gdp_plot <- fitgdp %>% 
  as_tibble() %>% 
  ggplot(aes(x = gdp_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter gdp_id", 
       subtitle = "Median is -.0187 with a Median SD of .0058", 
       x = "Value for Parameter gdp_id", 
       y = "Probability")


