library(tidyverse) 

crimegdpfile <- read_rds("crimegdpfile.rds")
fit1 <- read_rds("fit1.rds")

gdp_order <- crimegdpfile %>%
  select(state, gdp_id) %>% 
  group_by(state) %>% 
  summarize(mean_gdp_id = mean(gdp_id)) %>% 
  arrange(mean_gdp_id)


fit_1_plot <- fit1 %>% 
  as_tibble() %>% 
  ggplot(aes(x = gdp_id)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100) +
  labs(title = "Posterior Probability Distribution for Value of Parameter gdp_id", 
       subtitle = "Median is -.0187 with MAD_SD of .0058", 
       x = "Value for Parameter gdp_id", 
       y = "Probability")


