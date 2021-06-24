fitmodel <- read_rds("fitmodel.rds")
rtable <- read_rds("regressiontable.rds")
rtablealt <- read_rds("regressiontablealt.rds")


model_posteriors <- fitmodel %>% 
  mutate(single_id = as.character(single_id)) %>% 
  mutate(urban_id = as.character(urban_id)) %>% 
  ggplot(aes(x = .value, y = urban_id, fill = single_id)) + 
  stat_slab() +
  theme_bw() +
  labs(title = "Posterior of Gun Deaths in State per Hundred Thousand by Urbanicity and Proportion of Children with Single Parents", 
       subtitle = "Increase due to urbanicity likely depends much on a state's single parenthood", 
       x = "Gun Deaths Per Hundred Thousand", 
       y = "State Ranking of Urbanicity", 
       fill = "State Ranking 
of Proportion of 
Children with 
Single Parents") +
  theme(plot.title = element_text(face="bold")) +
  scale_y_discrete(labels = c("1 (Least Urban)", "11", "21", "31", "41", "50 (Most Urban)")) +
  scale_fill_discrete(labels = c("1 (Least Single Parenthood)", "11", "21", "31", "41", "50 (Most Single Parenthood)"))

model_posteriors

fitmodel

