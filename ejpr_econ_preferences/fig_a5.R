gg_1 <- dat_combined %>% 
  group_by(Q_inflation, Age) %>% 
  filter(Q_inflation!="NA") %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(Age) %>% 
  mutate(age_sum = sum(n)) %>% 
  mutate(n_perc = n / age_sum) %>%
  mutate(se = sqrt(n_perc * (1 - n_perc) / age_sum)) %>%  # Standard error
  mutate(conf_low = n_perc - 1.96 * se, conf_high = n_perc + 1.96 * se)  # Confidence interval

age_infl_dist<- ggplot(gg_1) +
  geom_col(aes(x = Age, y = n_perc, fill = Q_inflation), position = "dodge") +
  geom_errorbar(aes(x = Age, ymin = conf_low, ymax = conf_high, group = Q_inflation), 
                position = position_dodge(width = 0.9), width = 0.25) +
  theme_light() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(y = "Proportion of observations in each age group")

ggsave(age_infl_dist, width=6.7,height=6.7, filename= file.path(PLOTS_PATH, "age_inflation_dist.png"))

cat("figure a5 saved to", file.path(PLOTS_PATH, "age_inflation_dist.png"), "\n")

