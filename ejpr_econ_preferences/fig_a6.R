gg_1 <- dat_combined %>% 
  group_by(Partisanship, inequality_three_b) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(Partisanship) %>% 
  mutate(par_sum = sum(n)) %>% 
  mutate(n_perc = n / par_sum) %>%
  mutate(se = sqrt(n_perc * (1 - n_perc) / par_sum)) %>%  # Standard error
  mutate(conf_low = n_perc - 1.96 * se, conf_high = n_perc + 1.96 * se)  # Confidence interval

ineq_shape <- ggplot(gg_1) +
  geom_col(aes(x = Partisanship, y = n_perc, fill = inequality_three_b), position = "dodge") +
  geom_errorbar(aes(x = Partisanship, ymin = conf_low, ymax = conf_high, group = inequality_three_b), 
                position = position_dodge(width = 0.9), width = 0.25) +
  theme_light() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(y = "Proportion of observations by partisanship")

ggsave(ineq_shape, width =6.7, height=6.7, filename = file.path(PLOTS_PATH, "inequality_shape_partisanship_dist_perc.png"))

cat("figure a6 saved to", file.path(PLOTS_PATH, "inequality_shape_partisanship_dist_perc.png"), "\n")
