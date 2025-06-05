gg_1 <- dat_combined %>% 
  group_by(Q_inflation) %>% 
  filter(Q_inflation!="NA") %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(n=n/sum(n))

inf_dist <- ggplot(gg_1) +
  geom_col(aes(x = Q_inflation, y = n, fill = Q_inflation), position = "dodge") +
  theme_light() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(y = "Proportion of observations",
       x= "Inflation expectations")

ggsave(inf_dist, width=6.7,height=6.7, filename= file.path(PLOTS_PATH, "inflation_dist.png"))

cat("figure a4 saved to", file.path(PLOTS_PATH, "inflation_dist.png"), "\n")
