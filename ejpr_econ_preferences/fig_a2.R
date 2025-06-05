# Plot of UK survey dates ####
datmarginals_uk_gg <- datmarginals_uk %>%
  mutate(date_bins = case_when(
    date2 < as.Date("2021-03-08") ~ "Before 8 March",
    date2 >= as.Date("2021-03-08") & date2 < as.Date("2021-03-29") ~ "8-29 March",
    date2 >= as.Date("2021-03-29") & date2 <= as.Date("2021-04-11") ~ "29 March-12 April",
    TRUE ~ "After 11 April")) %>% 
  group_by(date_bins) %>% 
  summarise(n = n()) %>% 
  mutate(date_bins = factor(date_bins, levels = c("Before 8 March", "8-29 March", "29 March-12 April", "After 11 April")))

uk<-ggplot(datmarginals_uk_gg)+
  geom_col(aes(y=n,x=date_bins,fill=date_bins))+
  theme_light() +
  theme(legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x="Survey date", y = "Number of observations")+
  lims(y=c(0,22500))

# Plot of AUS
datmarginals_au_gg <- datmarginals_au %>%
  mutate(
    victoria100_vs_everyoneelse = case_when(
      g_state == "Victoria" & postcode_lock_days >= 100 ~ "Victoria, 100+ days of lockdown",
      (g_state == "Victoria" & postcode_lock_days < 100) | g_state == "Queensland" | g_state == "New South Wales" | 
        g_state == "South Australia" | g_state == "Tasmania" | g_state == "Western Australia"  ~ "Everyone Else",
      TRUE ~ "Everyone Else"
    )
  ) %>% 
  group_by(victoria100_vs_everyoneelse) %>% 
  summarise(n=n())

aus<-ggplot(datmarginals_au_gg)+
  geom_col(aes(y=n,x=victoria100_vs_everyoneelse,fill=victoria100_vs_everyoneelse))+
  theme_light() +
  theme(legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x="Lockdown and state", y = "Number of observations")+
  lims(y=c(0,22500))

pc<-egg::ggarrange(aus,
                   uk + theme(axis.title.y= element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank()), nrow=1)

ggsave(pc, height = 9, width = 12, filename= file.path(PLOTS_PATH, "lockdown_aus_surveydate_uk_dist.png"))

cat("figure a2 saved to", file.path(PLOTS_PATH, "lockdown_aus_surveydate_uk_dist.png"), "\n")


