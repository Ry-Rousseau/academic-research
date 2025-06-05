# 3 months, 20/20
d3m20 <- dat_combined %>% 
  filter(infection_last3mo_lad_top_bottom!="NA") %>% 
  dplyr::mutate(infection_last3mo_lad_top_bottom = droplevels(infection_last3mo_lad_top_bottom))

# 1 plot for mm infection rate (last 3 months, below-above median)
est <- cj(d3m20, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ infection_last3mo_lad_top_bottom,
          estimate = "mm")
plot1 <- plot(est, group = "infection_last3mo_lad_top_bottom",  legend_title = "", vline = 0.5)

plot1 <- plot1 + ggplot2::theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Infection Rate (last 3 months)") 

# 1 plot for mm death rate (Death 3 months, below-above median)
d3m20dr <- dat_combined %>% 
  filter(death_rate_last3mo_lad_top_bottom!="NA") %>% 
  dplyr::mutate(death_rate_last3mo_lad_top_bottom = droplevels(death_rate_last3mo_lad_top_bottom))
est <- cj(d3m20dr, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ death_rate_last3mo_lad_top_bottom,
          estimate = "mm")
plot2 <- plot(est, group = "death_rate_last3mo_lad_top_bottom",  legend_title = "", vline = 0.5)
plot2 <- plot2 + ggplot2::theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  ggtitle("Death Rate (last 3 months)") 

# 1 plot for mm vaccination rate (Death 3 months, below-above median)
d3m20vr <- dat_combined %>% 
  filter(vaccine_1dose_lad_last3mo_top_bottom!="NA") %>% 
  dplyr::mutate(vaccine_1dose_lad_last3mo_top_bottom = droplevels(vaccine_1dose_lad_last3mo_top_bottom))

est <- cj(d3m20vr, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ vaccine_1dose_lad_last3mo_top_bottom,
          estimate = "mm")
plot3 <- plot(est, group = "vaccine_1dose_lad_last3mo_top_bottom",  legend_title = "", vline = 0.5)
plot3 <- plot3 + ggplot2::theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Vaccination Rate (last 3 months)", ) 

plot <- egg::ggarrange(plot1, 
                       plot2 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), 
                       plot3 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ),
                       nrow = 1) 

ggplot2::ggsave(plot, height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_infections_deaths_vaccines_3m_20_20.png"))

cat("figure A11 saved as mm_infections_deaths_vaccines_3m_20_20.png\n")
                