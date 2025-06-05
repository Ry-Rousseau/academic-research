# 1 month, median
dat_combined<-dat_combined %>% mutate(death_rate_lastmo_lad_median = relevel(death_rate_lastmo_lad_median,ref="Below or Equal Median"))
# 1 plot for mm infection rate (last 1 month, below-above median)
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ infection_lastmo_lad_median,
          estimate = "mm")
plot1 <- plot(est, group = "infection_lastmo_lad_median",  legend_title = "", vline = 0.5)

plot1 <- plot1 + ggplot2::theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Infection Rate (last 1 month)") 

# 1 plot for mm death rate (Death 1 month, below-above median)
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ death_rate_lastmo_lad_median,
          estimate = "mm")
plot2 <- plot(est, group = "death_rate_lastmo_lad_median",  legend_title = "", vline = 0.5)
plot2 <- plot2 + ggplot2::theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  ggtitle("Death Rate (last 1 month)") 

# 1 plot for mm vaccination rate (Death 1 month, below-above median)
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ vaccine_1dose_lad_lastmo_median,
          estimate = "mm")              
plot3 <- plot(est, group = "vaccine_1dose_lad_lastmo_median",  legend_title = "", vline = 0.5)
plot3 <- plot3 + ggplot2::theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Vaccination Rate (last 1 month)", ) 

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

ggplot2::ggsave(plot, height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_infections_deaths_vaccines_1m_median_pooled.png"))

cat("figure a9 saved as mm_infections_deaths_vaccines_1m_median_pooled.png\n")