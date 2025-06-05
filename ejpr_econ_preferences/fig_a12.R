# 6 months, median
# 1 plot for mm infection rate (last 6 months, below-above median)
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ infection_last6mo_lad_median,
          estimate = "mm")
plot1 <- plot(est, group = "infection_last6mo_lad_median",  legend_title = "", vline = 0.5)

plot1 <- plot1 + ggplot2::theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Infection Rate (last 6 months)") 

# 1 plot for mm death rate (Death 6 months, below-above median)
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ death_rate_last6mo_lad_median,
          estimate = "mm")
plot2 <- plot(est, group = "death_rate_last6mo_lad_median",  legend_title = "", vline = 0.5)
plot2 <- plot2 + ggplot2::theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  ggtitle("Death Rate (last 6 months)") 

# 1 plot for mm vaccination rate (Death 6 months, below-above median)
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by =  ~ vaccine_1dose_lad_last3mo_median,
          estimate = "mm")
plot3 <- plot(est, group = "vaccine_1dose_lad_last3mo_median",  legend_title = "", vline = 0.5)
plot3 <- plot3 + ggplot2::theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Vaccination Rate (last 6 months)", ) 

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

ggplot2::ggsave(plot, height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_infections_deaths_vaccines_6m_median_pooled.png"))

cat("Figure A12 saved as mm_infections_deaths_vaccines_6m_median_pooled.png in the plots directory.\n")


