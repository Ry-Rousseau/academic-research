# 1 plot for mm
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, estimate = "mm")

plot1 <- plot(est, vline = 0.5)

# 1 plot for amce
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, estimate = "amce")

plot2 <- plot(est)

ate_comb<-egg::ggarrange(plot2+theme(legend.position = "none"),plot1+theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),legend.position = "none"),nrow=1)

ggsave(ate_comb,height = 12, width = 12, filename= file.path(PLOTS_PATH, "full_sample_amce_mm_both_Colour.png"))

cat("figure 3 saved to", file.path(PLOTS_PATH, "full_sample_amce_mm_both_Colour.png"), "\n")