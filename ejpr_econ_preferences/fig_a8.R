# 1 plot for mm
est <- cj(dat_combined, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
          id =  ~ respondent_id, by = ~ country, estimate = "mm")

plot1 <- plot(est, group = "country", legend_title = "Country", vline = 0.5)

ggsave(plot1,height = 9, width = 9, filename= file.path(PLOTS_PATH, "by_country_marginal_means_Colour.png"))

cat("figure a8 saved to", file.path(PLOTS_PATH, "by_country_marginal_means_Colour.png"), "\n")