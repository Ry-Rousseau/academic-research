plot1 <- ggplot(data = covid_data, aes(x = date, y = new_cases_smoothed_per_million, group = location, colour = location)) + 
  geom_line() +
  ggtitle("Confirmed cases") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_color_manual(values=c('red', 'navy')) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "right",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1.5, 'cm'),
        legend.text=element_text(size=12)) +
  scale_x_date(date_breaks = "6 months", 
               labels=scales::date_format("%b-%Y"),
               limits = as.Date(c('2020-01-03','2021-06-30')))

plot2 <- ggplot(data = covid_data, aes(x = date, y = new_deaths_smoothed_per_million, group = location, colour = location)) + 
  geom_line() +
  ggtitle("Confirmed deaths") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_color_manual(values=c('red', 'navy')) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "right",legend.title=element_blank(),
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1.5, 'cm'),
        legend.text=element_text(size=12)) +
  scale_x_date(date_breaks = "6 months", 
               labels=scales::label_date("%b-%Y"),
               limits = as.Date(c('2020-01-03','2021-06-30')))

plot <- egg::ggarrange(plot1 + theme(axis.text.y = element_text(size = 8),legend.position = "none"), 
                       plot2 + theme(axis.text.y = element_text(size = 8)),
                       nrow = 1) 

ggplot2::ggsave(plot, height = 6, width = 12, filename= file.path(PLOTS_PATH, "7_day_ra_cases_deaths_sidebyside_color.png"))

cat("figure 1 saved to", file.path(PLOTS_PATH, "7_day_ra_cases_deaths_sidebyside_color.png"), "\n")