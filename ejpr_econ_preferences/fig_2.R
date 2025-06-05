plot1 <- ggplot(data = covid_data, aes(x = date, y = people_vaccinated_per_hundred, group = location, colour = location)) + 
  geom_line() +
  ggtitle("First doses") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_color_manual(values=c('red', 'navy')) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "right",legend.title = element_blank(),
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1.5, 'cm'),
        legend.text=element_text(size=12)) +
  scale_x_date(date_breaks = "2 months", 
               labels=scales::date_format("%b-%Y"),
               limits = as.Date(c('2021-01-01','2021-06-30')))

plot2 <- ggplot(data = covid_data, aes(x = date, y = people_fully_vaccinated_per_hundred, group = location, colour = location)) + 
  geom_line() +
  ggtitle("Fully vaccinated") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, 5)) +
  scale_color_manual(values=c('red', 'navy')) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "right",legend.title = element_blank(),
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1.5, 'cm'),
        legend.text=element_text(size=12)) +
  scale_x_date(date_breaks = "2 months", 
               labels=scales::date_format("%b-%Y"),
               limits = as.Date(c('2021-01-01','2021-06-30')))

plot <- egg::ggarrange(plot1 + theme(axis.text.y = element_text(size = 8),legend.position = "none"), 
                       plot2 + theme(axis.text.y = element_text(size = 8)),
                       nrow = 1) 

ggplot2::ggsave(plot, height = 6, width = 12, filename= file.path(PLOTS_PATH, "firstdose_people_fully_vaccinated_per_hundred_colour.png"))

cat("figure 2 saved to", file.path(PLOTS_PATH, "firstdose_people_fully_vaccinated_per_hundred_colour.png"), "\n")
