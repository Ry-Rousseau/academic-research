mod200<-lm(atsi_prop~prop_200,data= atsi_firms)

gg200<-ggplot(atsi_firms) +
  geom_point(aes(x = prop_200, y =atsi_prop))+
  geom_abline(intercept = coefficients(mod200)[1], slope = coefficients(mod200)[2], color="red")+
  annotate("text", x = 0.35, y = 0.055, label = paste("R² = ", round(summary(mod200)$r.squared, 2)), 
           hjust = 0.5, vjust = 0.5, size = 5, color = "black")+
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Proportion of firms in industry >200 employees",
       y = "Proportion of employees in industry who are ATSI",
       title = "")+
  geom_text(data = filter(atsi_firms, atsi_prop > 0.05 | prop_200 > 0.1 & prop_200<0.37), 
            aes(x = prop_200, y = atsi_prop, label = industry), 
            hjust = 0.5, vjust = 1.5, check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1))

ggsave(gg200,, width = 9, height= 6, filename = file.path(PLOTS_PATH, "atsi_firmsize.png"))

cat("Figure A30 saved to", file.path(PLOTS_PATH, "atsi_firmsize.png"), "\n")