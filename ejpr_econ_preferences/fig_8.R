dat_combined <- dat_combined %>%
  dplyr::mutate(
    furloughed_vs_non_furloughed = case_when(
      Q_employment == "Furloughed" ~ "Furloughed",
      Q_employment == "Paid Employment or Self-Employed" | Q_employment == "Retired" | Q_employment == "Unemployed"  ~ "Not furloughed"
    ) %>% as.factor %>% relevel(., ref="Not furloughed")) %>% #
  mutate(
    unemployed_vs_non_unemployed = case_when(
      Q_employment == "Unemployed" ~ "Unemployed",
      Q_employment == "Paid Employment or Self-Employed" | Q_employment == "Furloughed"  | Q_employment == "Retired" ~ "Not unemployed"
    ) %>% as.factor) 

plot1<-create_conjoint_plot(dat_combined, "underbanked", "mm", "","Financial inclusion","N")+theme(legend.position = "bottom") #
plot2<-create_conjoint_plot(dat_combined, "furloughed_vs_non_furloughed", "mm", "","Furlough status","L")+theme(legend.position = "bottom") #
plot3<-create_conjoint_plot(dat_combined, "unemployed_vs_non_unemployed", "mm", "","Employment status","L")+theme(legend.position = "bottom") #
plotall<-egg::ggarrange(plot1,plot2,plot3,nrow = 1) #
ggsave(plotall,height = 6, width = 10, filename= file.path(PLOTS_PATH,"mm_underbanked_furlough_employment_color.png")) 

cat("Figure 8 saved as mm_underbanked_furlough_employment_color.png in the plots directory.\n")