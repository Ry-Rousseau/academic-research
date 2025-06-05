# age diff cohorts ####
dat_combined <- dat_combined %>%
  mutate(
    age35 = case_when(
      Q_age == "18-24" | Q_age == "25-34" ~ "18-34",
      Q_age == "35-44" | Q_age == "45-54" | Q_age == "55-64" ~ "35-64",
      Q_age == "65+" ~ "65+"
    ) %>% as.factor
  )

plot1<-create_conjoint_plot(dat_combined, "age35", "mm", "","","N")+theme(legend.position = "bottom")
ggsave(plot1,height = 9, width = 9, filename= file.path(PLOTS_PATH, "mm_age_18_35_color.png"),
       dpi = 300)

cat("Figure A29 saved as 'mm_age_18_35_color.png' in the plots directory.\n")