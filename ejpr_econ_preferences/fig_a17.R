plot2<-create_conjoint_plot(dat_combined, "education", "mm", "","","N")+theme(legend.position = "bottom")
ggsave(plot2,height = 6, width = 9, filename= file.path(PLOTS_PATH, "mm_education_pooled.png"))
         
cat("Figure a17 saved as mm_education_pooled.png in the plots directory.\n")