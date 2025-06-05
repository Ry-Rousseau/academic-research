plot1<-create_conjoint_plot(dat_combined, "Q_income", "mm", "","","N")+theme(legend.position = "bottom")
ggsave(plot1,height = 6, width = 9, filename= file.path(PLOTS_PATH, "mm_Q_income_pooled.png"))

cat("Figure A16 saved as mm_Q_income_pooled.png in the plots directory.\n")


