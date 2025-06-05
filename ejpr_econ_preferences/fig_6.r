# Code to generate mm_vic100_uk8mar_color.png

datmarginals_au <- datmarginals_au %>%
  mutate(
    victoria100_vs_everyoneelse = case_when(
      g_state == "Victoria" & postcode_lock_days >= 100 ~ "Victoria, 100+ days of lockdown",
      (g_state == "Victoria" & postcode_lock_days < 100) | g_state == "Queensland" | g_state == "New South Wales" | 
        g_state == "South Australia" | g_state == "Tasmania" | g_state == "Western Australia"  ~ "Everyone Else"
    )
  )


datmarginals_au$victoria100_vs_everyoneelse <- factor(datmarginals_au$victoria100_vs_everyoneelse, levels=c("Victoria, 100+ days of lockdown", "Everyone Else"))

plot1<-create_conjoint_plot(datmarginals_au, "victoria100_vs_everyoneelse", "mm", "","Australia","N")+theme(legend.position = "bottom") #
plot2<-create_conjoint_plot(datmarginals_uk, "survey_before8march_vs_after8march", "mm", "","UK","L")+theme(legend.position = "bottom") #
plotall<-egg::ggarrange(plot1,plot2,nrow = 1) #
ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH,"mm_vic100_uk8mar_color.png")) 
cat("Figure 6 saved as mm_vic100_uk8mar_color.png in the plots directory.\n")
