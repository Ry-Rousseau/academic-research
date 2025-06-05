datmarginals_au <- datmarginals_au %>% 
  mutate(experienced_lockdown = ifelse(postcode_lock_days==0,"Not experienced lockdown", "Experienced lockdown") %>% as.factor)

datmarginals_au <- datmarginals_au %>%
  mutate(
    victoria_vs_everyoneelse = case_when(
      g_state == "Victoria" ~ "Victoria",
      g_state == "Queensland" | g_state == "New South Wales" | g_state == "South Australia" | g_state == "Tasmania" | g_state == "Western Australia" ~ "Other States" 
    ) %>% as.factor
  )

plot1<-create_conjoint_plot(datmarginals_au, "victoria_vs_everyoneelse", "mm", "","Victoria","N")+theme(legend.position = "bottom")
plot2<-create_conjoint_plot(datmarginals_au, "experienced_lockdown", "mm", "","Lockdown experienced (all states)","L")+theme(legend.position = "bottom")
plotall<-egg::ggarrange(plot1,plot2,nrow = 1)
ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_aus_vic_color.png")) 

cat("Figure A15 saved to", file.path(PLOTS_PATH, "mm_aus_vic_color.png"), "\n")

