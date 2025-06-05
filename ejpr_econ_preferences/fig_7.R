# Code to generate mm_partisan_inequality_type_change_color.png

dat_combined<-dat_combined %>%
  mutate(Q_ideology_self_perc.3.factored=ifelse(Q_ideology_self_perc.3=="Conservative","Conservative","Progressive") %>% as.factor) #

plot3<-create_conjoint_plot(dat_combined, "Q_ideology_self_perc.3.factored", "mm", "","Partisanship","N")+theme(legend.position = "bottom") #
plot1<-create_conjoint_plot(dat_combined, "inequality_three_b", "mm", "","Perceived society type","L")+theme(legend.position = "bottom") +guides(color=guide_legend(nrow=2))#
plot2<-create_conjoint_plot(dat_combined, "top10change", "mm", "","Perceived inequality trend","L")+theme(legend.position = "bottom") +guides(color=guide_legend(nrow=2))#
plotall<-egg::ggarrange(plot3,plot1,plot2,nrow = 1) #
ggsave(plotall,height = 6, width = 10, filename= file.path(PLOTS_PATH,"mm_partisan_inequality_type_change_color.png")) 

cat("Figure 7 saved as mm_partisan_inequality_type_change_color.png in the output/plots directory.\n")