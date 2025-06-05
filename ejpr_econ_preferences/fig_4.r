# Code to generate covid_health_impact_mm_combined.png

#plot for paper - 4 charts side-by-side
plot1<-create_conjoint_plot(dat_combined, "economic_loss_personal", "mm", "","Personal economic loss","F") #
plot2<-create_conjoint_plot(dat_combined, "economic_loss_network", "mm", "","Personal or network economic loss","M") #
plot3<-create_conjoint_plot(dat_combined, "health_loss_personal", "mm", "","Personal health loss","M") #
plot4<-create_conjoint_plot(dat_combined, "health_loss_network", "mm", "","Personal or network health loss","L")+ #
  scale_color_discrete(na.translate = FALSE,labels = c("No loss", "Loss")) #

plotall<-egg::ggarrange(plot1,plot2,plot3,plot4,nrow=1) #

ggplot2::ggsave(plotall,height = 9, width = 16, filename= file.path(PLOTS_PATH, "covid_health_impact_mm_combined.png"))

cat("Figure 4 saved as covid_health_impact_mm_combined.png\n")

