p1<-create_conjoint_plot(dat_UK, "white_bin", "mm", "Ethnicity","UK\n(ethnic minority)","F")
p2<-create_conjoint_plot(dat_AU, "white_bin", "mm", "Ethnicity","Australia\n(ethnic minority incl. ATSI)","M")
p3<-create_conjoint_plot(dat_AU, "aboriginal_bin", "mm", "Ethnicity","Australia\n(ATSI only)","M")
p4<-create_conjoint_plot(dat_combined, "white_bin", "mm", "Ethnicity","Pooled ethnic\nminority/ATSI","L")+
  scale_color_discrete(na.translate = FALSE,labels = c("Ethnic minority", "Non-minority group"))

eth_bin<-ggarrange(p1,p2,p3,p4,nrow = 1)
ggsave(eth_bin,height = 6, width = 10, filename= file.path(PLOTS_PATH, "ethnicity_minority_ethnic_atsi_pooled.png"))

cat("Figure A18 saved to", file.path(PLOTS_PATH, "ethnicity_minority_ethnic_atsi_pooled.png"), "\n")