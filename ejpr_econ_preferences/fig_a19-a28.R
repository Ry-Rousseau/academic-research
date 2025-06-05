# A19

#Home ownership ####
plot1<-create_conjoint_plot(dat_combined, "Q_home_2", "mm", "","","N")+theme(legend.position = "bottom")
ggsave(plot1,height = 6, width = 9, filename= file.path(PLOTS_PATH, "mm_homeowners_vs_renters_color.png"))

cat("Figure a19 saved as mm_homeowners_vs_renters_color.png\n")

# A20

# House price-- median/20 ####
dat_combined <- dat_combined %>%
  mutate(house_price_top20_bottom20 = case_when(
    house_price_survey_lad_top_bottom == "Low" ~ "Bottom 20",
    house_price_survey_lad_top_bottom == "High" ~ "Top 20"
  ))
dat_combined$house_price_top20_bottom20 <- factor(dat_combined$house_price_top20_bottom20, levels = c("Bottom 20", "Top 20"))

plot1<-create_conjoint_plot(dat_combined, "house_price_survey_lad_median", "mm", "","","N")+theme(legend.position = "bottom")
plot2<-create_conjoint_plot(dat_combined, "house_price_top20_bottom20", "mm", "","","L")+theme(legend.position = "bottom")
plotall<-ggarrange(plot1,plot2,nrow = 1)

ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_house_median_20_color.png"))

cat("Figure a20 saved as mm_house_median_20_color.png\n")

# A21

dat_combined <- dat_combined %>%
  mutate(house_price_pc_1yr_top20_bottom20 = case_when(
    house_price_survey_pc_1yr_lad_top_bottom == "Low" ~ "Bottom 20",
    house_price_survey_pc_1yr_lad_top_bottom == "High" ~ "Top 20"
  ))

dat_combined$house_price_pc_1yr_top20_bottom20 <- factor(dat_combined$house_price_pc_1yr_top20_bottom20, levels = c("Bottom 20", "Top 20"))
class(dat_combined$house_price_pc_1yr_top20_bottom20)

dat_combined <- dat_combined %>%
  mutate(house_price_pc_5yr_top20_bottom20 = case_when(
    house_price_survey_pc_5yr_lad_top_bottom == "Low" ~ "Bottom 20",
    house_price_survey_pc_5yr_lad_top_bottom == "High" ~ "Top 20"
  ))

dat_combined$house_price_pc_5yr_top20_bottom20 <- factor(dat_combined$house_price_pc_5yr_top20_bottom20, levels = c("Bottom 20", "Top 20"))
class(dat_combined$house_price_pc_5yr_top20_bottom20)

plot1<-create_conjoint_plot(dat_combined, "house_price_survey_pc_1yr_lad_med", "mm", "","","N")+theme(legend.position = "bottom")
plot2<-create_conjoint_plot(dat_combined, "house_price_pc_1yr_top20_bottom20", "mm", "","","L")+theme(legend.position = "bottom")
plotall<-ggarrange(plot1,plot2,nrow = 1)
ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_house_median_20_1yr_change_color.png"))

cat("Figure a21 saved as mm_house_median_20_1yr_change_color.png\n")

# A 22

plot1<-create_conjoint_plot(dat_combined, "house_price_survey_pc_5yr_lad_med", "mm", "","","N")+theme(legend.position = "bottom")
plot2<-create_conjoint_plot(dat_combined, "house_price_pc_5yr_top20_bottom20", "mm", "","","L")+theme(legend.position = "bottom")
plotall<-ggarrange(plot1,plot2,nrow = 1)
ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_house_median_20_5yr_change_color.png"))

cat("Figure a22 saved as mm_house_median_20_5yr_change_color.png\n")

# A 23

dat_combined <- dat_combined %>%
  mutate(house_price_homeowners_vs_renters_top20_bottom_20 = case_when(
    house_price_survey_lad_top_bottom_6 == "Home Owners (outright OR mortgage) x Low" ~ "Home owners x bottom 20",
    house_price_survey_lad_top_bottom_6 == "Home Owners (outright OR mortgage) x High" ~ "Home owners x top 20",
    
    house_price_survey_lad_top_bottom_6 == "Renters x Low" ~ "Renters x bottom 20",
    house_price_survey_lad_top_bottom_6 == "Renters x High" ~ "Renters x top 20"
  )) %>% 
  mutate(own_rent_house_price_survey_lad_median_plot = case_when(
    own_rent_house_price_survey_lad_median == "Home Owners (outright OR mortgage) x Below or Equal Median" ~ "Home owners x below/equal to median",
    own_rent_house_price_survey_lad_median == "Home Owners (outright OR mortgage) x Above Median" ~ "Home owners x above median",
    
    own_rent_house_price_survey_lad_median == "Renters x Below or Equal Median" ~ "Renters x below/equal to median",
    own_rent_house_price_survey_lad_median == "Renters x Above Median" ~ "Renters x above median"
  ) %>% as.factor)


dat_combined$house_price_homeowners_vs_renters_top20_bottom_20 <- factor(dat_combined$house_price_homeowners_vs_renters_top20_bottom_20, 
                                                                         levels = c("Home owners x top 20", 
                                                                                    "Home owners x bottom 20",
                                                                                    "Renters x top 20",
                                                                                    "Renters x bottom 20"))

plot1<-create_conjoint_plot(dat_combined, "own_rent_house_price_survey_lad_median_plot", "mm", "","","N")+
  theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))
plot2<-create_conjoint_plot(dat_combined, "house_price_homeowners_vs_renters_top20_bottom_20", "mm", "","","L")+
  theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))
plotall<-ggarrange(plot1,plot2,nrow = 1)
ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_own_rent_house_price_median_20_color.png"))

cat("Figure a23 saved as mm_own_rent_house_price_median_20_color.png\n")

# A 24

#### Homeowners v. Renters X 1 year change ####
dat_combined <- dat_combined %>%
  mutate(house_price_pc_1yr_homeowners_vs_renters_top20_bottom_20  = case_when(
    house_price_survey_pc_1yr_lad_top_bottom_6 == "Home Owners (outright OR mortgage) x Low" ~ "Home owners x bottom 20",
    house_price_survey_pc_1yr_lad_top_bottom_6 == "Home Owners (outright OR mortgage) x High" ~ "Home owners x top 20",
    
    house_price_survey_pc_1yr_lad_top_bottom_6 == "Renters x Low" ~ "Renters x bottom 20",
    house_price_survey_pc_1yr_lad_top_bottom_6 == "Renters x High" ~ "Renters x top 20"
  )) %>% 
  mutate(own_rent_house_price_survey_pc_1yr_lad_median_plot = case_when(
    own_rent_house_price_survey_pc_1yr_lad_median == "Home Owners (outright OR mortgage) x Below or Equal Median" ~ "Home owners x below/equal to median",
    own_rent_house_price_survey_pc_1yr_lad_median == "Home Owners (outright OR mortgage) x Above Median" ~ "Home owners x above median",
    
    own_rent_house_price_survey_pc_1yr_lad_median == "Renters x Below or Equal Median" ~ "Renters x below/equal to median",
    own_rent_house_price_survey_pc_1yr_lad_median == "Renters x Above Median" ~ "Renters x above median"
  ) %>% as.factor)


dat_combined$house_price_pc_1yr_homeowners_vs_renters_top20_bottom_20 <- factor(dat_combined$house_price_pc_1yr_homeowners_vs_renters_top20_bottom_20,
                                                                                levels = c("Home owners x top 20", 
                                                                                           "Home owners x bottom 20",
                                                                                           "Renters x top 20",
                                                                                           "Renters x bottom 20"))

plot1<-create_conjoint_plot(dat_combined, "own_rent_house_price_survey_pc_1yr_lad_median_plot", "mm", "","","N")+
  theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))
plot2<-create_conjoint_plot(dat_combined, "house_price_pc_1yr_homeowners_vs_renters_top20_bottom_20", "mm", "","","L")+
  theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))
plotall<-ggarrange(plot1,plot2,nrow = 1)
ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_own_rent_house_price_pc_1yr_median_20_color.png"))

cat("Figure a24 saved as mm_own_rent_house_price_pc_1yr_median_20_color.png\n")

# A 25

dat_combined <- dat_combined %>%
  mutate(house_price_pc_5yr_homeowners_vs_renters_top20_bottom_20  = case_when(
    house_price_survey_pc_5yr_lad_top_bottom_6 == "Home Owners (outright OR mortgage) x Low" ~ "Home owners x bottom 20",
    house_price_survey_pc_5yr_lad_top_bottom_6 == "Home Owners (outright OR mortgage) x High" ~ "Home owners x top 20",
    
    house_price_survey_pc_5yr_lad_top_bottom_6 == "Renters x Low" ~ "Renters x bottom 20",
    house_price_survey_pc_5yr_lad_top_bottom_6 == "Renters x High" ~ "Renters x top 20"
  )) %>% 
  mutate(own_rent_house_price_survey_pc_5yr_lad_median_plot = case_when(
    own_rent_house_price_survey_pc_5yr_lad_median == "Home Owners (outright OR mortgage) x Below or Equal Median" ~ "Home owners x below/equal to median",
    own_rent_house_price_survey_pc_5yr_lad_median == "Home Owners (outright OR mortgage) x Above Median" ~ "Home owners x above median",
    
    own_rent_house_price_survey_pc_5yr_lad_median == "Renters x Below or Equal Median" ~ "Renters x below/equal to median",
    own_rent_house_price_survey_pc_5yr_lad_median == "Renters x Above Median" ~ "Renters x above median"
  ) %>% as.factor)


dat_combined$house_price_pc_5yr_homeowners_vs_renters_top20_bottom_20 <- factor(dat_combined$house_price_pc_5yr_homeowners_vs_renters_top20_bottom_20,
                                                                                levels = c("Home owners x top 20", 
                                                                                           "Home owners x bottom 20",
                                                                                           "Renters x top 20",
                                                                                           "Renters x bottom 20"))

plot1<-create_conjoint_plot(dat_combined, "own_rent_house_price_survey_pc_5yr_lad_median_plot", "mm", "","","N")+
  theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))
plot2<-create_conjoint_plot(dat_combined, "house_price_pc_5yr_homeowners_vs_renters_top20_bottom_20", "mm", "","","L")+
  theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))
plotall<-ggarrange(plot1,plot2,nrow = 1)
ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_own_rent_house_price_pc_5yr_median_20_color.png"))


cat("Figure a25 saved as mm_own_rent_house_price_pc_5yr_median_20_color.png\n")

# A 26
dat_combined <- dat_combined %>%
  mutate(financial_asset_ownership_top20_bottom20 = case_when(
    Total_Financial_Asset_Ownership6 == "Low" ~ "Bottom 20",
    Total_Financial_Asset_Ownership6 == "High" ~ "Top 20"
  ))

dat_combined$financial_asset_ownership_top20_bottom20 <- factor(dat_combined$financial_asset_ownership_top20_bottom20, levels = c("Bottom 20", "Top 20"))

plot1<-create_conjoint_plot(dat_combined, "Total_Financial_Asset_Ownership4", "mm", "","","N")+theme(legend.position = "bottom")
plot2<-create_conjoint_plot(dat_combined, "financial_asset_ownership_top20_bottom20", "mm", "","","L")+theme(legend.position = "bottom")
plotall<-ggarrange(plot1,plot2,nrow = 1)
ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_financial_assets_median_20_color.png"))
cat("Figure a26 saved as mm_financial_assets_median_20_color.png\n")

# A27
plot<-create_conjoint_plot(dat_combined, "Q_inequality", "mm","","","N")
ggsave(plot,height = 6, width = 9, filename= file.path(PLOTS_PATH, "inequality_shape_pooled_color.png"))
cat("Figure a27 saved as inequality_shape_pooled_color.png\n")

# A28
plot<-create_conjoint_plot(dat_combined, "Q_poverty", "mm", "","","N")+theme(legend.position = "bottom")
ggsave(plot,height = 6, width = 9, filename= file.path(PLOTS_PATH, "mm_poverty_perception_color.png"))
cat("Figure a28 saved as mm_poverty_perception_color.png\n")





