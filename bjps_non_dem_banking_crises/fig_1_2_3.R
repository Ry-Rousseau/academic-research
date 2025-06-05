
# FIGURE 1

# Calculate means with DC/PF information
cdfdcm <- cdf_income_cats %>% 
  left_join(., nd_years %>% summarise(DC_Mand_Revise2 = max(DC_Mand_Revise2, na.rm=T)), by = 'country')

# Calculate means for all countries
cdfdcm_alli <- cdfdcm %>% 
  group_by(DC_Mand_Revise2, variable, year) %>% 
  summarise(mean = mean(value), n=n()) %>%
  ungroup() %>% 
  group_by(variable, DC_Mand_Revise2) %>% 
  summarise(mean = mean(mean), n=n()) %>%
  mutate(income_level_new = "All countries")

# Calculate means by income level
cdfdcm_byi <- cdfdcm %>% 
  group_by(income_level_new, variable, DC_Mand_Revise2, year) %>% 
  summarise(mean = mean(value), n=n()) %>%
  ungroup() %>% 
  group_by(income_level_new, variable, DC_Mand_Revise2) %>% 
  summarise(mean = mean(mean), n=n()) 

# Combine and format data
cdfdcm_rbound <- rbind(cdfdcm_alli, cdfdcm_byi) %>% 
  mutate(`DC/PF` = ifelse(DC_Mand_Revise2 == 0, 'No', "Yes") %>% as.factor())

# Calculate again without Singapore
cdfdcm_alli <- cdfdcm %>% 
  group_by(DC_Mand_Revise2, variable, year) %>% 
  filter(country != "Singapore") %>% 
  summarise(mean = mean(value), n=n()) %>%
  ungroup() %>% 
  group_by(variable, DC_Mand_Revise2) %>% 
  summarise(mean = mean(mean), n=n()) %>%
  mutate(income_level_new = "All countries")

cdfdcm_byi <- cdfdcm %>% 
  filter(country != "Singapore") %>% 
  group_by(income_level_new, variable, DC_Mand_Revise2, year) %>% 
  summarise(mean = mean(value), n=n()) %>%
  ungroup() %>% 
  group_by(income_level_new, variable, DC_Mand_Revise2) %>% 
  summarise(mean = mean(mean), n=n()) 

# Create dataset without Singapore
cdfdcm_rbound_ns <- rbind(cdfdcm_alli, cdfdcm_byi) %>% 
  mutate(`DC/PF` = ifelse(DC_Mand_Revise2 == 0, 'No', "Yes (excl. Singapore)") %>% as.factor()) %>% 
  filter(`DC/PF` == "Yes (excl. Singapore)")

# Combine all data
cdfdcm_rbound_both <- rbind(cdfdcm_rbound_ns, cdfdcm_rbound)

# Set factor levels for proper ordering
cdfdcm_rbound_both$`DC/PF` <- factor(cdfdcm_rbound_both$`DC/PF`, 
                                     levels = c("Yes", "Yes (excl. Singapore)", "No"))

# Remove exclude Singapore low income rows
cdfdcm_rbound_both <- cdfdcm_rbound_both[!(grepl("Sin", cdfdcm_rbound_both$`DC/PF`) & 
                                             grepl("Low", cdfdcm_rbound_both$income_level_new)),]

# Create and save plot three
ggplot(cdfdcm_rbound_both) +
  geom_col(aes(x = income_level_new, y = mean, fill = `DC/PF`), position = "dodge") +
  facet_wrap(~variable) +
  labs(y = "Average wealth per adult \n(000s USD, constant 2022 prices, ppp)", x = "Income group") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"), 
        axis.text.x = element_text(angle = -45, hjust = 0)) +
  ylim(0, NA)

ggsave(paste0(FIGURE_PATH, "/wid_ave_wealth_barplot_allthree.png"), width = 10, height = 6)

cat(paste("Written figure 1 to", FIGURE_PATH))

# FIGURE 2

# horiz cov kr paop ####
gg_hck_paop <- data %>% 
  filter(!is.na(private_oldageprog), !is.na(bmr_democracy)) %>% 
  mutate(category = case_when(bmr_democracy == 1 ~ "Democracies,\nall pension schemes",
                              bmr_democracy == 0 & private_oldageprog == 0 ~ "Autocracies,\nother pension schemes",
                              bmr_democracy == 0 & private_oldageprog == 1 ~ "Autocracies,\nmandatory DC schemes/provident funds",
                              .default = NA)) %>% 
  group_by(category) %>% 
  summarise(mean = mean(horiz_cov_kr, na.rm = TRUE),
            lower_ci = mean(horiz_cov_kr, na.rm = TRUE) - qt(0.975, df=n()-1) * sd(horiz_cov_kr, na.rm = TRUE) / sqrt(n()),
            upper_ci = mean(horiz_cov_kr, na.rm = TRUE) + qt(0.975, df=n()-1) * sd(horiz_cov_kr, na.rm = TRUE) / sqrt(n())) %>% 
  arrange(-mean)

gg_hck_paop$color = c("x","y","y")

ggplot(gg_hck_paop)+
  geom_col(aes(x = mean, y = reorder(category,-mean), fill = category))+
  coord_flip()+
  geom_errorbar(mapping = aes(xmin = lower_ci, xmax = upper_ci, y = category),width = 0.25)+
  theme_minimal()+
  theme(legend.position = "none",plot.background = element_rect(fill = "white"))+
  labs(x = "Horizontal pension coverage",y="")

ggsave(paste0(FIGURE_PATH, "/hck_by_democracy_paop.png"),width=7,height=5)

cat(paste("Written figure 2 to", FIGURE_PATH))

# FIGURE 3

# Get country years of crisis countries
data_cc<-data%>% filter(!is.na(Norm_Index_canon),bmr_democracy==0)

order_df = data.frame(name = c("Liquidity", "Guarantees",
                               "Capital injection", "Asset management",
                               "Resolutions","Rules","No intervention"),
                      name_order = 1:7)

gg_piotdcm2<-rbind(data_cc %>% 
                     filter(year>1945) %>% 
                     dplyr::select(year,DC_Mand_Revise2,asset_management_dummy,capital_injection_dummy,guarantee_dummy,liquidity_bailout,resolutions_dummy,rules_dummy,no_intervention_dummy) %>% 
                     mutate(period="Post-1945") %>% 
                     mutate(DC_Mand_Revise2=ifelse(DC_Mand_Revise2==0,"No","Yes")) %>% 
                     pivot_longer(cols = c("asset_management_dummy","capital_injection_dummy",'guarantee_dummy','liquidity_bailout','resolutions_dummy','rules_dummy','no_intervention_dummy')) %>% 
                     mutate(name = case_when(
                       name == "liquidity_bailout" ~ "Liquidity",
                       name == "guarantee_dummy" ~ "Guarantees",
                       name == "capital_injection_dummy" ~ "Capital injection",
                       name == "asset_management_dummy" ~ "Asset management",
                       name == "resolutions_dummy" ~ "Resolutions",
                       name == "rules_dummy" ~ "Rules",
                       name == "no_intervention_dummy" ~ "No intervention")) %>% 
                     group_by(DC_Mand_Revise2,name,period) %>% 
                     summarise(mean = mean(value, na.rm = TRUE),
                               lower_ci = mean(value, na.rm = TRUE) - qt(0.975, df=n()-1) * sd(value, na.rm = TRUE) / sqrt(n()),
                               upper_ci = mean(value, na.rm = TRUE) + qt(0.975, df=n()-1) * sd(value, na.rm = TRUE) / sqrt(n()),
                               n=n()),
                   data_cc %>% 
                     dplyr::select(year,DC_Mand_Revise2,asset_management_dummy,capital_injection_dummy,guarantee_dummy,liquidity_bailout,resolutions_dummy,rules_dummy,no_intervention_dummy) %>% 
                     mutate(period="All Years") %>% 
                     mutate(DC_Mand_Revise2=ifelse(DC_Mand_Revise2==0,"No","Yes")) %>% 
                     pivot_longer(cols = c("asset_management_dummy","capital_injection_dummy",'guarantee_dummy','liquidity_bailout','resolutions_dummy','rules_dummy','no_intervention_dummy')) %>% 
                     mutate(name = case_when(
                       name == "liquidity_bailout" ~ "Liquidity",
                       name == "guarantee_dummy" ~ "Guarantees",
                       name == "capital_injection_dummy" ~ "Capital injection",
                       name == "asset_management_dummy" ~ "Asset management",
                       name == "resolutions_dummy" ~ "Resolutions",
                       name == "rules_dummy" ~ "Rules",
                       name == "no_intervention_dummy" ~ "No intervention")) %>% 
                     group_by(DC_Mand_Revise2,name,period) %>% 
                     summarise(mean = mean(value, na.rm = TRUE),
                               lower_ci = mean(value, na.rm = TRUE) - qt(0.975, df=n()-1) * sd(value, na.rm = TRUE) / sqrt(n()),
                               upper_ci = mean(value, na.rm = TRUE) + qt(0.975, df=n()-1) * sd(value, na.rm = TRUE) / sqrt(n()),
                               n=n())) %>% 
  rename(`DC/PF` = DC_Mand_Revise2) %>% 
  ungroup() %>% 
  arrange(`DC/PF`) %>% 
  mutate(order = c(rep(1,14),rep(0,14))) %>% 
  left_join(order_df)

ggplot(gg_piotdcm2)+
  geom_col(aes(x = reorder(name,name_order), y = mean, fill = reorder(`DC/PF`,order)),position = "dodge")+
  facet_wrap(~period)+
  labs(fill = "DC/PF", y = 'Proportion', x = "Policy")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white"),axis.text.x = element_text(angle = -45, hjust=0))

ggsave(paste0(FIGURE_PATH, "/policy_proportion_by_dcm2_1945.png"),width=8,height=5)

cat(paste("Written figure 3 to", FIGURE_PATH))













