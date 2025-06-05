dat_combined<-dat_combined %>% 
  mutate(reduce_ws = ifelse(grepl("one",`Wage Subsidies`),1,0)) %>%
  mutate(reduce_ph = ifelse(grepl("one",`Payment Holidays`),1,0)) %>%
  mutate(sum_reduce = ifelse((reduce_ph + reduce_ws) > 0,1,0)) %>% 
  group_by(respondent_id) %>% 
  mutate(imp_times = sum(sum_reduce)) %>%
  mutate(imp_buckets = case_when(imp_times<4 ~ "0-3 illogical conditions",
                                 imp_times<7 ~ "4-6 illogical conditions") %>% as.factor)  

amce_simple<-create_conjoint_plot(dat_combined, "imp_buckets", "amce", "","","F")+theme(legend.position = "none")
mm_simple<-create_conjoint_plot(dat_combined, "imp_buckets", "mm", "","","L")
ate_comb<-egg::ggarrange(amce_simple,mm_simple,nrow=1)
ggplot2::ggsave(ate_comb, height = 9, width = 9, filename= file.path(PLOTS_PATH, "illogical_conditions_mm_amce.png"))
                  
cat("figure a7 saved as illogical_conditions_mm_amce.png\n")

