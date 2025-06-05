datmarginals_uk <- datmarginals_uk %>%
  dplyr::mutate(
    survey_before29march_vs_after29march = case_when(
      survey_before_8march == 1 | survey_btw_7mar_29mar == 1 ~ "Before 29 March",
      survey_btw_28mar_12apr == 1 | survey_after_11apr == 1 ~ "After 29 March"
    ) %>% as.factor
    )
datmarginals_uk <- datmarginals_uk %>%
  dplyr::mutate(
    survey_before12april_vs_after12april = case_when(
      survey_btw_7mar_12apr == 1 | survey_before_8march == 1 ~ "Before cutoff date",
      survey_after_11apr == 1 ~ "After cutoff date"
    ) %>% as.factor) %>% 
  dplyr::mutate(
    survey_before8march_vs_after8march = case_when(
      survey_before_8march == 1 ~ "Before 8 March",
      survey_before_8march != 1 ~ "After 8 March",
    ) %>% as.factor)

plot1<-create_conjoint_plot(datmarginals_uk, "survey_before29march_vs_after29march", "mm", "","Before/after 29 March","F")
plot2<-create_conjoint_plot(datmarginals_uk, "survey_before12april_vs_after12april", "mm", "","Before/after 12 April","L")+theme(legend.position = "right")
plotall<-egg::ggarrange(plot1,plot2,nrow = 1)

ggsave(plotall,height = 9, width = 12, filename= file.path(PLOTS_PATH, "mm_uk_29mar_12apr_color.png"))
         
cat("Figure A14 saved to", file.path(PLOTS_PATH, "mm_uk_29mar_12apr.png"), "\n")