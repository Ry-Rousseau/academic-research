# This script mutates some new variables for certain group measurements and cleans up a lot of the factored variabels in the data

names(datmarginals_uk)[1]<-"id"
datmarginals_uk<-datmarginals_uk %>% mutate("id" = paste0(id,"_","uk"))

cols2 <- c("wages_new", "wage_length_new", "hols_new", "hols_length_new", "CBpols_new")
datmarginals_uk[cols2] <- lapply(datmarginals_uk[cols2], as.factor)

levels(datmarginals_uk$wages_new) <- c("80%", "70%", "60%", "50%", "None  ")
datmarginals_uk$wages_new <- factor(datmarginals_uk$wages_new, levels = c("None  ", "50%", "60%","70%", "80%"))

levels(datmarginals_uk$wage_length_new) <- c("3 months", "6 months", "9 months", "12 months", "Open-ended")

#Ref 5
levels(datmarginals_uk$hols_new) <- c("Only households demonstrating COVID-related distress","Only mortgage holders","Only mortgage holders and renters",  "All debtors and renters", "None " )
datmarginals_uk$hols_new <- factor(datmarginals_uk$hols_new, levels = rev(c("Only households demonstrating COVID-related distress",
                                                                            "Only mortgage holders",
                                                                            "Only mortgage holders and renters",  
                                                                            "All debtors and renters", "None " )))

levels(datmarginals_uk$hols_length_new) <- c("3 months ", "6 months ", "9 months ", "12 months ", "Open-ended ")

levels(datmarginals_uk$CBpols_new) <- c("None", "Purchase bank assets", "Support large firms", "Support small firms", "Support households")

names(datmarginals_uk)

names(datmarginals_uk)[462:466]     <- c("Wage Subsidies",
                                         "Wage Subsidy Duration",
                                         "Payment Holidays",
                                         "Payment Holiday Duration",
                                         "Central Bank Policies" )

#Ref 5
levels(datmarginals_uk$`Payment Holidays`) <- c("Only households demonstrating COVID-related distress","Only mortgage holders","Only mortgage holders and renters",  "All debtors and renters", "None " )
datmarginals_uk$`Payment Holidays` <- factor(datmarginals_uk$`Payment Holidays`, levels = rev(c("Only households demonstrating COVID-related distress",
                                                                            "Only mortgage holders",
                                                                            "Only mortgage holders and renters",  
                                                                            "All debtors and renters", "None " )))




cols2 <- c("wages_new", "wage_length_new", "hols_new", "hols_length_new", "CBpols_new")
datmarginals_au[cols2] <- lapply(datmarginals_au[cols2], as.factor)

#Ref 5
levels(datmarginals_au$wages_new) <- c("80%", "70%", "60%", "50%", "None  ")
datmarginals_au$wages_new <- factor(datmarginals_au$wages_new, levels = c("None  ", "50%", "60%","70%", "80%"))


levels(datmarginals_au$wage_length_new) <- c("3 months", "6 months", "9 months", "12 months", "Open-ended")

levels(datmarginals_au$hols_new) <- c("Only households demonstrating COVID-related distress","Only mortgage holders","Only mortgage holders and renters",  
                                      "All debtors and renters", "None ")
datmarginals_au$hols_new <- factor(datmarginals_au$hols_new, levels = rev(c("Only households demonstrating COVID-related distress","Only mortgage holders",
                                                                            "Only mortgage holders and renters",  
                                                                            "All debtors and renters", "None " )))

levels(datmarginals_au$hols_length_new) <- c("3 months ", "6 months ", "9 months ", "12 months ", "Open-ended ")

levels(datmarginals_au$CBpols_new) <- c("None", "Purchase bank assets", "Support large firms", "Support small firms", "Support households")

names(datmarginals_au)[171:175]     <- c("Wage Subsidies",
                                         "Wage Subsidy Duration",
                                         "Payment Holidays",
                                         "Payment Holiday Duration",
                                         "Central Bank Policies" )

datmarginals_au <- datmarginals_au %>%
  mutate(
    victoria_vs_everyoneelse = case_when(
      g_state == "Victoria" ~ "Victoria",
      g_state == "Queensland" | g_state == "New South Wales" | g_state == "South Australia" | g_state == "Tasmania" | g_state == "Western Australia" ~ "Other States" 
    ) %>% as.factor
  )

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

