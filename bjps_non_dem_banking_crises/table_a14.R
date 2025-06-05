# table_A14.R - Generate Table A14 from supplementary material
# DC/PF Effect on Bailout Policy Index - IMF Program and Capital Account Openness Controls

# Initialize results storage
model_results_tab_A14 <- model_results_empty

# For Table A14, we need to include IMF Program and Capital Account Openness controls
democracy_types <- c("BMR", "V-Dem", "POLITY")
additional_controls <- c("imfprog", "kaopen", "Quinn")

# Create data dictionary
# Transform imf prog into binary variable where 0 indicates absence of IMF program OR NA value 
# i.e. assume no IMF program in country-years where none recorded
democracy_data_list <- list(
  "BMR" = list(
    nonimp = bmr_data %>% mutate(imfprog = ifelse(year <= 2019 & is.na(imfprog)==TRUE, 0, imfprog)),
    imp = lapply(data_imp_bmr, function(df) df %>% mutate(imfprog = ifelse(year <= 2019 & is.na(imfprog)==TRUE, 0, imfprog)))
  ),
  "V-Dem" = list(
    nonimp = v2x_data %>% mutate(imfprog = ifelse(year <= 2019 & is.na(imfprog)==TRUE, 0, imfprog)),
    imp = lapply(data_imp_v2x, function(df) df %>% mutate(imfprog = ifelse(year <= 2019 & is.na(imfprog)==TRUE, 0, imfprog)))
  ),
  "POLITY" = list(
    nonimp = p2_data %>% mutate(imfprog = ifelse(year <= 2019 & is.na(imfprog)==TRUE, 0, imfprog)),
    imp = lapply(data_imp_p2, function(df) df %>% mutate(imfprog = ifelse(year <= 2019 & is.na(imfprog)==TRUE, 0, imfprog)))
  )
)

# Loop through democracy types and additional control variables
for (d_type in democracy_types) {
  nonimp_data <- democracy_data_list[[d_type]]$nonimp
  imp_data <- democracy_data_list[[d_type]]$imp
  
  for (add_control in additional_controls) {
    # Base formula with additional control
    base_formula <- paste0("index_canon_no ~ polity2 + e_gdppc + FixedER + DC_Mand_Revise2 + ", add_control)
    
    # Run M1-M4
    for (i in 1:2) {
      # Non-imputed models (M1/M2)
      formula_mi <- as.formula(paste0(base_formula, control_sets[i]))
      model_mi <- tryCatch({
        lm_robust(formula_mi, data = nonimp_data, clusters = country, se_type = 'stata')
      }, error = function(e) {
        message(paste("Error in M", i, "for", d_type, "with var", add_control, ":", e))
        NULL
      })
      
      if (!is.null(model_mi)) {
        # Extract results for DC_Mand_Revise2
        var_of_interest <- "DC_Mand_Revise2"
        est <- coef(summary(model_mi))[var_of_interest, "Estimate"]
        se <- coef(summary(model_mi))[var_of_interest, "Std. Error"]
        pval <- coef(summary(model_mi))[var_of_interest, "Pr(>|t|)"]
        nobs_mi <- nobs(model_mi)
        
        model_results_tab_A14 <- rbind(model_results_tab_A14, data.frame(
          democracy_type = d_type,
          model_number = paste0("M", i),
          effect_name = var_of_interest,
          control_set = control_sets[i],
          control_variable = add_control,
          estimate = est,
          std_error = se,
          p_value = pval,
          nobs = nobs_mi,
          signif_0_05 = pval < 0.05,
          signif_0_10 = pval < 0.10,
          stringsAsFactors = FALSE
        ))
      }
      
      # Imputed models (M3/M4)
      formula_mi_imp <- as.formula(paste0(base_formula, control_sets[i]))
      pooled_mi <- tryCatch({
        combine3(formula_mi_imp, imp_data)
      }, error = function(e) {
        message(paste("Error in M", i+2, "for", d_type, "with var", add_control, ":", e))
        NULL
      })
      
      if (!is.null(pooled_mi)) {
        var_of_interest <- "DC_Mand_Revise2"
        est <- pooled_mi[pooled_mi$term == var_of_interest, "estimate"]
        se <- pooled_mi[pooled_mi$term == var_of_interest, "std.error"]
        pval <- pooled_mi[pooled_mi$term == var_of_interest, "p.value"]
        
        test_data <- imp_data[[1]]
        test_model <- tryCatch({
          lm_robust(formula_mi_imp, data = test_data, clusters = country, se_type = 'stata')
        }, error = function(e) {
          NULL
        })
        
        nobs_mi <- ifelse(!is.null(test_model), nobs(test_model), NA)
        
        model_results_tab_A14 <- rbind(model_results_tab_A14, data.frame(
          democracy_type = d_type,
          model_number = paste0("M", i+2),
          effect_name = var_of_interest,
          control_set = control_sets[i],
          control_variable = add_control,
          estimate = est,
          std_error = se,
          p_value = pval,
          nobs = nobs_mi,
          signif_0_05 = ifelse(!is.na(pval), pval < 0.05, FALSE),
          signif_0_10 = ifelse(!is.na(pval), pval < 0.10, FALSE),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# Save raw results
write.csv(model_results_tab_A14, file.path(TABLE_PATH, "table_A14.csv"), row.names = FALSE)