# table_A13.R - Generate Table A13 from supplementary material
# DC/PF Effect on Bailout Policy Index - Polity & V-Dem Samples

# Initialize results storage
model_results_tab_A13 <- model_results_empty

# For Table A13, we're using Polity and V-Dem democracy measures
democracy_types <- c("POLITY", "V-Dem")

# Create data dictionary
democracy_data_list <- list(
  "POLITY" = list(nonimp = p2_data, imp = data_imp_p2),
  "V-Dem" = list(nonimp = v2x_data, imp = data_imp_v2x)
)

# Base formula for all models
base_formula <- "index_canon_no ~ polity2 + e_gdppc + FixedER + DC_Mand_Revise2"

# Loop through democracy types
for (d_type in democracy_types) {
  nonimp_data <- democracy_data_list[[d_type]]$nonimp
  imp_data <- democracy_data_list[[d_type]]$imp
  
  # Run M1-M4
  for (i in 1:2) {
    # Non-imputed models (M1/M2)
    formula_mi <- as.formula(paste0(base_formula, control_sets[i]))
    model_mi <- tryCatch({
      lm_robust(formula_mi, data = nonimp_data, clusters = country, se_type = 'stata')
    }, error = function(e) {
      message(paste("Error in M", i, "for", d_type, ":", e))
      NULL
    })
    
    if (!is.null(model_mi)) {
      # Extract results for DC_Mand_Revise2
      var_of_interest <- "DC_Mand_Revise2"
      est <- coef(summary(model_mi))[var_of_interest, "Estimate"]
      se <- coef(summary(model_mi))[var_of_interest, "Std. Error"]
      pval <- coef(summary(model_mi))[var_of_interest, "Pr(>|t|)"]
      nobs_mi <- nobs(model_mi)
      
      model_results_tab_A13 <- rbind(model_results_tab_A13, data.frame(
        democracy_type = d_type,
        model_number = paste0("M", i),
        effect_name = var_of_interest,
        control_set = control_sets[i],
        control_variable = "None",
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
      message(paste("Error in M", i+2, "for", d_type, ":", e))
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
      
      model_results_tab_A13 <- rbind(model_results_tab_A13, data.frame(
        democracy_type = d_type,
        model_number = paste0("M", i+2),
        effect_name = var_of_interest,
        control_set = control_sets[i],
        control_variable = "None",
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

# Save raw results
write.csv(model_results_tab_A13, file.path(TABLE_PATH, "table_A13.csv"), row.names = FALSE)