# table_A9.R - Generate Table A9 from supplementary material
# DC/PF Effect on Bailout Policy Index - Corruption Variables

# Initialize results storage
model_results_tab_A9 <- model_results_empty

# For Table A9, we need to control for corruption variables
democracy_types <- c("BMR", "V-Dem", "POLITY")
corruption_vars <- c(
  "v2xnp_client",   # Clientelism Index
  "v2xnp_regcorr",    # Regime Corruption Index
  "v2x_corr",       # Political Corruption Index
  "v2x_execorr",    # Executive Corruption Index
  "v2x_pubcorr"     # Public Sector Corruption Index
)

# Create data dictionary
democracy_data_list <- list(
  "BMR" = list(nonimp = bmr_data, imp = data_imp_bmr),
  "V-Dem" = list(nonimp = v2x_data, imp = data_imp_v2x),
  "POLITY" = list(nonimp = p2_data, imp = data_imp_p2)
)

# Loop through democracy types and corruption variables
for (d_type in democracy_types) {
  nonimp_data <- democracy_data_list[[d_type]]$nonimp
  imp_data <- democracy_data_list[[d_type]]$imp
  
  for (corr_var in corruption_vars) {
    # Base formula with corruption variable as control
    base_formula <- paste0("index_canon_no ~ polity2 + e_gdppc + FixedER + DC_Mand_Revise2 + ", corr_var)
    
    # Run M1-M4
    for (i in 1:2) {
      # Non-imputed models (M1/M2)
      formula_mi <- as.formula(paste0(base_formula, control_sets[i]))
      model_mi <- tryCatch({
        lm_robust(formula_mi, data = nonimp_data, clusters = country, se_type = 'stata')
      }, error = function(e) {
        message(paste("Error in M", i, "for", d_type, "with var", corr_var, ":", e))
        NULL
      })
      
      if (!is.null(model_mi)) {
        # Extract results for DC_Mand_Revise2 and corruption variable
        for (var_of_interest in c("DC_Mand_Revise2", corr_var)) {
          est <- coef(summary(model_mi))[var_of_interest, "Estimate"]
          se <- coef(summary(model_mi))[var_of_interest, "Std. Error"]
          pval <- coef(summary(model_mi))[var_of_interest, "Pr(>|t|)"]
          nobs_mi <- nobs(model_mi)
          
          model_results_tab_A9 <- rbind(model_results_tab_A9, data.frame(
            democracy_type = d_type,
            model_number = paste0("M", i),
            effect_name = var_of_interest,
            control_set = control_sets[i],
            control_variable = corr_var,
            estimate = est,
            std_error = se,
            p_value = pval,
            nobs = nobs_mi,
            signif_0_05 = pval < 0.05,
            signif_0_10 = pval < 0.10,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Imputed models (M3/M4)
      formula_mi_imp <- as.formula(paste0(base_formula, control_sets[i]))
      pooled_mi <- tryCatch({
        combine3(formula_mi_imp, imp_data)
      }, error = function(e) {
        message(paste("Error in M", i+2, "for", d_type, "with var", corr_var, ":", e))
        NULL
      })
      
      if (!is.null(pooled_mi)) {
        for (var_of_interest in c("DC_Mand_Revise2", corr_var)) {
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
          
          model_results_tab_A9 <- rbind(model_results_tab_A9, data.frame(
            democracy_type = d_type,
            model_number = paste0("M", i+2),
            effect_name = var_of_interest,
            control_set = control_sets[i],
            control_variable = corr_var,
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
}

# Format and save results
write.csv(model_results_tab_A9, file.path(TABLE_PATH, "table_A9.csv"), row.names = FALSE)
