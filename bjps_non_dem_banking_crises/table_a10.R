# table_A10.R - Generate Table A10 from supplementary material
# DC/PF Effect on Bailout Policy Index - Autocratic Regime Characteristics 2

# Initialize results storage
model_results_tab_A10 <- model_results_empty

# For Table A10, we need to control for regime type variables
democracy_types <- c("BMR", "V-Dem", "POLITY")
regime_vars <- c(
  "bankloan_firm_gdp", # Bank Loans to Firms
  "assets_JST_dba", # Bank Assets
  "bankloan_total_gdp", # Bank Loans
  "v2x_regime_closed",      # Party-based regime
  "v2x_regime_electoral" # Electoral Regime
)

# Create data dictionary
democracy_data_list <- list(
  "BMR" = list(nonimp = bmr_data, imp = data_imp_bmr),
  "V-Dem" = list(nonimp = v2x_data, imp = data_imp_v2x),
  "POLITY" = list(nonimp = p2_data, imp = data_imp_p2)
)

# Loop through democracy types and regime variables
for (d_type in democracy_types) {
  nonimp_data <- democracy_data_list[[d_type]]$nonimp
  imp_data <- democracy_data_list[[d_type]]$imp
  
  for (regime_var in regime_vars) {
    # Base formula with regime variable as control
    base_formula <- paste0("index_canon_no ~ polity2 + e_gdppc + FixedER + DC_Mand_Revise2 + ", regime_var)
    
    # Run M1-M4
    for (i in 1:2) {
      # Non-imputed models (M1/M2)
      formula_mi <- as.formula(paste0(base_formula, control_sets[i]))
      model_mi <- tryCatch({
        lm_robust(formula_mi, data = nonimp_data, clusters = country, se_type = 'stata')
      }, error = function(e) {
        message(paste("Error in M", i, "for", d_type, "with var", regime_var, ":", e))
        NULL
      })
      
      if (!is.null(model_mi)) {
        # Extract results for DC_Mand_Revise2 and regime variable
        for (var_of_interest in c("DC_Mand_Revise2", regime_var)) {
          est <- coef(summary(model_mi))[var_of_interest, "Estimate"]
          se <- coef(summary(model_mi))[var_of_interest, "Std. Error"]
          pval <- coef(summary(model_mi))[var_of_interest, "Pr(>|t|)"]
          nobs_mi <- nobs(model_mi)
          
          model_results_tab_A10 <- rbind(model_results_tab_A10, data.frame(
            democracy_type = d_type,
            model_number = paste0("M", i),
            effect_name = var_of_interest,
            control_set = control_sets[i],
            control_variable = regime_var,
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
        message(paste("Error in M", i+2, "for", d_type, "with var", regime_var, ":", e))
        NULL
      })
      
      if (!is.null(pooled_mi)) {
        for (var_of_interest in c("DC_Mand_Revise2", regime_var)) {
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
          
          model_results_tab_A10 <- rbind(model_results_tab_A10, data.frame(
            democracy_type = d_type,
            model_number = paste0("M", i+2),
            effect_name = var_of_interest,
            control_set = control_sets[i],
            control_variable = regime_var,
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

# Save raw results
write.csv(model_results_tab_A10, file.path(TABLE_PATH, "table_A10.csv"), row.names = FALSE)