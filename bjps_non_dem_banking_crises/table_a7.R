# table_A7.R - Generate Table A7 from supplementary material
# DC/PF Effect on Bailout Policy Index - Middle Class Interests

# Initialize results storage
model_results_tab_A7 <- model_results_empty

# For Table A7, we need to test models with middle class variables
democracy_types <- c("BMR", "V-Dem", "POLITY")
middle_class_vars <- c("urbanization", "lhc_MF_25_64") # Urbanization and tertiary education

# Create data dictionary
democracy_data_list <- list(
  "BMR" = list(nonimp = bmr_data, imp = data_imp_bmr),
  "V-Dem" = list(nonimp = v2x_data, imp = data_imp_v2x),
  "POLITY" = list(nonimp = p2_data, imp = data_imp_p2)
)

# Loop through democracy types and middle class variables
for (d_type in democracy_types) {
  nonimp_data <- democracy_data_list[[d_type]]$nonimp
  imp_data <- democracy_data_list[[d_type]]$imp
  
  for (mc_var in middle_class_vars) {
    # First run models with the control variable only
    base_formula <- paste0("index_canon_no ~ polity2 + e_gdppc + FixedER + DC_Mand_Revise2 + ", mc_var)
    
    # Run M1-M4 without interaction
    for (i in 1:2) {
      # Non-imputed models (M1/M2)
      formula_mi <- as.formula(paste0(base_formula, control_sets[i]))
      
      # Special handling for BMR with tertiary education in M2
      if (d_type == "BMR" && mc_var == "lhc_MF_25_64" && i == 2) {
        # Use a modified formula without factor(period dummies) to avoid the error
        # Without this, we run into a contrasts error due to lack of variation between our time period dummies, avoids a singularity
        formula_mi <- as.formula(paste0(base_formula, " + debt + partisan_cons"))
      }
      
      model_mi <- tryCatch({
        lm_robust(formula_mi, data = nonimp_data, clusters = country, se_type = 'stata')
      }, error = function(e) {
        message(paste("Error in M", i, "for", d_type, "with var", mc_var, ":", e))
        NULL
      })
      
      if (!is.null(model_mi)) {
        # Extract results for DC_Mand_Revise2 and middle class variable
        for (var_of_interest in c("DC_Mand_Revise2", mc_var)) {
          est <- coef(summary(model_mi))[var_of_interest, "Estimate"]
          se <- coef(summary(model_mi))[var_of_interest, "Std. Error"]
          pval <- coef(summary(model_mi))[var_of_interest, "Pr(>|t|)"]
          nobs_mi <- nobs(model_mi)
          
          model_results_tab_A7 <- rbind(model_results_tab_A7, data.frame(
            democracy_type = d_type,
            model_number = paste0("M", i),
            effect_name = var_of_interest,
            control_set = control_sets[i],
            control_variable = mc_var,
            interaction = "No",
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
        message(paste("Error in M", i+2, "for", d_type, "with var", mc_var, ":", e))
        NULL
      })
      
      if (!is.null(pooled_mi)) {
        for (var_of_interest in c("DC_Mand_Revise2", mc_var)) {
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
          
          model_results_tab_A7 <- rbind(model_results_tab_A7, data.frame(
            democracy_type = d_type,
            model_number = paste0("M", i+2),
            effect_name = var_of_interest,
            control_set = control_sets[i],
            control_variable = mc_var,
            interaction = "No",
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
    
    # Then run models with the interaction term
    int_formula <- paste0("index_canon_no ~ polity2 + e_gdppc + FixedER + DC_Mand_Revise2 + ", 
                          mc_var, " + DC_Mand_Revise2:", mc_var)
    
    # Run M1-M4 with interaction
    for (i in 1:2) {
      # Non-imputed models (M1/M2)
      formula_int_mi <- as.formula(paste0(int_formula, control_sets[i]))
      
      # Special handling for BMR with tertiary education in M2
      if (d_type == "BMR" && mc_var == "lhc_MF_25_64" && i == 2) {
        # Use a modified formula without factor() to avoid the error
        # Without this, we run into a contrasts error due to lack of variation between our time period dummies, avoids a singularity
        formula_int_mi <- as.formula(paste0(int_formula, " + debt + partisan_cons"))
      }
      
      model_int_mi <- tryCatch({
        lm_robust(formula_int_mi, data = nonimp_data, clusters = country, se_type = 'stata')
      }, error = function(e) {
        message(paste("Error in M", i, "(interaction) for", d_type, "with var", mc_var, ":", e))
        NULL
      })
      
      if (!is.null(model_int_mi)) {
        # Extract results for DC_Mand_Revise2, middle class variable, and interaction
        int_term <- paste0("DC_Mand_Revise2:", mc_var)
        for (var_of_interest in c("DC_Mand_Revise2", mc_var, int_term)) {
          est <- coef(summary(model_int_mi))[var_of_interest, "Estimate"]
          se <- coef(summary(model_int_mi))[var_of_interest, "Std. Error"]
          pval <- coef(summary(model_int_mi))[var_of_interest, "Pr(>|t|)"]
          nobs_mi <- nobs(model_int_mi)
          
          model_results_tab_A7 <- rbind(model_results_tab_A7, data.frame(
            democracy_type = d_type,
            model_number = paste0("M", i),
            effect_name = var_of_interest,
            control_set = control_sets[i],
            control_variable = mc_var,
            interaction = "Yes",
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
      formula_int_mi_imp <- as.formula(paste0(int_formula, control_sets[i]))
      pooled_int_mi <- tryCatch({
        combine3(formula_int_mi_imp, imp_data)
      }, error = function(e) {
        message(paste("Error in M", i+2, "(interaction) for", d_type, "with var", mc_var, ":", e))
        NULL
      })
      
      if (!is.null(pooled_int_mi)) {
        int_term <- paste0("DC_Mand_Revise2:", mc_var)
        for (var_of_interest in c("DC_Mand_Revise2", mc_var, int_term)) {
          est <- pooled_int_mi[pooled_int_mi$term == var_of_interest, "estimate"]
          se <- pooled_int_mi[pooled_int_mi$term == var_of_interest, "std.error"]
          pval <- pooled_int_mi[pooled_int_mi$term == var_of_interest, "p.value"]
          
          test_data <- imp_data[[1]]
          test_model <- tryCatch({
            lm_robust(formula_int_mi_imp, data = test_data, clusters = country, se_type = 'stata')
          }, error = function(e) {
            NULL
          })
          
          nobs_mi <- ifelse(!is.null(test_model), nobs(test_model), NA)
          
          model_results_tab_A7 <- rbind(model_results_tab_A7, data.frame(
            democracy_type = d_type,
            model_number = paste0("M", i+2),
            effect_name = var_of_interest,
            control_set = control_sets[i],
            control_variable = mc_var,
            interaction = "Yes",
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
write.csv(model_results_tab_A7, file.path(TABLE_PATH, "table_A7.csv"), row.names = FALSE)

