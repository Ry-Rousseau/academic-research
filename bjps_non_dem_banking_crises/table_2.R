# table_2.R - Generate Table 2 from the manuscript
# Effect of Mandatory DC/PF on Bailout Policy Index - BMR Autocracy Sample

# Initialize results storage for Table 2 
model_results_tab_2 <- model_results_empty

# For Table 2, we're only using the BMR democracy measure
democracy_types <- c("BMR")

# No additional control variables for Table 2
control_vars <- c("")

# Create a dictionary-like list for data access
democracy_data_list <- list(
  "BMR" = list(
    nonimp = bmr_data,
    imp = data_imp_bmr
  )
)

# Loop through democracy types
for (d_type in democracy_types) {
  
  # Extract data subsets
  nonimp_data <- democracy_data_list[[d_type]]$nonimp
  imp_data <- democracy_data_list[[d_type]]$imp
  
  # Loop through control variables (just an empty string for Table 2)
  for (control_var in control_vars) {
    
    # Construct formula parts shared by all models
    base_formula <- paste0("index_canon_no ~ polity2 + e_gdppc + FixedER + DC_Mand_Revise2", control_var)
    
    # ================
    # M1: Non-imputed
    # ================
    formula_m1 <- as.formula(paste0(base_formula, control_sets[1]))
    model_m1 <- tryCatch(
      {
        lm_robust(formula_m1, data = nonimp_data, clusters = country, se_type = 'stata')
      },
      error = function(e) {
        message(paste("Error in M1 for", d_type, "with var", control_var, ":", e))
        NULL  # Return NULL to signal model failed
      }
    )
    
    if (!is.null(model_m1)) {
      # For Table 2, we only care about DC_Mand_Revise2
      var_of_interest <- "DC_Mand_Revise2"
      est <- coef(summary(model_m1))[var_of_interest, "Estimate"]
      se <- coef(summary(model_m1))[var_of_interest, "Std. Error"]
      pval <- coef(summary(model_m1))[var_of_interest, "Pr(>|t|)"]
      nobs_m1 <- nobs(model_m1)
      
      model_results_tab_2 <- rbind(model_results_tab_2, data.frame(
        democracy_type = d_type,
        model_number = "M1",
        effect_name = var_of_interest,
        control_set = control_sets[1],
        control_variable = control_var,
        estimate = est,
        std_error = se,
        p_value = pval,
        nobs = nobs_m1,
        signif_0_05 = pval < 0.05,
        signif_0_10 = pval < 0.10,
        stringsAsFactors = FALSE
      ))
    } else {
      # If there's an error, append a row with NA
      var_of_interest <- "DC_Mand_Revise2"
      model_results_tab_2 <- rbind(model_results_tab_2, data.frame(
        democracy_type = d_type,
        model_number = "M1",
        effect_name = var_of_interest,
        control_set = control_sets[1],
        control_variable = control_var,
        estimate = NA,
        std_error = NA,
        p_value = NA,
        nobs = NA,
        signif_0_05 = FALSE,
        signif_0_10 = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # ================
    # M2: Non-imputed
    # ================
    formula_m2 <- as.formula(paste0(base_formula, control_sets[2]))
    model_m2 <- tryCatch(
      {
        lm_robust(formula_m2, data = nonimp_data, clusters = country, se_type = 'stata')
      },
      error = function(e) {
        message(paste("Error in M2 for", d_type, "with var", control_var, ":", e))
        NULL
      }
    )
    
    if (!is.null(model_m2)) {
      var_of_interest <- "DC_Mand_Revise2"
      est <- coef(summary(model_m2))[var_of_interest, "Estimate"]
      se <- coef(summary(model_m2))[var_of_interest, "Std. Error"]
      pval <- coef(summary(model_m2))[var_of_interest, "Pr(>|t|)"]
      nobs_m2 <- nobs(model_m2)
      
      model_results_tab_2 <- rbind(model_results_tab_2, data.frame(
        democracy_type = d_type,
        model_number = "M2",
        effect_name = var_of_interest,
        control_set = control_sets[2],
        control_variable = control_var,
        estimate = est,
        std_error = se,
        p_value = pval,
        nobs = nobs_m2,
        signif_0_05 = pval < 0.05,
        signif_0_10 = pval < 0.10,
        stringsAsFactors = FALSE
      ))
    } else {
      var_of_interest <- "DC_Mand_Revise2"
      model_results_tab_2 <- rbind(model_results_tab_2, data.frame(
        democracy_type = d_type,
        model_number = "M2",
        effect_name = var_of_interest,
        control_set = control_sets[2],
        control_variable = control_var,
        estimate = NA,
        std_error = NA,
        p_value = NA,
        nobs = NA,
        signif_0_05 = FALSE,
        signif_0_10 = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # ================
    # M3: Imputed
    # ================
    formula_m3 <- as.formula(paste0(base_formula, control_sets[1]))
    pooled_m3 <- tryCatch(
      {
        combine3(formula_m3, imp_data)
      },
      error = function(e) {
        message(paste("Error in M3 for", d_type, "with var", control_var, ":", e))
        NULL
      }
    )
    
    if (!is.null(pooled_m3)) {
      var_of_interest <- "DC_Mand_Revise2"
      est <- pooled_m3[pooled_m3$term == var_of_interest, "estimate"]
      se <- pooled_m3[pooled_m3$term == var_of_interest, "std.error"]
      pval <- pooled_m3[pooled_m3$term == var_of_interest, "p.value"]
      
      # get nobs from a single imputed dataset
      test_data <- imp_data[[1]]
      test_model_m3 <- tryCatch(
        {
          lm_robust(formula_m3, data = test_data, clusters = country, se_type = 'stata')
        },
        error = function(e) {
          message(paste("Error in single-imputation run (M3) for", d_type, "with var", control_var, ":", e))
          NULL
        }
      )
      
      nobs_m3 <- ifelse(!is.null(test_model_m3), nobs(test_model_m3), NA)
      
      model_results_tab_2 <- rbind(model_results_tab_2, data.frame(
        democracy_type = d_type,
        model_number = "M3",
        effect_name = var_of_interest,
        control_set = control_sets[1],
        control_variable = control_var,
        estimate = est,
        std_error = se,
        p_value = pval,
        nobs = nobs_m3,
        signif_0_05 = ifelse(!is.na(pval), pval < 0.05, FALSE),
        signif_0_10 = ifelse(!is.na(pval), pval < 0.10, FALSE),
        stringsAsFactors = FALSE
      ))
    } else {
      # If pooled_m3 is NULL, store an NA row
      var_of_interest <- "DC_Mand_Revise2"
      model_results_tab_2 <- rbind(model_results_tab_2, data.frame(
        democracy_type = d_type,
        model_number = "M3",
        effect_name = var_of_interest,
        control_set = control_sets[1],
        control_variable = control_var,
        estimate = NA,
        std_error = NA,
        p_value = NA,
        nobs = NA,
        signif_0_05 = FALSE,
        signif_0_10 = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # ================
    # M4: Imputed
    # ================
    formula_m4 <- as.formula(paste0(base_formula, control_sets[2]))
    pooled_m4 <- tryCatch(
      {
        combine3(formula_m4, imp_data)
      },
      error = function(e) {
        message(paste("Error in M4 for", d_type, "with var", control_var, ":", e))
        NULL
      }
    )
    
    if (!is.null(pooled_m4)) {
      var_of_interest <- "DC_Mand_Revise2"
      est <- pooled_m4[pooled_m4$term == var_of_interest, "estimate"]
      se <- pooled_m4[pooled_m4$term == var_of_interest, "std.error"]
      pval <- pooled_m4[pooled_m4$term == var_of_interest, "p.value"]
      
      test_data <- imp_data[[1]]
      test_model_m4 <- tryCatch(
        {
          lm_robust(formula_m4, data = test_data, clusters = country, se_type = 'stata')
        },
        error = function(e) {
          message(paste("Error in single-imputation run (M4) for", d_type, "with var", control_var, ":", e))
          NULL
        }
      )
      
      nobs_m4 <- ifelse(!is.null(test_model_m4), nobs(test_model_m4), NA)
      
      model_results_tab_2 <- rbind(model_results_tab_2, data.frame(
        democracy_type = d_type,
        model_number = "M4",
        effect_name = var_of_interest,
        control_set = control_sets[2],
        control_variable = control_var,
        estimate = est,
        std_error = se,
        p_value = pval,
        nobs = nobs_m4,
        signif_0_05 = ifelse(!is.na(pval), pval < 0.05, FALSE),
        signif_0_10 = ifelse(!is.na(pval), pval < 0.10, FALSE),
        stringsAsFactors = FALSE
      ))
    } else {
      # If pooled_m4 is NULL, store an NA row
      var_of_interest <- "DC_Mand_Revise2"
      model_results_tab_2 <- rbind(model_results_tab_2, data.frame(
        democracy_type = d_type,
        model_number = "M4",
        effect_name = var_of_interest,
        control_set = control_sets[2],
        control_variable = control_var,
        estimate = NA,
        std_error = NA,
        p_value = NA,
        nobs = NA,
        signif_0_05 = FALSE,
        signif_0_10 = FALSE,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# ================
# Format results for Table 2
# ================

# Create formatted table with coefficients and standard errors in parentheses
format_table_2 <- model_results_tab_2 %>%
  mutate(
    formatted_coef = sprintf("%.3f%s", 
                             estimate,
                             ifelse(signif_0_05, "***", 
                                    ifelse(signif_0_10, "**", ""))),
    formatted_se = sprintf("(%.3f)", std_error)
  ) %>%
  dplyr::select(model_number, formatted_coef, formatted_se, nobs) %>%
  arrange(model_number)

# Print the results
print(format_table_2)

# Save the results as CSV
write.csv(format_table_2, file.path(TABLE_PATH, "table_2.csv"), row.names = FALSE)

# Create a formatted text version of the table for the paper
text_table <- "Table 2: Effect on Bailout Policy Index\n\n"
text_table <- paste0(text_table, "                    M1        M2        M3        M4\n")
text_table <- paste0(text_table, "------------------------------------------------------\n")

# Add DC_Mand_Revise2 coefficient row
dc_coef_row <- model_results_tab_2 %>%
  arrange(model_number) %>%
  summarize(
    coef_line = paste0(
      "Mandatory DC/PF  ", 
      paste(sprintf("%.3f%s", 
                    estimate,
                    ifelse(signif_0_05, "***", 
                           ifelse(signif_0_10, "**", ""))), 
            collapse = "    ")
    )
  ) %>%
  pull(coef_line)

# Add standard error row
dc_se_row <- model_results_tab_2 %>%
  arrange(model_number) %>%
  summarize(
    se_line = paste0(
      "                 ", 
      paste(sprintf("(%.3f)", std_error), collapse = "  ")
    )
  ) %>%
  pull(se_line)

# Add number of observations row
nobs_row <- model_results_tab_2 %>%
  arrange(model_number) %>%
  summarize(
    nobs_line = paste0(
      "Num.Obs.         ", 
      paste(sprintf("%d", nobs), collapse = "      ")
    )
  ) %>%
  pull(nobs_line)

# Combine rows into table
text_table <- paste0(text_table, dc_coef_row, "\n", dc_se_row, "\n\n", nobs_row, "\n\n")
text_table <- paste0(text_table, "------------------------------------------------------\n")
text_table <- paste0(text_table, "Notes: * p < 0.05, ** p < 0.01, *** p < 0.001.\n")
text_table <- paste0(text_table, "All models include controls for degree of democracy, exchange rate regime, and GDP per capita.\n")
text_table <- paste0(text_table, "Full controls are public debt / GDP and partisanship.\n")

# Save the text table
writeLines(text_table, file.path(TABLE_PATH, "table_2.txt"))

# Create a formatted stargazer table
# Create list of models to include in stargazer
models_list <- list()
if(!is.null(model_m1)) models_list[["M1"]] <- model_m1
if(!is.null(model_m2)) models_list[["M2"]] <- model_m2

# Add a note about M3 and M4 being from imputed data
stargazer_note <- paste0(
  "Models M1 and M2 use complete case data. ",
  "Models M3 and M4 use multiple imputation with ", 
  length(imp_data), " imputed datasets. ",
  "DC_Mand_Revise2 coefficients for M3: ", 
  sprintf("%.3f", model_results_tab_2$estimate[model_results_tab_2$model_number == "M3"]),
  " (", sprintf("%.3f", model_results_tab_2$std_error[model_results_tab_2$model_number == "M3"]), "). ",
  "For M4: ", 
  sprintf("%.3f", model_results_tab_2$estimate[model_results_tab_2$model_number == "M4"]),
  " (", sprintf("%.3f", model_results_tab_2$std_error[model_results_tab_2$model_number == "M4"]), ")."
)

# Generate and save the stargazer output if we have at least one model
if(length(models_list) > 0) {
  stargazer_output <- stargazer(
    models_list,
    title = "Table 2: Effect on Bailout Policy Index",
    dep.var.labels = "Bailout Policy Index",
    covariate.labels = c("Mandatory DC/PF", "Polity2", "GDP per capita", "Fixed Exchange Rate"),
    add.lines = list(c("Period Fixed Effects", "No", "Yes", "No", "Yes"),
                     c("Full Controls", "No", "Yes", "No", "Yes")),
    model.names = FALSE,
    column.labels = c("M1", "M2"),
    notes = stargazer_note,
    notes.append = TRUE,
    type = "text",
    out = file.path(TABLE_PATH, "table_2_stargazer.txt")
  )
}