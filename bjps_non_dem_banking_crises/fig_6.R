# Figure 6

# Initialize a custom results dataframe
model_results_protest <- data.frame(
  dc_pf_status = logical(),  # Added field to track DC/PF status
  democracy_filter = character(),
  dependent_variable = character(),
  control_variable_set = character(),
  extension_type = character(),
  estimate = numeric(),
  std_error = numeric(),
  p_value = numeric(),
  nobs = integer(),
  signif_0_05 = logical(),
  signif_0_10 = logical(),
  stringsAsFactors = FALSE
)

# Define democracy filters
democracy_filters <- list(
  "BMR" = expr(bmr_democracy == 0),
  "V-Dem" = expr(v2x_polyarchy <= 0.5),
  "Polity" = expr(polity2 < 7)
)

# Define count dependent variables
count_vars <- c(
  "Protest_DRS_Count_CNTS",
  "Protest_Count_MM"
)

# Define control variable formulations and their model numbers
control_var_sets <- list(
  "M1" = list(name = "Base", var = NULL),
  "M2" = list(name = "ER Repression", var = "CL_cv"),
  "M3" = list(name = "PIR Repression", var = "inv_hrp_farris")
)

# Define bailout extension types
extensions <- c("1")  # Using 1-year extension

# Loop through DC/PF status (true/false)
for (is_dc_pf in c(TRUE, FALSE)) {
  
  # Begin loops for democracy filters
  for (democracy_filter_name in names(democracy_filters)) {
    democracy_filter_expr <- democracy_filters[[democracy_filter_name]]
    
    # Filter data according to the democracy filter
    model_data <- data %>%
      filter(!!democracy_filter_expr) %>%
      # Apply DC/PF filter directly here
      filter(dc_pf == as.numeric(is_dc_pf)) %>%
      # Create crisis variables
      mutate(
        crisis1_bin = ifelse(index_canon_no_1_yr != 0, 1, 0),
        crisis3_bin = ifelse(index_canon_no_3_yr != 0, 1, 0),
        crisis0_bin = ifelse(index_canon_no_0_yr != 0, 1, 0)
      )
    
    # Now, loop over dependent variables
    for (dep_var in count_vars) {
      
      # Update lagged dependent variable
      model_data_dep <- model_data %>%
        group_by(country) %>%
        arrange(year) %>%
        mutate(
          lag_dep_var = lag(.data[[dep_var]])
        ) %>%
        ungroup()
      
      # Loop over control variable sets
      for (model_name in names(control_var_sets)) {
        control_info <- control_var_sets[[model_name]]
        control_label <- control_info$name
        control_var <- control_info$var
        
        # Loop over extension types
        for (extension in extensions) {
          
          # Create crisis_bin and index_canon_no variable names
          crisis_bin_var <- paste0("crisis", extension, "_bin")
          index_canon_no_var <- paste0("index_canon_no_", extension, "_yr")
          
          # Filter model_data_dep to match the current extension
          model_data_filtered <- model_data_dep %>%
            filter(!is.na(.data[[index_canon_no_var]]), .data[[index_canon_no_var]] != 0)
          
          # Remove rows with missing values in variables used in the model
          vars_to_check <- c(dep_var, "lag_dep_var", crisis_bin_var, index_canon_no_var, "polity2", "e_gdppc", "growth", "yr_counter", "yr_counter_sq", "yr_counter_cub", "country")
          if (!is.null(control_var)) {
            vars_to_check <- c(vars_to_check, control_var)
          }
          
          model_data_filtered <- model_data_filtered %>%
            filter_at(vars(vars_to_check), all_vars(!is.na(.)))
          
          # Check if there are enough observations to run the model
          if (nrow(model_data_filtered) < 10) {
            # Skip this iteration if not enough data
            next
          }
          
          # Build the formula for the model
          formula_terms <- c(
            "lag_dep_var",
            index_canon_no_var,
            "polity2",
            "e_gdppc",
            "growth",
            "yr_counter",
            "yr_counter_sq",
            "yr_counter_cub"
          )
          
          if (!is.null(control_var)) {
            formula_terms <- c(formula_terms, control_var)
          }
          
          formula_str <- paste(dep_var, "~", paste(formula_terms, collapse = " + "), "+ factor(country)")
          
          formula <- as.formula(formula_str)
          
          # Fit the model and handle potential errors
          model_fit <- tryCatch({
            protest_model <- glm.nb(
              formula = formula,
              data = model_data_filtered
            )
            
            # Extract the estimate, std error, p-value for index_canon_no_var
            coef_summary <- summary(protest_model)$coefficients
            
            # Check if index_canon_no_var is in the model
            if (index_canon_no_var %in% rownames(coef_summary)) {
              estimate <- coef_summary[index_canon_no_var, "Estimate"]
              std_error <- coef_summary[index_canon_no_var, "Std. Error"]
              p_value <- coef_summary[index_canon_no_var, "Pr(>|z|)"]
              signif_0_05 <- p_value < 0.05
              signif_0_10 <- p_value < 0.10
            } else {
              estimate <- NA
              std_error <- NA
              p_value <- NA
              signif_0_05 <- NA
              signif_0_10 <- NA
            }
            
            n_obs <- nobs(protest_model)
            
            list(
              estimate = estimate,
              std_error = std_error,
              p_value = p_value,
              nobs = n_obs,
              signif_0_05 = signif_0_05,
              signif_0_10 = signif_0_10
            )
          }, error = function(e) {
            # In case of error, return NA values
            message(paste("Error in model:", democracy_filter_name, dep_var, model_name, "DC/PF:", is_dc_pf, "- Error:", e$message))
            list(
              estimate = NA,
              std_error = NA,
              p_value = NA,
              nobs = NA,
              signif_0_05 = NA,
              signif_0_10 = NA
            )
          })
          
          # Now, store the results
          model_results_protest <- rbind(
            model_results_protest,
            data.frame(
              dc_pf_status = is_dc_pf,
              democracy_filter = democracy_filter_name,
              dependent_variable = dep_var,
              control_variable_set = model_name,  # Store model name (M1, M2, M3)
              extension_type = extension,
              estimate = model_fit$estimate,
              std_error = model_fit$std_error,
              p_value = model_fit$p_value,
              nobs = model_fit$nobs,
              signif_0_05 = model_fit$signif_0_05,
              signif_0_10 = model_fit$signif_0_10,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }
}

# Firstly we run the protest models

library(dplyr)
library(ggplot2)
library(forcats)

# Start with your data frame
protest_bmr <- model_results_protest %>% filter(democracy_filter=="BMR")

# Recode control_variable_set to more readable labels
protest_bmr <- protest_bmr %>%
  mutate(control_variable_set = case_when(
    control_variable_set == "Base" ~ "Base",
    control_variable_set == "CL_cv" ~ "ER Repression",
    control_variable_set == "inv_hrp_farris" ~ "PIR Repression",
    TRUE ~ control_variable_set
  ))

# Calculate 90% and 95% confidence intervals
protest_bmr <- protest_bmr %>%
  mutate(
    ci_lower_90 = estimate - 1.645 * std_error,
    ci_upper_90 = estimate + 1.645 * std_error,
    ci_lower_95 = estimate - 1.96 * std_error,
    ci_upper_95 = estimate + 1.96 * std_error
  )

# Ensure that dc_pf is a factor and name its levels for clarity
protest_bmr <- protest_bmr %>%
  mutate(dc_pf_status = ifelse(dc_pf_status == TRUE, "dc_pf", "no dc_pf")) %>% 
  mutate(dc_pf_status = factor(dc_pf_status, 
                        levels = c("dc_pf", "no dc_pf"), 
                        labels = c("DC PF", "No DC PF")))

# Filter for only extension_type == 1
protest_bmr_1year <- protest_bmr %>%
  filter(extension_type == 1)

# Re-level the control_variable_set and then reverse so Base is at the top
protest_bmr_1year <- protest_bmr_1year %>%
  mutate(control_variable_set = fct_relevel(control_variable_set, "Base", "ER Repression", "PIR Repression"),
         control_variable_set = fct_rev(control_variable_set))

# Set the order of dependent_variable to have CNTS first and MM second
protest_bmr_1year <- protest_bmr_1year %>%
  mutate(dependent_variable = factor(dependent_variable, 
                                     levels = c("Protest_DRS_Count_CNTS", "Protest_Count_MM")))

# Fix the x-axis limits for the plots (adjust as needed)
x_limits <- c(-5, 7)

# Create directory if it doesn't exist
if(!dir.exists("protest_forest_plots")) {
  dir.create("protest_forest_plots")
}

# Provide a custom named vector for facet labels, ordering CNTS first
facet_labels <- c(
  "Protest_DRS_Count_CNTS" = "Protest Count - CNTS Data",
  "Protest_Count_MM" = "Protest Count - Mass Mobilization Data"
)

# Create one facet-wrapped plot for all dependent variables with extension_type == 1
p_1year <- ggplot(protest_bmr_1year, aes(x = estimate,
                                y = control_variable_set,
                                color = dc_pf_status)) +
  # 95% CI horizontal bars
  geom_errorbarh(aes(xmin = ci_lower_95, xmax = ci_upper_95),
                 position = position_dodge(width = 0.5),
                 height = 0.01,
                 size = 0.6,
                 alpha = 0.8) +
  # 90% CI horizontal bars (thicker for emphasis)
  geom_errorbarh(aes(xmin = ci_lower_90, xmax = ci_upper_90),
                 position = position_dodge(width = 0.5),
                 height = 0.01,
                 size = 1.2,
                 alpha = 0.8) +
  # Point estimates
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("DC PF" = "blue", "No DC PF" = "red")) +
  facet_wrap(
    ~ dependent_variable,
    labeller = labeller(dependent_variable = facet_labels),
    nrow = 1  # Arrange facets in a single row
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Coefficient Estimate (90% & 95% CI)",
    y = "",  # no y-axis label
    color = NULL
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # Add boxes around each facet
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.7),
    panel.spacing = unit(1, "lines")  # Spacing between facets
  ) +
  coord_cartesian(xlim = x_limits, clip = "off")

# Save the facet-wrapped plot
ggsave(
  filename = paste0(FIGURE_PATH, "/forestplot_1year_facet.png"),
  plot = p_1year,
  width = 12,  # Increased width to accommodate reversed facets
  height = 6,
  dpi = 300,
  bg = "white"
)

cat(paste("Written figure 6 to", FIGURE_PATH))

