# Define functions ####
combine3<-function (formula, df_list,dof_adjust = TRUE, ...){
  args <- list(...)
  models <- lapply(df_list, function(x) {
    model <- lm_robust(
      formula,
      data = x,
      clusters = country,
      se_type = "stata"
    )
  })
  mods_est <- sapply(models, function(x) summary(x)$coefficients[, 1], simplify = "cbind")
  mods_var <- sapply(models, function(x) summary(x)$coefficients[, 2]^2, simplify = "cbind")
  m <- length(models)
  Q_bar <- (1/m) * rowSums(mods_est)
  U_bar <- (1/m) * rowSums(mods_var)
  models_demean <- apply(mods_est, 2, function(x) (x - Q_bar)^2)
  B <- (1/(m - 1)) * rowSums(models_demean)
  Q_bar_var <- U_bar + (1 + (1/m)) * B
  Q_bar_se <- sqrt(Q_bar_var)
  v_m <- (m - 1) * (1 + (U_bar/((1 + m^-1) * B)))^2
  if (dof_adjust) {
    v_complete <- models[[1]]$df.residual
    gamma <- ((1 + m^-1) * B)/Q_bar_var
    v_obs <- ((v_complete + 1)/(v_complete + 3)) * v_complete *
      (1 - gamma)
    v_corrected <- ((1/v_m) + (1/v_obs))^-1
    dof <- v_corrected
  }
  else {
    dof <- v_m
  }
  est = Q_bar
  std.err = Q_bar_se
  stat = est/std.err
  combined_mat <- data.frame(term = names(est), estimate = est,
                             std.error = std.err, statistic = stat, df = dof, p.value = 2 *
                               stats::pt(abs(stat), dof, lower.tail = FALSE))
  rownames(combined_mat) <- NULL
  return(combined_mat)
}


generate_formulas <- function(variable, control_sets, additional_controls) {
  dv_vars <- "index_canon_no" # Define the dependent variables
  formulas <- expand.grid(Control = control_sets, Additional = additional_controls, DV = dv_vars) %>%
    apply(1, function(x) {
      paste0(x['DV'], " ~ polity2 + e_gdppc + FixedER + ", variable, " ", x['Additional'], " ", x['Control'])
    })
  return(formulas)
}
how_many_imputations_mod<-function (model, variable_of_interest, cv = 0.05, alpha = 0.05) {
  if (!is(model, "mipo")) {
    tryCatch(model <- mice::as.mira(model), error = function(e) {
      stop("model must be a `mira`, or convertible to `mira`.")
    })
    model <- mice::pool(model)
  }
  fmi <- model$pooled$fmi[model$pooled$term == variable_of_interest]
  z <- qnorm(1 - alpha/2)
  fmiu <- plogis(qlogis(fmi) + z * sqrt(2/model$m))
  ceiling(1 + 1/2 * (fmiu/cv)^2)
}
run_models_variable_m <- function(all_formula_pairs, data,random_seed = 89){
  set.seed(random_seed)
  results_list <- list()
  
  for (comment in names(all_formula_pairs)) {
    
    formulas <- all_formula_pairs[[comment]]
    comment_results <- list()
    
    for (formula_str in formulas){
      
      mi_model_fit <- lapply(sample(data, size = 20), 
                             function(x) lm_robust(as.formula(formula_str),
                                                   clusters = country,
                                                   se_type = "stata",
                                                   data = x))
      
      m <- how_many_imputations_mod(mi_model_fit, comment)
      m<-ifelse(m>100,100,ifelse(m<6,5,m))
      tryCatch({
        test_results <- combine3(as.formula(formula_str), sample(data,size=m))
        rownames(test_results)<-test_results[,1]
        coef_value <- test_results[comment, "estimate"]
        std_error <- test_results[comment, "std.error"]
        p_value <- test_results[comment, "p.value"]
      })
      is_significant <- ifelse(p_value < 0.05, "Yes", ifelse(p_value < 0.1, "At 0.1", "No"))
      
      varying_controls <- case_when(grepl('factor',  formula_str) & grepl('kaopen',  formula_str) & grepl('debt',  formula_str) ~ "All (kaopen)",
                                    grepl('factor',  formula_str) & grepl('Quinn',  formula_str) & grepl('debt',  formula_str) ~ "All (Quinn)",
                                    grepl('factor',  formula_str) & grepl('debt',  formula_str) ~ "FEs + debt + partisan_cons",
                                    grepl('Quinn',  formula_str) & grepl('debt',  formula_str) ~ "Quinn + debt + partisan_cons",
                                    grepl('kaopen',  formula_str) & grepl('debt',  formula_str) ~ "kaopen + debt + partisan_cons",
                                    .default = "None")
      
      comment_result <- list("Is Significant" = is_significant, 
                             "Varying Controls" = varying_controls,
                             "std_error" = std_error,
                             "Coefficient" = coef_value, 
                             "p_value" = p_value,
                             "m" = m)
      
      comment_results[[formula_str]] <- comment_result
    }
    results_list[[comment]] <- comment_results
  }
  
  summary_df <- data.frame(Variable = character(),
                           DV = character(),
                           Varying_Controls = character(),
                           Coefficient = numeric(),
                           std_error = numeric(),
                           p_value = numeric(),
                           is_significant = character(),
                           Formula = character(),
                           m = numeric())
  for (comment in names(results_list)) {
    comment_results <- results_list[[comment]]
    
    # Check if any formula had the variable as significant
    for (formula_str in names(comment_results)) {
      is_significant <- comment_results[[formula_str]]$`Is Significant`
      
      # Treat NAs as FALSE
      if (is.na(is_significant)) {
        is_significant <- FALSE
      }
      
      dv <- stringr::str_split(formula_str,"~")[[1]][1] %>% stringr::str_trim()
      print(dv)
      
      new_row <- data.frame(Variable = comment,
                            DV = dv, 
                            Varying_Controls = comment_results[[formula_str]]$`Varying Controls`,
                            Coefficient = comment_results[[formula_str]]$Coefficient,
                            std_error = comment_results[[formula_str]]$std_error,
                            p_value = comment_results[[formula_str]]$p_value,
                            is_significant = is_significant,
                            Formula = formula_str,
                            m = comment_results[[formula_str]]$m)
      summary_df <- rbind(summary_df, new_row)
    }
  }
  return(summary_df)
}


run_models <- function(all_formula_pairs, data, imputed = TRUE) {
  # Initialize an empty list to hold the results
  results_list <- list()
  
  for (comment in names(all_formula_pairs)) {
    formulas <- all_formula_pairs[[comment]]
    comment_results <- list()
    
    for (formula_str in formulas){
      tryCatch({
        if (imputed) {
          test_results <- combine3(as.formula(formula_str), data)
          rownames(test_results)<-test_results[,1]
          coef_value <- test_results[comment, "estimate"]
          std_error <- test_results[comment,'std.error']
          p_value <- test_results[comment, "p.value"]
        } else {
          model <- lm_robust(as.formula(formula_str), data = data, clusters = country, se_type = "stata")
          test_results <- summary(model)
          coef_value <- ifelse(is.infinite(test_results$coefficients[comment, "Estimate"]), NA, test_results$coefficients[comment, "Estimate"])
          std_error <- ifelse(is.infinite(test_results$coefficients[comment, "Std. Error"]), NA, test_results$coefficients[comment, "Std. Error"])
          p_value <- ifelse(is.infinite(test_results$coefficients[comment, "Pr(>|t|)"]),NA,test_results$coefficients[comment, "Pr(>|t|)"])
          n_obs <- nobs(model)
        }
        
        is_significant <- ifelse(p_value < 0.05, "Yes", ifelse(p_value < 0.1, "At 0.1", "No"))
        varying_controls <- case_when(grepl('factor',  formula_str) & grepl('kaopen',  formula_str) & grepl('debt',  formula_str) ~ "All (kaopen)",
                                      grepl('factor',  formula_str) & grepl('Quinn',  formula_str) & grepl('debt',  formula_str) ~ "All (Quinn)",
                                      grepl('factor',  formula_str) & grepl('debt',  formula_str) ~ "FEs + debt + partisan_cons",
                                      grepl('Quinn',  formula_str) & grepl('debt',  formula_str) ~ "Quinn + debt + partisan_cons",
                                      grepl('kaopen',  formula_str) & grepl('debt',  formula_str) ~ "kaopen + debt + partisan_cons",
                                      .default = "None")
        
        comment_result <- list("Is Significant" = is_significant, "Varying Controls" = varying_controls, 
                               "Coefficient" = coef_value, "p_value" = p_value, 'std_error' = std_error)
        if (!imputed) {
          comment_result[["N_Obs"]] <- n_obs
        }
        
        comment_results[[formula_str]] <- comment_result
      }, error = function(e) {
        comment_results[[formula_str]] <- list("Error" = toString(e))
      })
    }
    results_list[[comment]] <- comment_results
  }
  
  # Initialize an empty data frame to hold the summary
  summary_df <-  if (imputed){data.frame(Variable = character(),
                                         DV = character(),
                                         Varying_Controls = character(),
                                         Coefficient = numeric(),
                                         std_error = numeric(),
                                         p_value = numeric(),
                                         is_significant = character(),
                                         Formula = character())}
  else{
    data.frame(Variable = character(),
               DV = character(),
               Varying_Controls = character(),
               Coefficient = numeric(),
               std_error = numeric(),
               p_value = numeric(),
               is_significant = logical(),
               N_Obs = integer(),
               Formula = character())}
  
  for (comment in names(results_list)) {
    comment_results <- results_list[[comment]]
    
    # Check if any formula had the variable as significant
    for (formula_str in names(comment_results)) {
      is_significant <- comment_results[[formula_str]]$`Is Significant`
      
      # Treat NAs as FALSE
      if (is.na(is_significant)) {
        is_significant <- FALSE
      }
      
      
      dv <- stringr::str_split(formula_str,"~")[[1]][1] %>% stringr::str_trim()
      
      new_row <- if(imputed){data.frame(Variable = comment,
                                        DV = dv, 
                                        Varying_Controls = comment_results[[formula_str]]$`Varying Controls`,
                                        Coefficient = comment_results[[formula_str]]$Coefficient,
                                        std_error = comment_results[[formula_str]]$std_error,
                                        p_value = comment_results[[formula_str]]$p_value,
                                        is_significant = is_significant,
                                        Formula = formula_str)}else{
                                          data.frame(Variable = comment,
                                                     DV = dv, 
                                                     Varying_Controls = comment_results[[formula_str]]$`Varying Controls`,
                                                     Coefficient = comment_results[[formula_str]]$Coefficient,
                                                     std_error = comment_results[[formula_str]]$std_error,
                                                     p_value = comment_results[[formula_str]]$p_value,
                                                     is_significant = is_significant,
                                                     N_Obs = comment_results[[formula_str]]$N_Obs,
                                                     Formula = formula_str)
                                        }
      summary_df <- rbind(summary_df, new_row)
    }
  }
  return(summary_df)
}

# Summary statistics function
summary_stats <- function(df, variables, labels){
  
  ss = data.frame(variable=character(),
                  n = numeric(),
                  mean = numeric(),
                  sd =numeric(),
                  min=numeric(),
                  median=numeric(),
                  missing =numeric())
  
  for (variable in variables){
    
    n = sum(!is.na(df[[variable]]))
    mean = mean(df[[variable]],na.rm=T)
    sd = sd(df[[variable]],na.rm=T)
    min = min(df[[variable]],na.rm=T)
    max = max(df[[variable]],na.rm=T)
    median = median(df[[variable]],na.rm=T)
    missing = (nrow(df)-n)/nrow(df)
    
    newrow = data.frame(variable = labels[variables==variable],
                        n=n,mean=mean,sd=sd,min=min,max=max,median=median,missing=missing)
    ss<-rbind(ss,newrow)
  }
  return(ss %>% tibble::remove_rownames())
}

# Summary stats function with error handling
robust_summary_stats <- function(df, variables, labels) {
  ss <- data.frame(
    variable = character(),
    n = numeric(),
    mean = numeric(),
    sd = numeric(),
    min = numeric(),
    max = numeric(),
    median = numeric(),
    missing = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(variables)) {
    variable <- variables[i]
    label <- labels[i]
    
    # Check if variable exists in dataframe
    if (!(variable %in% colnames(df))) {
      cat("Warning: Variable", variable, "not found in dataset\n")
      # Add row with NAs
      newrow <- data.frame(
        variable = label,
        n = 0,
        mean = NA,
        sd = NA,
        min = NA,
        max = NA,
        median = NA,
        missing = 1,
        stringsAsFactors = FALSE
      )
      ss <- rbind(ss, newrow)
      next
    }
    
    # Try to convert to numeric if possible
    if (!is.numeric(df[[variable]])) {
      cat("Warning: Variable", variable, "is not numeric. Attempting conversion...\n")
      tryCatch({
        df[[variable]] <- as.numeric(df[[variable]])
      }, error = function(e) {
        cat("  Conversion failed:", e$message, "\n")
      })
    }
    
    # Calculate stats with robust error handling
    tryCatch({
      n <- sum(!is.na(df[[variable]]))
      if (n > 0 && is.numeric(df[[variable]])) {
        mean_val <- mean(df[[variable]], na.rm = TRUE)
        sd_val <- sd(df[[variable]], na.rm = TRUE)
        min_val <- min(df[[variable]], na.rm = TRUE)
        max_val <- max(df[[variable]], na.rm = TRUE)
        median_val <- median(df[[variable]], na.rm = TRUE)
      } else {
        mean_val <- sd_val <- min_val <- max_val <- median_val <- NA
      }
      missing <- (nrow(df) - n) / nrow(df)
      
      newrow <- data.frame(
        variable = label,
        n = n,
        mean = mean_val,
        sd = sd_val,
        min = min_val,
        max = max_val,
        median = median_val,
        missing = missing,
        stringsAsFactors = FALSE
      )
      ss <- rbind(ss, newrow)
    }, error = function(e) {
      cat("Error processing variable", variable, ":", e$message, "\n")
      newrow <- data.frame(
        variable = label,
        n = NA,
        mean = NA,
        sd = NA,
        min = NA,
        max = NA,
        median = NA,
        missing = NA,
        stringsAsFactors = FALSE
      )
      ss <- rbind(ss, newrow)
    })
  }
  return(ss)
}


# Check if tensorflow is installed and install if needed
check_and_install_tensorflow <- function() {
  tensorflow_available <- tryCatch({
    reticulate::import("tensorflow")
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if (!tensorflow_available) {
    message("TensorFlow not found. Installing...")
    install_result <- tryCatch({
      reticulate::py_install("tensorflow", pip = TRUE)
      TRUE
    }, error = function(e) {
      message("Error installing TensorFlow: ", e$message)
      FALSE
    })
    
    if (install_result) {
      message("TensorFlow successfully installed.")
    } else {
      stop("Failed to install TensorFlow. Please install manually using 'reticulate::py_install(\"tensorflow\")'")
    }
  } else {
    message("TensorFlow is already installed.")
  }
}



