
#--------------------------------------------------------------------------------------
## Helper functions
#--------------------------------------------------------------------------------------

# A simple is_empty function
is_empty <- function(x) {
  length(x) == 0 || is.null(x) || all(is.na(x))
}

# Calculate Root Mean Squared Error (RMSE)
rmse <- function(sim, obs) {
  sqrt(mean((obs - sim)^2, na.rm = TRUE))
}

# Check if required columns exist in a data frame
check_data_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in data:", paste(missing_cols, collapse = ", ")))
  }
}

#--------------------------------------------------------------------------------------
## Fit and compare models
#--------------------------------------------------------------------------------------

# Fit Generalized Linear Models (GLMs) with different families
#
# This function fits Gaussian, Poisson, Quasipoisson, and Negative Binomial GLMs
# and calculates their overdispersion parameters.
#
# @param data A data frame containing recruitment and SSB data.
# @param recruit_col The name of the recruitment column (character string).
# @param ssb_col The name of the SSB column (character string).
# @return A list containing fitted GLM models and their overdispersion values.
fit_glm_models <- function(data, recruit_col = "R", ssb_col = "SSB") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  formula_str <- paste(recruit_col, "~", ssb_col)
  formula_obj <- as.formula(formula_str)
  
  models <- list()
  overdispersion <- list()
  
  # Gaussian GLM
  tryCatch({
    models$gaussian <- glm(formula_obj, data = data, family = gaussian)
    overdispersion$gaussian <- deviance(models$gaussian) / df.residual(models$gaussian)
  }, error = function(e) {
    warning(paste("Gaussian GLM failed:", e$message))
    models$gaussian <- NULL
    overdispersion$gaussian <- NA
  })
  
  # Poisson GLM
  tryCatch({
    models$poisson <- glm(formula_obj, data = data, family = poisson)
    overdispersion$poisson <- deviance(models$poisson) / df.residual(models$poisson)
  }, error = function(e) {
    warning(paste("Poisson GLM failed:", e$message))
    models$poisson <- NULL
    overdispersion$poisson <- NA
  })
  
  # Quasipoisson GLM
  tryCatch({
    models$quasipoisson <- glm(formula_obj, data = data, family = quasipoisson)
    overdispersion$quasipoisson <- deviance(models$quasipoisson) / df.residual(models$quasipoisson)
  }, error = function(e) {
    warning(paste("Quasipoisson GLM failed:", e$message))
    models$quasipoisson <- NULL
    overdispersion$quasipoisson <- NA
  })
  
  # Negative Binomial GLM
  tryCatch({
    models$negbinom <- MASS::glm.nb(formula_obj, data = data)
    overdispersion$negbinom <- deviance(models$negbinom) / df.residual(models$negbinom)
  }, error = function(e) {
    warning(paste("Negative Binomial GLM failed:", e$message))
    models$negbinom <- NULL
    overdispersion$negbinom <- NA
  })
  
  list(models = models, overdispersion = overdispersion)
}

# Main Modeling Functions ----

# Fit an independence (linear) model
#
# @param data A data frame.
# @param recruit_col The name of the recruitment column.
# @param ssb_col The name of the SSB column.
# @return A list containing the fitted linear model and its fitted values.
fit_independence_model <- function(data, recruit_col = "R", ssb_col = "SSB") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  formula_str <- paste(recruit_col, "~", ssb_col)
  model <- lm(as.formula(formula_str), data = data)
  
  list(model = model, fitted = fitted(model))
}

# Fit a Beverton-Holt stock-recruitment model
#
# @param data A data frame.
# @param recruit_col The name of the recruitment column.
# @param ssb_col The name of the SSB column.
# @return A list containing the fitted nls model, its fitted values, and R-squared.
fit_beverton_holt <- function(data, recruit_col = "R", ssb_col = "SSB") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  formula_str_srStarts <- paste(recruit_col, "~", ssb_col)
  formula_obj_srStarts <- as.formula(formula_str_srStarts)
  
  fitted_vals <- rep(NA, nrow(data)) # Initialize with NA
  r2 <- NA # Initialize r2
  
  tryCatch({
    # Suppress warnings/messages from nls convergence issues
    suppressWarnings(suppressMessages({
      sv <- FSA::srStarts(formula_obj_srStarts, data = data, type = "BevertonHolt")
      bh <- FSA::srFuns("BevertonHolt")
      
      log_formula_str <- paste("log(", recruit_col, ") ~ log(bh(", ssb_col, ", a, b))")
      model <- nls(as.formula(log_formula_str), data = data, start = sv)
      
      fitted_vals <- bh(data[[ssb_col]], a = coef(model))
      r2 <- cor(fitted_vals, data[[recruit_col]], use = "pairwise.complete.obs")^2
    }))
    
    list(model = model, fitted = fitted_vals, r2 = r2)
  }, error = function(e) {
    warning(paste("Beverton-Holt model failed:", e$message))
    list(model = NULL, fitted = fitted_vals, r2 = r2)
  })
}

# Fit a Ricker stock-recruitment model
#
# @param data A data frame.
# @param recruit_col The name of the recruitment column.
# @param ssb_col The name of the SSB column.
# @return A list containing the fitted nls model and its fitted values.
fit_ricker <- function(data, recruit_col = "R", ssb_col = "SSB") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  formula_str_srStarts <- paste(recruit_col, "~", ssb_col)
  formula_obj_srStarts <- as.formula(formula_str_srStarts)
  
  fitted_vals <- rep(NA, nrow(data)) # Initialize with NA
  
  tryCatch({
    # Suppress warnings/messages from nls convergence issues
    suppressWarnings(suppressMessages({
      sv <- FSA::srStarts(formula_obj_srStarts, data = data, type = "Ricker")
      rckr <- FSA::srFuns("Ricker")
      
      log_formula_str <- paste("log(", recruit_col, ") ~ log(rckr(", ssb_col, ", a, b))")
      model <- nls(as.formula(log_formula_str), data = data, start = sv)
      
      fitted_vals <- rckr(data[[ssb_col]], a = coef(model))
    }))
    
    list(model = model, fitted = fitted_vals)
  }, error = function(e) {
    warning(paste("Ricker model failed:", e$message))
    list(model = NULL, fitted = fitted_vals)
  })
}

# Fit segmented regression models (linear, log-transformed, and Negative Binomial)
#
# @param data A data frame.
# @param recruit_col The name of the recruitment column.
# @param ssb_col The name of the SSB column.
# @return A list containing fitted segmented models, their fitted values, and breakpoints.
fit_segmented_models <- function(data, recruit_col = "R", ssb_col = "SSB") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  mean_ssb <- mean(data[[ssb_col]], na.rm = TRUE)
  # Create log-transformed columns for internal use within this function
  data_log <- data
  data_log$R_log <- log(data_log[[recruit_col]])
  data_log$sbb_log <- log(data_log[[ssb_col]])
  
  mean_sbb_log <- mean(data_log$sbb_log, na.rm = TRUE)
  
  results <- list()
  
  # Define common formulas
  formula_str <- paste(recruit_col, "~", ssb_col)
  seg_formula_str <- paste("~", ssb_col)
  
  # Regular segmented (lm)
  tryCatch({
    base_model <- lm(as.formula(formula_str), data = data)
    seg_regular <- segmented::segmented(
      base_model,
      seg.Z = as.formula(seg_formula_str),
      psi = mean_ssb
    )
    
    results$regular <- list(
      model = seg_regular,
      fitted = fitted(seg_regular),
      breakpoint = seg_regular$psi[2],
      breakpoint_se = seg_regular$psi[3]
    )
    
  }, error = function(e) {
    warning(paste("Regular segmented model failed:", e$message))
    results$regular <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)
  })
  
  # Log-transformed segmented (lm on log-transformed data)
  tryCatch({
    base_model_log <- lm(R_log ~ sbb_log, data = data_log)
    seg_log <- segmented::segmented(
      base_model_log,
      seg.Z = ~sbb_log,
      psi = mean_sbb_log
    )
    
    results$log <- list(
      model = seg_log,
      fitted = fitted(seg_log),
      breakpoint = seg_log$psi[2],
      breakpoint_se = seg_log$psi[3]
    )
    
  }, error = function(e) {
    warning(paste("Log segmented model failed:", e$message))
    results$log <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)
  })
  
  # Segmented with Negative Binomial GLM (primary attempt)
  # This section contains the fix for the "unused argument" error
  tryCatch({
    # Step 1: Fit a standard glm.nb model to get a robust estimate of theta.
    base_model_negbi_fit_theta <- MASS::glm.nb(as.formula(formula_str), data = data, link = log)
    
    # Step 2: Extract the estimated theta.
    estimated_theta <- base_model_negbi_fit_theta$theta
    
    # Step 3: Fit a standard glm using the negative.binomial family with the fixed theta.
    # This creates a 'glm' object that 'segmented' can handle correctly.
    base_model_negbi_glm <- glm(
      as.formula(formula_str),
      data = data,
      family = MASS::negative.binomial(theta = estimated_theta)
    )
    
    # Step 4: Pass the compatible 'glm' object to segmented.
    seg_negbi <- segmented::segmented(
      obj = base_model_negbi_glm,
      seg.Z = as.formula(seg_formula_str),
      psi = mean_ssb
    )
    
    results$negbinom <- list(
      model = seg_negbi,
      fitted = fitted(seg_negbi),
      breakpoint = seg_negbi$psi[2],
      breakpoint_se = seg_negbi$psi[3],
      note = "Base model: Negative Binomial GLM"
    )
    
  }, error = function(e) {
    warning(paste("Negative Binomial segmented model failed, attempting Quasipoisson fallback:", e$message))
    # Fallback to Quasipoisson GLM for segmented if Negative Binomial fails
    tryCatch({
      base_model_qpois <- glm(as.formula(formula_str), data = data, family = quasipoisson)
      seg_qpois <- segmented::segmented(
        base_model_qpois,
        seg.Z = as.formula(seg_formula_str),
        psi = mean_ssb
      )
      
      results$negbinom_fallback <- list( # Use a different key for fallback
        model = seg_qpois,
        fitted = fitted(seg_qpois),
        breakpoint = seg_qpois$psi[2],
        breakpoint_se = seg_qpois$psi[3],
        note = "Base model: Quasipoisson GLM (Negative Binomial fallback)"
      )
    }, error = function(e2) {
      warning(paste("Quasipoisson segmented fallback also failed:", e2$message))
      results$negbinom_fallback <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)
    })
  })
  
  return(results)
}

# Fit a structural change model with robust breakpoint detection
#
# This function uses `strucchange::breakpoints` to identify and fit a model
# with optimal structural breakpoints using BIC for model selection.
#
# @param data A data frame.
# @param recruit_col The name of the recruitment column.
# @param ssb_col The name of the SSB column.
# @param max_breaks The maximum number of breakpoints to consider (integer).
# @param use_bic Whether to use BIC for optimal breakpoint selection (logical).
# @return A list containing the fitted model, detected breakpoints, and related objects.
fit_strucchange <- function(data, recruit_col = "R", ssb_col = "SSB", max_breaks = 3, use_bic = TRUE) {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  tryCatch({
    formula_str <- paste(recruit_col, "~", ssb_col)
    cat("Fitting breakpoints model with formula:", formula_str, "\n")
    
    # Step 1: Fit initial breakpoints model
    bpts <- strucchange::breakpoints(as.formula(formula_str), data = data)
    
    if (is.null(bpts) || length(bpts$breakpoints) == 0) {
      warning("No breakpoints detected. Fitting simple linear model.")
      model <- lm(as.formula(formula_str), data = data)
      return(list(model = model, breakpoints = NULL, bpts_obj = NULL, fitted = fitted(model), 
                  optimal_breaks = 0, method = "linear_fallback"))
    }
    
    # Step 2: Determine optimal number of breakpoints
    optimal_breaks <- 0
    best_brk <- NULL
    
    if (use_bic) {
      # Method 1: Use BIC for optimal breakpoint selection
      tryCatch({
        cat("Attempting BIC-based breakpoint selection...\n")
        bpts_bic <- strucchange::breakpoints(bpts, ic = "BIC")
        
        if (!is.null(bpts_bic) && !is.na(bpts_bic$breakpoints[1])) {
          optimal_breaks <- length(bpts_bic$breakpoints)
          best_brk <- data[[ssb_col]][bpts_bic$breakpoints]
          cat("BIC selected", optimal_breaks, "breakpoints at positions:", best_brk, "\n")
        } else {
          cat("BIC method returned NA breakpoints, falling back to RSS method\n")
          use_bic <- FALSE  # Fall back to RSS method
        }
      }, error = function(e) {
        cat("BIC method failed:", e$message, "- falling back to RSS method\n")
        use_bic <- FALSE  # Fall back to RSS method
      })
    }
    
    if (!use_bic) {
      # Method 2: Fallback using RSS table analysis
      tryCatch({
        cat("Using RSS-based breakpoint selection...\n")
        
        # Check if RSS table exists and has the expected structure
        if (!is.null(bpts$RSS.table) && is.matrix(bpts$RSS.table)) {
          rss_table <- bpts$RSS.table
          cat("RSS table dimensions:", dim(rss_table), "\n")
          
          # Find the optimal number of breaks (up to max_breaks)
          max_possible_breaks <- min(max_breaks, nrow(rss_table))
          
          if (max_possible_breaks > 0) {
            # Simple heuristic: look for the "elbow" in RSS reduction
            rss_values <- rss_table[1:max_possible_breaks, "RSS"]
            
            # Calculate RSS reduction ratios
            if (length(rss_values) > 1) {
              rss_reductions <- diff(rss_values) / rss_values[-length(rss_values)]
              # Find the point where RSS reduction becomes marginal (< 10%)
              significant_reductions <- which(abs(rss_reductions) > 0.1)
              
              if (length(significant_reductions) > 0) {
                optimal_breaks <- max(significant_reductions)
              } else {
                optimal_breaks <- 1  # Default to 1 break if no significant reductions
              }
            } else {
              optimal_breaks <- 1
            }
            
            # Get breakpoints for the optimal number
            bpts_optimal <- strucchange::breakpoints(bpts, breaks = optimal_breaks)
            if (!is.null(bpts_optimal) && !is.na(bpts_optimal$breakpoints[1])) {
              best_brk <- data[[ssb_col]][bpts_optimal$breakpoints]
              cat("RSS method selected", optimal_breaks, "breakpoints at positions:", best_brk, "\n")
            }
          }
        }
      }, error = function(e) {
        cat("RSS method also failed:", e$message, "\n")
        optimal_breaks <- 0
      })
    }
    
    # Step 3: Fit the final model
    if (optimal_breaks > 0 && !is.null(best_brk)) {
      # Fit segmented model with optimal breakpoints
      bpts_final <- strucchange::breakpoints(bpts, breaks = optimal_breaks)
      
      # Create the model formula with breakfactor
      model_formula <- as.formula(paste(recruit_col, "~", 
                                        paste0("strucchange::breakfactor(bpts_final, breaks = ", optimal_breaks, ")")))
      
      model <- lm(model_formula, data = data)
      fitted_values <- predict(model, newdata = data)
      
      list(model = model, 
           breakpoints = best_brk, 
           bpts_obj = bpts, 
           fitted = fitted_values,
           optimal_breaks = optimal_breaks,
           method = ifelse(use_bic, "BIC", "RSS"))
      
    } else {
      # No breakpoints found or all methods failed - fit simple linear model
      cat("No optimal breakpoints found. Fitting simple linear model.\n")
      model <- lm(as.formula(formula_str), data = data)
      list(model = model, 
           breakpoints = NULL, 
           bpts_obj = NULL, 
           fitted = fitted(model),
           optimal_breaks = 0,
           method = "linear_fallback")
    }
    
  }, error = function(e) {
    warning(paste("Structural change model failed:", e$message))
    # Return a simple linear model as fallback
    formula_str <- paste(recruit_col, "~", ssb_col)
    model <- lm(as.formula(formula_str), data = data)
    list(model = model, 
         breakpoints = NULL, 
         bpts_obj = NULL, 
         fitted = fitted(model),
         optimal_breaks = 0,
         method = "error_fallback")
  })
}

# Model Comparison Function
## Compare fitted models using AIC, RMSE, and R-squared
#
# @param models_list A list of lists, where each inner list contains a model object and fitted values.
# @param observed_data A numeric vector of observed data for RMSE calculation.
# @return A data frame summarizing model comparison metrics.
compare_models <- function(models_list, observed_data) {
  model_names <- names(models_list)
  n_models <- length(models_list)
  
  comparison <- data.frame(
    Model = model_names,
    AIC = numeric(n_models),
    RMSE = numeric(n_models),
    R_squared = numeric(n_models),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(models_list)) {
    model_info <- models_list[[i]]
    
    if (!is.null(model_info) && !is.null(model_info$model) && !is.null(model_info$fitted)) {
      tryCatch({
        # AIC calculation
        comparison$AIC[i] <- AIC(model_info$model)
        
        # RMSE calculation
        comparison$RMSE[i] <- rmse(model_info$fitted, observed_data)
        
        # R-squared calculation based on model type
        if (inherits(model_info$model, "nls")) {
          # For nls models (Beverton-Holt, Ricker), use pre-calculated R2 if available, else calculate
          if (!is.null(model_info$r2)) {
            comparison$R_squared[i] <- model_info$r2
          } else {
            # Calculate from fitted values for Ricker or if r2 wasnt stored for BH
            comparison$R_squared[i] <- cor(model_info$fitted, observed_data, use = "pairwise.complete.obs")^2
          }
        } else if (inherits(model_info$model, "lm") || inherits(model_info$model, "glm")) {
          # For lm and glm, extract from summary
          summary_model <- summary(model_info$model)
          if (!is.null(summary_model$r.squared)) {
            comparison$R_squared[i] <- summary_model$r.squared
          } else if (!is.null(summary_model$adj.r.squared)) {
            comparison$R_squared[i] <- summary_model$adj.r.squared
          } else {
            # Fallback for GLMs that dont report R-squared directly (e.g., some quasi-models)
            comparison$R_squared[i] <- NA
          }
        } else if (inherits(model_info$model, "segmented")) {
          # For segmented models, extract R-squared from the underlying base model
          if (inherits(model_info$model$obj, "lm")) {
            comparison$R_squared[i] <- summary(model_info$model$obj)$r.squared
          } else if (inherits(model_info$model$obj, "glm")) {
            summary_obj <- summary(model_info$model$obj)
            comparison$R_squared[i] <- ifelse(!is.null(summary_obj$r.squared), summary_obj$r.squared,
                                              ifelse(!is.null(summary_obj$adj.r.squared), summary_obj$adj.r.squared, NA)
            )
          } else {
            comparison$R_squared[i] <- NA
          }
        } else {
          comparison$R_squared[i] <- NA
        }
        
      }, error = function(e) {
        warning(paste("Error during comparison for model", model_names[i], ":", e$message))
        comparison$AIC[i] <- NA
        comparison$RMSE[i] <- NA
        comparison$R_squared[i] <- NA
      })
    } else {
      # If model or fitted values are NULL, set all metrics to NA
      comparison$AIC[i] <- NA
      comparison$RMSE[i] <- NA
      comparison$R_squared[i] <- NA
    }
  }
  
  return(comparison)
}

# Main Analysis Workflow ----
# Run a comprehensive Stock-Recruitment Relationship (SRR) analysis
#
# This function orchestrates the fitting of various SRR models and compares them.
# Assumes `data_for_srr` is already prepared with appropriate columns and lags.
#
# @param data_for_srr A data frame containing the recruitment and SSB data.
#   It should already include any necessary lagged SSB columns if required.
# @param recruit_col The name of the recruitment column (e.g., "R", "R_0", "R_1", "R_3").
# @param ssb_col The name of the SSB column (e.g., "SSB", "SSB_lag").
# @param max_breaks The maximum number of breakpoints to consider for structural change model (integer).
# @param use_bic Whether to use BIC for optimal breakpoint selection in structural change model (logical).
# @return A list containing all fitted models, GLM diagnostic results, and a model comparison table.
run_srr_analysis <- function(data_for_srr,
                             recruit_col = "R",
                             ssb_col = "SSB",
                             max_breaks = 3,
                             use_bic = TRUE) {
  
  cat("Starting SRR analysis...\n")
  
  # --- NA Handling and Data Cropping ---
  # Define the variables required for the analysis
  required_vars <- c(recruit_col, ssb_col)
  
  # Check if required columns exist before proceeding
  check_data_columns(data_for_srr, required_vars)
  
  # Find the indices of all rows that have complete data for the required variables
  complete_indices <- which(complete.cases(data_for_srr[, required_vars]))
  
  # Check if any valid data exists
  if (length(complete_indices) == 0) {
    stop(paste("No complete cases found for the required variables", paste(required_vars, collapse=", "), ". The function cannot proceed."))
  }
  
  # Determine the first and last row index of the valid data block
  start_index <- min(complete_indices)
  end_index <- max(complete_indices)
  
  # Crop the dataset to this optimal range
  original_nrow <- nrow(data_for_srr)
  data_for_srr_cleaned <- data_for_srr[start_index:end_index, ]
  
  # Ensure columns are numeric after cropping
  data_for_srr_cleaned[[recruit_col]] <- as.numeric(data_for_srr_cleaned[[recruit_col]])
  data_for_srr_cleaned[[ssb_col]] <- as.numeric(data_for_srr_cleaned[[ssb_col]])
  
  # Report the results of the cropping
  cat("--- Data Cropping ---\n")
  cat("Original dataset had", original_nrow, "rows.\n")
  cat("Data cropped to rows", start_index, "through", end_index, "based on non-NA values in '", recruit_col, "' and '", ssb_col, "'.\n")
  cat("New dataset has", nrow(data_for_srr_cleaned), "observations for analysis.\n")
  cat("---------------------\n\n")
  
  if (nrow(data_for_srr_cleaned) < 10) { # Check for sufficient data after cropping
    stop("Insufficient data after cleaning (less than 10 rows). Cannot perform SRR analysis.")
  }
  
  # Initialize results storage
  models <- list()
  
  # 1. Basic models
  cat("Fitting independence model...\n")
  models$independence <- fit_independence_model(data_for_srr_cleaned, recruit_col, ssb_col)
  
  cat("Fitting Beverton-Holt model...\n")
  models$beverton_holt <- fit_beverton_holt(data_for_srr_cleaned, recruit_col, ssb_col)
  
  cat("Fitting Ricker model...\n")
  models$ricker <- fit_ricker(data_for_srr_cleaned, recruit_col, ssb_col)
  
  # 2. Segmented models
  cat("Fitting segmented models (linear, log, negative binomial/quasipoisson)...\n")
  segmented_results <- fit_segmented_models(data_for_srr_cleaned, recruit_col, ssb_col)
  models$segmented_regular <- segmented_results$regular
  models$segmented_log <- segmented_results$log
  models$segmented_negbinom <- segmented_results$negbinom
  if (!is.null(segmented_results$negbinom_fallback)) {
    models$segmented_negbinom_fallback <- segmented_results$negbinom_fallback
  }
  
  # 3. Structural change model
  cat("Fitting structural change model with automatic breakpoint detection...\n")
  models$strucchange <- fit_strucchange(data_for_srr_cleaned, recruit_col, ssb_col, max_breaks, use_bic)
  
  # 4. GLM model comparison (for diagnostic purposes)
  cat("Fitting base GLM models (Gaussian, Poisson, Quasipoisson, NB) for diagnostics...\n")
  glm_diagnostic_results <- fit_glm_models(data_for_srr_cleaned, recruit_col, ssb_col)
  # Store these GLMs in the main models list for comparison if desired
  models$glm_gaussian <- list(
    model = glm_diagnostic_results$models$gaussian,
    fitted = if (!is.null(glm_diagnostic_results$models$gaussian)) fitted(glm_diagnostic_results$models$gaussian) else NULL
  )
  models$glm_poisson <- list(
    model = glm_diagnostic_results$models$poisson,
    fitted = if (!is.null(glm_diagnostic_results$models$poisson)) fitted(glm_diagnostic_results$models$poisson) else NULL
  )
  models$glm_quasipoisson <- list(
    model = glm_diagnostic_results$models$quasipoisson,
    fitted = if (!is.null(glm_diagnostic_results$models$quasipoisson)) fitted(glm_diagnostic_results$models$quasipoisson) else NULL
  )
  models$glm_negbinom <- list(
    model = glm_diagnostic_results$models$negbinom,
    fitted = if (!is.null(glm_diagnostic_results$models$negbinom)) fitted(glm_diagnostic_results$models$negbinom) else NULL
  )
  
  
  # Prepare list of models for the final comparison table
  models_for_comparison <- list(
    independence = models$independence,
    beverton_holt = models$beverton_holt,
    ricker = models$ricker,
    segmented_regular = models$segmented_regular,
    segmented_log = models$segmented_log,
    segmented_negbinom = models$segmented_negbinom # NB or its QP fallback
  )
  if (!is.null(models$segmented_negbinom_fallback)) {
    models_for_comparison$segmented_negbinom_fallback <- models$segmented_negbinom_fallback
  }
  models_for_comparison$strucchange = models$strucchange
  models_for_comparison$glm_gaussian = models$glm_gaussian
  models_for_comparison$glm_poisson = models$glm_poisson
  models_for_comparison$glm_quasipoisson = models$glm_quasipoisson
  models_for_comparison$glm_negbinom = models$glm_negbinom
  
  
  cat("Comparing all fitted models...\n")
  comparison_table <- compare_models(models_for_comparison, data_for_srr_cleaned[[recruit_col]])
  
  cat("Analysis complete!\n")
  
  return(list(
    all_fitted_models = models, # Contains all model objects and their fitted values
    glm_diagnostic_results = glm_diagnostic_results, # Overdispersion for raw GLMs
    comparison_table = comparison_table
  ))
}


#--------------------------------------------------------------------------------------
## LOOCV
#--------------------------------------------------------------------------------------

cross_valid <- function(dataset) {
  
  cat("Starting cross-validation with", nrow(dataset), "observations...\n")
  
  
  
  # Initialize results data frames
  
  rmse_values <- data.frame("run" = c(1:nrow(dataset)),
                            
                            "linear model" = rep(NA, nrow(dataset)),
                            
                            "beverton"= rep(NA, nrow(dataset)),
                            
                            "ricker"= rep(NA, nrow(dataset)),
                            
                            "segmented"= rep(NA, nrow(dataset)),
                            
                            "segmented log"= rep(NA, nrow(dataset)),
                            
                            "segmented best glm"= rep(NA, nrow(dataset)),
                            
                            "strucchange" = rep(NA, nrow(dataset)))
  
  
  
  rmse_values_train <- data.frame("run" = c(1:nrow(dataset)),
                                  
                                  "linear model" = rep(NA, nrow(dataset)),
                                  
                                  "beverton"= rep(NA, nrow(dataset)),
                                  
                                  "ricker"= rep(NA, nrow(dataset)),
                                  
                                  "segmented"= rep(NA, nrow(dataset)),
                                  
                                  "segmented log"= rep(NA, nrow(dataset)),
                                  
                                  "segmented best glm"= rep(NA, nrow(dataset)),
                                  
                                  "strucchange" = rep(NA, nrow(dataset)))
  
  
  
  # Function to safely compute RMSE
  
  safe_rmse <- function(sim, obs) {
    
    if(any(is.na(sim)) || any(is.na(obs)) || length(sim) != length(obs)) {
      
      return(NA)
      
    }
    
    sqrt(mean((sim - obs)^2, na.rm = TRUE))
    
  }
  
  
  
  for(i in 1:nrow(dataset)){
    
    cat("Processing fold", i, "of", nrow(dataset), "\n")
    
    
    
    train_data <- dataset[-i, ]
    
    test_data <- dataset[i, ]
    
    
    
    if(nrow(train_data) < 10) {
      
      cat("Warning: Insufficient training data for fold", i, ". Skipping to next fold.\n")
      
      next
      
    }
    
    
    
    # 1. Linear model
    
    tryCatch({
      
      m1 <- lm(R ~ SSB, data = train_data, na.action = na.exclude)
      
      fitI <- predict(m1, newdata = test_data)
      
      rmse_values$linear.model[i] <- safe_rmse(fitI, test_data$R)
      
      fitI_train <- predict(m1, newdata = train_data)
      
      rmse_values_train$linear.model[i] <- safe_rmse(fitI_train, train_data$R)
      
    }, error = function(e) {
      
      cat("Linear model failed for fold", i, ":", e$message, "\n")
      
    })
    
    
    
    # 2. Beverton-Holt
    
    tryCatch({
      
      if(any(train_data$R <= 0, na.rm = TRUE) || any(train_data$SSB <= 0, na.rm = TRUE)) {
        
        cat("Warning: Non-positive values detected for Beverton-Holt model in fold", i, ". Skipping.\n")
        
      } else {
        
        svR <- tryCatch(srStarts(R ~ SSB, data = train_data, type = "BevertonHolt"),
                        
                        error = function(e) {
                          
                          cat("srStarts (BevertonHolt) failed for fold", i, ":", e$message, "\n")
                          
                          return(NULL)
                          
                        })
        
        if (!is.null(svR)) {
          
          bh <- srFuns("BevertonHolt")
          
          
          
          srR_beverton <- nls(log(R) ~ log(bh(SSB, a, b)),
                              
                              data = train_data,
                              
                              start = svR,
                              
                              control = nls.control(maxiter = 50, minFactor = 1/2048),
                              
                              na.action = na.exclude)
          
          
          
          pR_beverton <- bh(test_data$SSB, a = coef(srR_beverton))
          
          rmse_values$beverton[i] <- safe_rmse(pR_beverton, test_data$R)
          
          
          
          pR_beverton_train <- bh(train_data$SSB, a = coef(srR_beverton))
          
          rmse_values_train$beverton[i] <- safe_rmse(pR_beverton_train, train_data$R)
          
        }
        
      }
      
    }, error = function(e) {
      
      cat("Beverton-Holt model failed for fold", i, ":", e$message, "\n")
      
    })
    
    
    
    # 3. Ricker
    
    tryCatch({
      
      if(any(train_data$R <= 0, na.rm = TRUE) || any(train_data$SSB <= 0, na.rm = TRUE)) {
        
        cat("Warning: Non-positive values detected for Ricker model in fold", i, ". Skipping.\n")
        
      } else {
        
        svR <- tryCatch(srStarts(R ~ SSB, data = train_data, type = "Ricker"),
                        
                        error = function(e) {
                          
                          cat("srStarts (Ricker) failed for fold", i, ":", e$message, "\n")
                          
                          return(NULL)
                          
                        })
        
        if (!is.null(svR)) {
          
          rckr <- srFuns("Ricker")
          
          
          
          srR_ricker <- nls(log(R) ~ log(rckr(SSB, a, b)),
                            
                            data = train_data,
                            
                            start = svR,
                            
                            control = nls.control(maxiter = 50, minFactor = 1/2048),
                            
                            na.action = na.exclude)
          
          
          
          pR_ricker <- rckr(test_data$SSB, a = coef(srR_ricker))
          
          rmse_values$ricker[i] <- safe_rmse(pR_ricker, test_data$R)
          
          
          
          pR_ricker_train <- rckr(train_data$SSB, a = coef(srR_ricker))
          
          rmse_values_train$ricker[i] <- safe_rmse(pR_ricker_train, train_data$R)
          
        }
        
      }
      
    }, error = function(e) {
      
      cat("Ricker model failed for fold", i, ":", e$message, "\n")
      
    })
    
    
    
    # 4. Segmented regression
    
    tryCatch({
      
      base_lm <- lm(R ~ SSB, data = train_data, na.action = na.exclude)
      
      seg <- segmented::segmented(base_lm,
                                  
                                  seg.Z = ~SSB,
                                  
                                  psi = mean(train_data$SSB, na.rm = TRUE))
      
      
      
      fit_segmented <- predict(seg, newdata = test_data)
      
      rmse_values$segmented[i] <- safe_rmse(fit_segmented, test_data$R)
      
      
      
      fit_segmented_train <- predict(seg, newdata = train_data)
      
      rmse_values_train$segmented[i] <- safe_rmse(fit_segmented_train, train_data$R)
      
    }, error = function(e) {
      
      cat("Segmented model failed for fold", i, ":", e$message, "\n")
      
    })
    
    
    
    # 5. Segmented log
    
    tryCatch({
      
      if("R_log" %in% names(train_data) && "SSB_log" %in% names(train_data) &&
         
         all(is.finite(train_data$R_log)) && all(is.finite(train_data$SSB_log))) {
        
        base_lm_log <- lm(R_log ~ SSB_log, data = train_data, na.action = na.exclude)
        
        seg_log <- segmented::segmented(base_lm_log,
                                        
                                        seg.Z = ~ SSB_log,
                                        
                                        psi = mean(train_data$SSB_log, na.rm = TRUE))
        
        
        
        if("R_log" %in% names(test_data) && "SSB_log" %in% names(test_data)) {
          
          fit_segmented_log <- predict(seg_log, newdata = test_data)
          
          # Back-transform from log scale to original scale
          
          fit_segmented_log_original <- exp(fit_segmented_log)
          
          rmse_values$segmented.log[i] <- safe_rmse(fit_segmented_log_original, test_data$R)
          
        } else {
          
          cat("Warning: Log columns not found in test data for fold", i, ". Cannot predict segmented log model.\n")
          
        }
        
        fit_segmented_log_train <- predict(seg_log, newdata = train_data)
        
        # Back-transform from log scale to original scale
        
        fit_segmented_log_train_original <- exp(fit_segmented_log_train)
        
        rmse_values_train$segmented.log[i] <- safe_rmse(fit_segmented_log_train_original, train_data$R)
        
      } else {
        
        cat("Warning: Log columns not found or contain non-finite values for segmented log model in fold", i, ". Skipping.\n")
        
      }
      
    }, error = function(e) {
      
      cat("Segmented log model failed for fold", i, ":", e$message, "\n")
      
    })
    
    
    
    # 6. Segmented GLM (Negative Binomial with Quasipoisson fallback)
    
    formula_str <- "R ~ SSB"
    
    seg_formula_str <- "~SSB"
    
    mean_ssb <- mean(train_data$SSB, na.rm = TRUE)
    
    
    
    tryCatch({
      
      base_model_negbi_fit_theta <- MASS::glm.nb(as.formula(formula_str), data = train_data, na.action = na.omit)
      
      estimated_theta <- base_model_negbi_fit_theta$theta
      
      
      
      base_model_negbi_glm <- glm(as.formula(formula_str),
                                  
                                  data = train_data,
                                  
                                  family = MASS::negative.binomial(theta = estimated_theta),
                                  
                                  na.action = na.omit)
      
      
      
      seg_negbi <- segmented::segmented(
        
        obj = base_model_negbi_glm,
        
        seg.Z = as.formula(seg_formula_str),
        
        psi = mean_ssb,
        
        control = seg.control(maxit = 200, tol = 1e-5))
      
      
      
      fit_negbi_test <- predict(seg_negbi, newdata = test_data, type = "response")
      
      rmse_values$segmented.best.glm[i] <- safe_rmse(fit_negbi_test, test_data$R)
      
      
      
      fit_negbi_train <- predict(seg_negbi, newdata = train_data, type = "response")
      
      rmse_values_train$segmented.best.glm[i] <- safe_rmse(fit_negbi_train, train_data$R)
      
      
      
    }, error = function(e) {
      
      warning(paste("Negative Binomial segmented model failed for fold", i, ", attempting Quasipoisson fallback:", e$message))
      
      tryCatch({
        
        base_model_qpois <- glm(as.formula(formula_str), data = train_data, family = quasipoisson, na.action = na.omit)
        
        seg_qpois <- segmented::segmented(
          
          base_model_qpois,
          
          seg.Z = as.formula(seg_formula_str),
          
          psi = mean_ssb,
          
          control = seg.control(maxit = 200, tol = 1e-5))
        
        
        
        fit_qpois_test <- predict(seg_qpois, newdata = test_data, type = "response")
        
        rmse_values$segmented.best.glm[i] <- safe_rmse(fit_qpois_test, test_data$R)
        
        
        
        fit_qpois_train <- predict(seg_qpois, newdata = train_data, type = "response")
        
        rmse_values_train$segmented.best.glm[i] <- safe_rmse(fit_qpois_train, train_data$R)
        
        
        
      }, error = function(e2) {
        
        warning(paste("Quasipoisson segmented fallback also failed for fold", i, ":", e2$message))
        
      })
      
    })
    
    
    
    # 7. Structural change detection
    
    tryCatch({
      
      bpts <- strucchange::breakpoints(R ~ SSB, data = train_data, h = 0.15)
      
      
      
      if (!is.null(bpts$RSS.table) && nrow(bpts$RSS.table) > 0) {
        
        tryCatch({
          
          opt_bpts <- strucchange::breakpoints(bpts, ic = "BIC")
          
          
          
          if(!is.null(opt_bpts$breakpoints) && !any(is.na(opt_bpts$breakpoints))) {
            
            opt_breaks <- length(opt_bpts$breakpoints)
            
          } else {
            
            opt_breaks <- 0
            
          }
          
          
          
        }, error = function(e) {
          
          if("RSS1" %in% colnames(bpts$RSS.table)) {
            
            rss1_values <- bpts$RSS.table[,"RSS1"]
            
            if(any(!is.na(rss1_values))) {
              
              opt_breaks <- 1
              
            } else {
              
              opt_breaks <- 0
              
            }
            
          } else {
            
            opt_breaks <- 0
            
          }
          
        })
        
        
        
        if (opt_breaks > 2) opt_breaks <- 2
        
        
        
        if (opt_breaks > 0) {
          
          if(exists("opt_bpts") && !is.null(opt_bpts$breakpoints) && !any(is.na(opt_bpts$breakpoints))) {
            
            bp_final <- opt_bpts
            
          } else {
            
            bp_final <- strucchange::breakpoints(bpts, breaks = opt_breaks)
            
          }
          
          
          
          if(!is.null(bp_final$breakpoints) && !any(is.na(bp_final$breakpoints))) {
            
            train_bp_factor <- strucchange::breakfactor(bp_final, breaks = opt_breaks)
            
            
            
            if(length(levels(train_bp_factor)) > 1) {
              
              strucc_model <- lm(R ~ train_bp_factor * SSB, data = train_data)
              
              
              
              # Training RMSE
              
              fit_strucc_train <- fitted(strucc_model)
              
              rmse_values_train$strucchange[i] <- safe_rmse(fit_strucc_train, train_data$R)
              
              
              
              # Test prediction
              
              bp_values <- train_data$SSB[bp_final$breakpoints]
              
              test_segment <- cut(test_data$SSB, 
                                  
                                  breaks = c(-Inf, bp_values, Inf), 
                                  
                                  labels = levels(train_bp_factor), 
                                  
                                  include.lowest = TRUE)
              
              
              
              test_data_expanded <- test_data
              
              test_data_expanded$train_bp_factor <- test_segment
              
              
              
              fit_strucc_test <- predict(strucc_model, newdata = test_data_expanded)
              
              rmse_values$strucchange[i] <- safe_rmse(fit_strucc_test, test_data$R)
              
            }
            
          }
          
        }
        
      }
      
      
      
    }, error = function(e) {
      
      cat("Structural change model failed for fold", i, ":", e$message, "\n")
      
    })
    
    
    
    if(i %% 10 == 0) {
      
      cat("Completed", i, "folds...\n")
      
    }
    
  }
  
  
  
  # Calculate means
  
  rmse_values_means <- data.frame(
    
    "linear model" = mean(rmse_values$linear.model, na.rm = TRUE),
    
    "beverton" = mean(rmse_values$beverton, na.rm = TRUE),
    
    "ricker" = mean(rmse_values$ricker, na.rm = TRUE),
    
    "segmented" = mean(rmse_values$segmented, na.rm = TRUE),
    
    "segmented log" = mean(rmse_values$segmented.log, na.rm = TRUE),
    
    "segmented best glm" = mean(rmse_values$segmented.best.glm, na.rm = TRUE),
    
    "strucchange" = mean(rmse_values$strucchange, na.rm = TRUE)
    
  )
  
  
  
  rmse_values_means_train <- data.frame(
    
    "linear model" = mean(rmse_values_train$linear.model, na.rm = TRUE),
    
    "beverton" = mean(rmse_values_train$beverton, na.rm = TRUE),
    
    "ricker" = mean(rmse_values_train$ricker, na.rm = TRUE),
    
    "segmented" = mean(rmse_values_train$segmented, na.rm = TRUE),
    
    "segmented log" = mean(rmse_values_train$segmented.log, na.rm = TRUE),
    
    "segmented best glm" = mean(rmse_values_train$segmented.best.glm, na.rm = TRUE),
    
    "strucchange" = mean(rmse_values_train$strucchange, na.rm = TRUE)
    
  )
  
  
  
  cat("Cross-validation completed!\n")
  
  
  
  return(list(
    
    test = rmse_values_means,
    
    train = rmse_values_means_train,
    
    detailed_test = rmse_values,
    
    detailed_train = rmse_values_train
    
  ))
  
}