
#--------------------------------------------------------------------------------------
## Helper functions
#--------------------------------------------------------------------------------------

# A simple is_empty function
is_empty <- function(x) {
  length(x) == 0 || is.null(x) || all(is.na(x))}

# Calculate Root Mean Squared Error (RMSE)
rmse <- function(sim, obs) {
  sqrt(mean((obs - sim)^2, na.rm = TRUE))}

# Check if required columns exist in a data frame
check_data_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in data:", paste(missing_cols, collapse = ", ")))}}

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
    overdispersion$gaussian <- NA})
  
  # Poisson GLM
  tryCatch({
    models$poisson <- glm(formula_obj, data = data, family = poisson)
    overdispersion$poisson <- deviance(models$poisson) / df.residual(models$poisson)
  }, error = function(e) {
    warning(paste("Poisson GLM failed:", e$message))
    models$poisson <- NULL
    overdispersion$poisson <- NA})
  
  # Quasipoisson GLM
  tryCatch({
    models$quasipoisson <- glm(formula_obj, data = data, family = quasipoisson)
    overdispersion$quasipoisson <- deviance(models$quasipoisson) / df.residual(models$quasipoisson)
  }, error = function(e) {
    warning(paste("Quasipoisson GLM failed:", e$message))
    models$quasipoisson <- NULL
    overdispersion$quasipoisson <- NA})
  
  # Negative Binomial GLM
  tryCatch({
    models$negbinom <- MASS::glm.nb(formula_obj, data = data)
    overdispersion$negbinom <- deviance(models$negbinom) / df.residual(models$negbinom)
  }, error = function(e) {
    warning(paste("Negative Binomial GLM failed:", e$message))
    models$negbinom <- NULL
    overdispersion$negbinom <- NA})
  
  list(models = models, overdispersion = overdispersion)}

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
  
  list(model = model, fitted = fitted(model))}

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
      r2 <- cor(fitted_vals, data[[recruit_col]], use = "pairwise.complete.obs")^2}))
    
    list(model = model, fitted = fitted_vals, r2 = r2)
  }, error = function(e) {
    warning(paste("Beverton-Holt model failed:", e$message))
    list(model = NULL, fitted = fitted_vals, r2 = r2)})}

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
      
      fitted_vals <- rckr(data[[ssb_col]], a = coef(model))}))
    
    list(model = model, fitted = fitted_vals)
  }, error = function(e) {
    warning(paste("Ricker model failed:", e$message))
    list(model = NULL, fitted = fitted_vals)})}

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
  data_log <- data %>%
    dplyr::mutate(
      R_log = log(.[[recruit_col]]),
      ssb_log = log(.[[ssb_col]]))
  
  mean_ssb_log <- mean(data_log$ssb_log, na.rm = TRUE)
  
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
      psi = mean_ssb)
    
    results$regular <- list(
      model = seg_regular,
      fitted = fitted(seg_regular),
      breakpoint = seg_regular$psi[2],
      breakpoint_se = seg_regular$psi[3])
    
  }, error = function(e) {
    warning(paste("Regular segmented model failed:", e$message))
    results$regular <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)})
  
  # Log-transformed segmented (lm on log-transformed data)
  tryCatch({
    base_model_log <- lm(R_log ~ ssb_log, data = data_log)
    seg_log <- segmented::segmented(
      base_model_log,
      seg.Z = ~ssb_log,
      psi = mean_ssb_log)
    
    results$log <- list(
      model = seg_log,
      fitted = fitted(seg_log),
      breakpoint = seg_log$psi[2],
      breakpoint_se = seg_log$psi[3])
    
  }, error = function(e) {
    warning(paste("Log segmented model failed:", e$message))
    results$log <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)})
  
  # Segmented with Negative Binomial GLM (primary attempt)
  # This section is the focus of the fix for the "unused argument" error
  tryCatch({
    # Fit glm.nb separately first
    base_model_negbi_fit <- MASS::glm.nb(as.formula(formula_str), data = data)
    
    # Attempt to simplify the call object before passing to segmented
    # This is a common workaround for issues where functions try to re-evaluate the call
    # and get confused by complex arguments like the family list from glm.nb.
    # We remove the family argument from the call, as segmented should infer it
    # or handle it differently.
    simplified_call <- base_model_negbi_fit$call
    simplified_call$family <- NULL # Remove the problematic family argument from the call
    
    # Create a new glm.nb object with the simplified call
    # This might not be strictly necessary if segmented doesnt re-evaluate the call
    # but rather uses the family slot directly. However, the error suggests re-evaluation.
    base_model_negbi_simplified <- base_model_negbi_fit
    base_model_negbi_simplified$call <- simplified_call
    
    
    seg_negbi <- segmented::segmented(
      obj = base_model_negbi_simplified, # Pass the potentially simplified object
      seg.Z = as.formula(seg_formula_str),
      psi = mean_ssb)
    
    results$negbinom <- list(
      model = seg_negbi,
      fitted = fitted(seg_negbi),
      breakpoint = seg_negbi$psi[2],
      breakpoint_se = seg_negbi$psi[3],
      note = "Base model: Negative Binomial GLM")
    
  }, error = function(e) {
    warning(paste("Negative Binomial segmented model failed, attempting Quasipoisson fallback:", e$message))
    # Fallback to Quasipoisson GLM for segmented if Negative Binomial fails
    tryCatch({
      base_model_qpois <- glm(as.formula(formula_str), data = data, family = quasipoisson)
      seg_qpois <- segmented::segmented(
        base_model_qpois,
        seg.Z = as.formula(seg_formula_str),
        psi = mean_ssb)
      
      results$negbinom_fallback <- list( # Use a different key for fallback
        model = seg_qpois,
        fitted = fitted(seg_qpois),
        breakpoint = seg_qpois$psi[2],
        breakpoint_se = seg_qpois$psi[3],
        note = "Base model: Quasipoisson GLM (Negative Binomial fallback)")
    }, error = function(e2) {
      warning(paste("Quasipoisson segmented fallback also failed:", e2$message))
      results$negbinom_fallback <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)})})
  
  return(results)}

# Fit a structural change model
#
# This function uses `strucchange::breakpoints` to identify and fit a model
# with a specified number of structural breakpoints.
#
# @param data A data frame.
# @param recruit_col The name of the recruitment column.
# @param ssb_col The name of the SSB column.
# @param opt_brks_val The desired number of breakpoints (integer).
# @return A list containing the fitted model, detected breakpoints, and related objects.
fit_strucchange <- function(data, recruit_col = "R", ssb_col = "SSB", opt_brks_val = 1) {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  tryCatch({
    formula_str <- paste(recruit_col, "~", ssb_col)
    bpts <- strucchange::breakpoints(as.formula(formula_str), data = data)
    
    if (opt_brks_val > 0) {
      bpts2 <- strucchange::breakpoints(bpts, breaks = opt_brks_val)
      best_brk <- data[[ssb_col]][bpts2$breakpoints]
      
      # Dynamically construct the formula using breakfactor for flexibility
      # This creates dummy variables for each segment based on breakpoints
      model_formula <- as.formula(paste(recruit_col, "~", "strucchange::breakfactor(bpts2, breaks =", opt_brks_val, ")"))
      
      model <- lm(model_formula, data = data)
      
      # Predict fitted values using the original data to ensure correct alignment
      fitted_values <- predict(model, newdata = data)
      
      list(model = model, breakpoints = best_brk, bpts_obj = bpts, fitted = fitted_values)
    } else {
      # If 0 breakpoints are specified, fit a simple linear model (no structural change)
      message("opt_brks_val is 0. Fitting a simple linear model as no structural change.")
      model <- lm(as.formula(formula_str), data = data)
      list(model = model, breakpoints = NULL, bpts_obj = NULL, fitted = fitted(model))}
    
  }, error = function(e) {
    warning(paste("Structural change model failed:", e$message))
    list(model = NULL, breakpoints = NULL, bpts_obj = NULL, fitted = NULL)})}

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
    stringsAsFactors = FALSE)
  
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
            comparison$R_squared[i] <- cor(model_info$fitted, observed_data, use = "pairwise.complete.obs")^2}
        } else if (inherits(model_info$model, "lm") || inherits(model_info$model, "glm")) {
          # For lm and glm, extract from summary
          summary_model <- summary(model_info$model)
          if (!is.null(summary_model$r.squared)) {
            comparison$R_squared[i] <- summary_model$r.squared
          } else if (!is.null(summary_model$adj.r.squared)) {
            comparison$R_squared[i] <- summary_model$adj.r.squared
          } else {
            # Fallback for GLMs that dont report R-squared directly (e.g., some quasi-models)
            comparison$R_squared[i] <- NA}
        } else if (inherits(model_info$model, "segmented")) {
          # For segmented models, extract R-squared from the underlying base model
          if (inherits(model_info$model$obj, "lm")) {
            comparison$R_squared[i] <- summary(model_info$model$obj)$r.squared
          } else if (inherits(model_info$model$obj, "glm")) {
            summary_obj <- summary(model_info$model$obj)
            comparison$R_squared[i] <- ifelse(!is.null(summary_obj$r.squared), summary_obj$r.squared,
                                              ifelse(!is.null(summary_obj$adj.r.squared), summary_obj$adj.r.squared, NA))
          } else {
            comparison$R_squared[i] <- NA}
        } else {
          comparison$R_squared[i] <- NA}
        
      }, error = function(e) {
        warning(paste("Error during comparison for model", model_names[i], ":", e$message))
        comparison$AIC[i] <- NA
        comparison$RMSE[i] <- NA
        comparison$R_squared[i] <- NA})
    } else {
      # If model or fitted values are NULL, set all metrics to NA
      comparison$AIC[i] <- NA
      comparison$RMSE[i] <- NA
      comparison$R_squared[i] <- NA}}
  
  return(comparison)}

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
# @param opt_brks_val The desired number of breakpoints for the structural change model (integer).
# @return A list containing all fitted models, GLM diagnostic results, and a model comparison table.
run_srr_analysis <- function(data_for_srr,
                             recruit_col = "R",
                             ssb_col = "SSB",
                             opt_brks_val = 1) {
  
  cat("Starting SRR analysis...\n")
  
  # Ensure relevant columns are present and numeric, and drop rows with NAs
  data_for_srr_cleaned <- data_for_srr %>%
    dplyr::select(!!sym(recruit_col), !!sym(ssb_col)) %>%
    tidyr::drop_na() %>% # Explicitly call tidyr::drop_na
    dplyr::mutate(dplyr::across(c(!!sym(recruit_col), !!sym(ssb_col)), as.numeric)) # Ensure columns are numeric
  
  if (nrow(data_for_srr_cleaned) == 0) {
    stop("Input data is empty after cleaning. Cannot perform SRR analysis.")}
  
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
    models$segmented_negbinom_fallback <- segmented_results$negbinom_fallback}
  
  # 3. Structural change model
  cat(paste0("Fitting structural change model with ", opt_brks_val, " breakpoints...\n"))
  models$strucchange <- fit_strucchange(data_for_srr_cleaned, recruit_col, ssb_col, opt_brks_val)
  
  # 4. GLM model comparison (for diagnostic purposes, not directly used in segmented here)
  cat("Fitting base GLM models (Gaussian, Poisson, Quasipoisson, NB) for diagnostics...\n")
  glm_diagnostic_results <- fit_glm_models(data_for_srr_cleaned, recruit_col, ssb_col)
  # Store these GLMs in the main models list for comparison if desired
  models$glm_gaussian <- list(model = glm_diagnostic_results$models$gaussian,
                              fitted = if(!is.null(glm_diagnostic_results$models$gaussian)) fitted(glm_diagnostic_results$models$gaussian) else NULL)
  models$glm_poisson <- list(model = glm_diagnostic_results$models$poisson,
                             fitted = if(!is.null(glm_diagnostic_results$models$poisson)) fitted(glm_diagnostic_results$models$poisson) else NULL)
  models$glm_quasipoisson <- list(model = glm_diagnostic_results$models$quasipoisson,
                                  fitted = if(!is.null(glm_diagnostic_results$models$quasipoisson)) fitted(glm_diagnostic_results$models$quasipoisson) else NULL)
  models$glm_negbinom <- list(model = glm_diagnostic_results$models$negbinom,
                              fitted = if(!is.null(glm_diagnostic_results$models$negbinom)) fitted(glm_diagnostic_results$models$negbinom) else NULL)
  
  
  # Prepare list of models for the final comparison table
  models_for_comparison <- list(
    independence = models$independence,
    beverton_holt = models$beverton_holt,
    ricker = models$ricker,
    segmented_regular = models$segmented_regular,
    segmented_log = models$segmented_log,
    segmented_negbinom = models$segmented_negbinom) # NB or its QP fallback
  if (!is.null(models$segmented_negbinom_fallback)) {
    models_for_comparison$segmented_negbinom_fallback <- models$segmented_negbinom_fallback}
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
    comparison_table = comparison_table))}