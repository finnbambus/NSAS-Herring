# Functions for "data_cleanup.Rmd"
#--------------------------------------------------------------------------------------
## Import and combine .nc files
#--------------------------------------------------------------------------------------

load_environmental_data <- function(sst_file, sbt_file, sss_file) {
  ### Load SST data
  nc_sst <- nc_open(sst_file)
  sst_data_raw <- ncvar_get(nc_sst, "thetao")
  lon <- ncvar_get(nc_sst, "longitude")
  lat <- ncvar_get(nc_sst, "latitude")
  time_raw <- ncvar_get(nc_sst, "time")
  nc_close(nc_sst)
  
  ### Load SBT data
  nc_sbt <- nc_open(sbt_file)
  sbt_data_raw <- ncvar_get(nc_sbt, "bottomT")
  nc_close(nc_sbt)
  
  ### Load SSS data
  nc_sss <- nc_open(sss_file)
  sss_data_raw <- ncvar_get(nc_sss, "so")
  nc_close(nc_sss)
  
  ### Handle NAs
  sst_data_raw[is.nan(sst_data_raw)] <- NA
  sst_data_raw[sst_data_raw == -32768] <- NA
  
  sbt_data_raw[is.nan(sbt_data_raw)] <- NA
  sbt_data_raw[sbt_data_raw == -32768] <- NA
  
  sss_data_raw[is.nan(sss_data_raw)] <- NA
  sss_data_raw[sss_data_raw == -32768] <- NA
  
  ### Handle time
  ### Re-open nc_sst to get time attributes, or ensure it's passed/stored if needed
  temp_nc_sst <- nc_open("data raw/NS SST Monthly.nc")
  time_units <- ncatt_get(temp_nc_sst, "time", "units")$value
  nc_close(temp_nc_sst)
  
  time_origin <- sub("seconds since ", "", time_units)
  
  dates <- as.Date(time_raw / (24 * 60 * 60), origin = time_origin)
  years <- as.numeric(format(dates, "%Y"))
  months <- as.numeric(format(dates, "%m"))
  
  ### Combine into a single structure
  env_data_full <- list(
    SST = sst_data_raw,
    SBT = sbt_data_raw,
    SSS = sss_data_raw,
    lon = lon,
    lat = lat,
    years = years,
    months = months,
    dates = dates)
  
  return(env_data_full)}


#--------------------------------------------------------------------------------------
## Subset environmental data by components
#--------------------------------------------------------------------------------------

subset_environmental_data <- function(combined_data) {
  
  ### Load coords_components
  coords_components <- read_csv("data raw/coords_components.csv")
  
  ### Initialise list
  environmental_subsets_by_area <- list()
  
  ### Loop through each area defined in coords_components
  for (i in 1:nrow(coords_components)) {
    area_name <- coords_components$component[i]
    lon_min <- coords_components$lon_min[i]
    lon_max <- coords_components$lon_max[i]
    lat_min <- coords_components$lat_min[i]
    lat_max <- coords_components$lat_max[i]
    
    ### Find the indices of long & lat that fall within the areas bounds
    ### Use 'combined_data$' to access elements of the passed list
    lon_indices <- which(combined_data$lon >= lon_min & combined_data$lon <= lon_max)
    lat_indices <- which(combined_data$lat >= lat_min & combined_data$lat <= lat_max)
    
    ### Subset the 3D environmental arrays using the identified indices
    ### Use 'combined_data$' to access elements of the passed list
    subset_env_list <- list(
      SST = combined_data$SST[lon_indices, lat_indices, ],
      SBT = combined_data$SBT[lon_indices, lat_indices, ],
      SSS = combined_data$SSS[lon_indices, lat_indices, ],
      lon = combined_data$lon[lon_indices],
      lat = combined_data$lat[lat_indices],
      years = combined_data$years,
      months = combined_data$months,
      dates = combined_data$dates)
    
    ### Store data in the 'environmental_subsets_by_area' list
    environmental_subsets_by_area[[area_name]] <- subset_env_list}
  
  return(environmental_subsets_by_area)}


#--------------------------------------------------------------------------------------
## Plot environmental data
#--------------------------------------------------------------------------------------

plot_environmental_ts <- function(data_list, plot_area_title) {
  
  ### Extract variables from the input list
  sst_data_raw <- data_list$SST
  sbt_data_raw <- data_list$SBT
  sss_data_raw <- data_list$SSS
  dates <- data_list$dates
  years <- data_list$years
  months <- data_list$months
  
  ### Calculate overall time series by averaging across the third dimension
  overall_sst_ts <- apply(sst_data_raw, 3, mean, na.rm = TRUE)
  overall_sbt_ts <- apply(sbt_data_raw, 3, mean, na.rm = TRUE)
  overall_sss_ts <- apply(sss_data_raw, 3, mean, na.rm = TRUE)
  
  ### Create a data frame for monthly time series plotting
  overall_ts_df <- data.frame(
    Date = dates,
    Year = years,
    Month = months,
    SST = overall_sst_ts,
    SBT = overall_sbt_ts,
    SSS = overall_sss_ts)
  
  ### Calculate yearly averages from the monthly data
  yearly_avg_df <- overall_ts_df %>%
    group_by(Year) %>%
    summarise(
      SST = mean(SST, na.rm = TRUE),
      SBT = mean(SBT, na.rm = TRUE),
      SSS = mean(SSS, na.rm = TRUE),
      .groups = 'drop') %>%
    mutate(Date = as.Date(paste0(Year, "-06-06")))
  
  ### Melt the data frames for easier plotting with ggplot2 (one 'value' column)
  overall_ts_melted <- melt(overall_ts_df, id.vars = c("Date", "Year", "Month"),
                            variable.name = "Variable", value.name = "Value_Monthly")
  
  yearly_avg_melted <- melt(yearly_avg_df, id.vars = c("Date", "Year"),
                            variable.name = "Variable", value.name = "Value_Yearly")
  
  ### Plot overall time series with yearly averages overlayed
  p <- ggplot(overall_ts_melted, aes(x = Date, y = Value_Monthly, color = Variable)) +
    geom_line(alpha = 0.8) +
    geom_smooth(data = yearly_avg_melted, aes(x = Date, 
                                              y = Value_Yearly, 
                                              color = Variable), size = 1.2) +
    labs(title = paste(plot_area_title, "Environmental Conditions (Monthly & Yearly Averages)"),
         y = "Value",
         x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("SST" = "red", "SBT" = "blue", "SSS" = "darkgreen")) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
    theme(legend.position = "none")
  
  return(p)}


# Functions for "data_analysis.Rmd"
#--------------------------------------------------------------------------------------
## Plot SSB change-points
#--------------------------------------------------------------------------------------

plot_SSB_cpt <- function(data, changepoints, component, SSB_column, l_bnd_column, u_bnd_column,
                         ribbon_colors = NULL, 
                         vline_colors = NULL,
                         start_year = NULL, 
                         end_year = NULL, 
                         show_hlines = TRUE,
                         scale_to_full = FALSE) {
  
  # Set default colors if not provided
  default_ribbon_colors <- c("steelblue", "darkorange", "purple", "lightgreen", "indianred", "cyan", "magenta")
  default_vline_colors <- c("black", "black", "black", "black", "black", "black", "black", "grey")
  
  if (is.null(ribbon_colors)) {
    ribbon_colors <- rep(default_ribbon_colors, length.out = length(changepoints) + 1)}
  
  if (is.null(vline_colors)) {
    vline_colors <- rep(default_vline_colors, length.out = length(changepoints))}
  
  # Determine start and end years from data if not provided
  if (is.null(start_year)) {
    start_year <- min(data$year, na.rm = TRUE)}
  if (is.null(end_year)) {
    end_year <- max(data$year, na.rm = TRUE)}
  
  # Create the base plot
  p <- ggplot(data) +
    geom_line(aes(x = year, y = .data[[SSB_column]]/1000000), linewidth = 0.8) +
    labs(title = paste(component, "SSB Change-point analysis"), 
         x = "Year", 
         y = "SSB in million t") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
  
  # Scale to full stock proportio if enabled
  if (scale_to_full) {
    p <- p + xlim(1947, 2025) +
      ylim(0, 7)
  }
  
  # Add horizontal lines if enabled
  if (show_hlines) {
    p <- p + 
      geom_hline(yintercept = 1130747/1000000, col = "darkorange") + #MSY Btrigger
      geom_hline(yintercept = 1049521/1000000, col = "gray", linetype = "dashed") + #Bpa
      geom_hline(yintercept = 828874/1000000, col = "gray", linetype = "dotted")} #Blim
  
  # Add vertical lines for changepoints
  for (i in seq_along(changepoints)) {
    p <- p + geom_vline(xintercept = changepoints[i], 
                        col = vline_colors[i], 
                        linewidth = 0.8)}
  
  # Create year ranges for ribbons
  all_years <- c(start_year, changepoints, end_year)
  
  # Add ribbons for each period
  for (i in 1:(length(all_years) - 1)) {
    year_range <- all_years[i]:all_years[i + 1]
    
    p <- p + geom_ribbon(data = filter(data, year %in% year_range),
                         mapping = aes(x = year, 
                                       ymin = .data[[l_bnd_column]]/1000000,
                                       ymax = .data[[u_bnd_column]]/1000000),
                         fill = ribbon_colors[i], 
                         alpha = 0.5)}
  
  return(p)}


#--------------------------------------------------------------------------------------
## Extract optimal breakpoint
#--------------------------------------------------------------------------------------

opt_bpts <- function(x) {
  #x = bpts_sum$RSS["BIC",]
  n <- length(x)
  lowest <- vector("logical", length = n-1)
  lowest[1] <- FALSE
  for (i in 2:n) {
    lowest[i] <- x[i] < x[i-1] & x[i] < x[i+1]
  }
  out <- as.integer(names(x)[lowest])
  return(out)}


#--------------------------------------------------------------------------------------
## Plot Hysteresis
#--------------------------------------------------------------------------------------

plot_hysteresis <- function(data, break_years, component,
                            msy_btrigger = 1130747, 
                            fmsy = 0.32,
                            colors = c("steelblue", "darkorange", "purple", "lightgreen", "indianred")) {
  
  # Determine number of break years
  n_breaks <- length(break_years)
  n_phases <- n_breaks + 1
  
  # Create phase assignment vector
  phase_assignment <- rep(1, nrow(data))
  for (i in 1:n_breaks) {
    phase_assignment[data$year > break_years[i]] <- i + 1}
  
  # Assign colors based on phase
  hyst_phases <- colors[phase_assignment]
  
  # Create list of phase data
  phase_data_list <- list()
  for (i in 1:n_phases) {
    if (i == 1) {
      # First phase: up to first break year
      phase_data_list[[i]] <- data %>% filter(year <= break_years[1])
    } else if (i == n_phases) {
      # Last phase: after last break year
      phase_data_list[[i]] <- data %>% filter(year > break_years[n_breaks])
    } else {
      # Middle phases: between break years
      phase_data_list[[i]] <- data %>% filter(year > break_years[i-1] & year <= break_years[i])}}
  
  # Create the plot
  p <- ggplot(data = data, aes(x = F, y = SSB/1000000)) +
    geom_path(colour = "grey") +
    geom_hline(yintercept = msy_btrigger/1000000, linetype = "dashed", color = "gray30") +
    geom_vline(xintercept = fmsy, linetype = "dashed", color = "gray30") +
    geom_label(x = fmsy, y = 0.5, label = expression("F"[MSY]), 
               color = "gray30", size = 3.5) +
    geom_label(x = 1.2, y = msy_btrigger/1000000, label = expression("MSY B"[trigger]), 
               color = "gray30", size = 3.5, fontface = "bold") +
    geom_point(colour = hyst_phases) +
    labs(title = paste("Hysteresis in", component), x = "F", y = "SSB in millions") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Add geom_smooth for each phase (only if data exists)
  for (i in 1:n_phases) {
    if (nrow(phase_data_list[[i]]) > 0) {
      p <- p + geom_smooth(data = phase_data_list[[i]], aes(x = F, y = SSB/1000000),
                           method = "lm", colour = colors[i])}}
  
  # Add text labels for break years (only for existing break years)
  # Define nudge parameters for each break year position
  nudge_params <- list(
    list(nudge_y = 0, nudge_x = 0.2),
    list(nudge_y = -0.5, nudge_x = 0.2),
    list(nudge_y = 0, nudge_x = -0.2),
    list(nudge_y = 0, nudge_x = 0.2))
  
  for (i in 1:n_breaks) {
    break_year_data <- data %>% filter(year == break_years[i])
    if (nrow(break_year_data) > 0) {
      nudge_y <- if (i <= length(nudge_params)) nudge_params[[i]]$nudge_y else 0
      nudge_x <- if (i <= length(nudge_params)) nudge_params[[i]]$nudge_x else 0
      
      p <- p + geom_text_repel(data = break_year_data, aes(label = year),
                               point.padding = 0.2, nudge_y = nudge_y, nudge_x = nudge_x,
                               size = 3, col = "black", segment.size = 0.2)}}
  p <- p + geom_text_repel(data = data[c(1, length(data$year)),], aes(label = year),
                           point.padding = 0.2, nudge_y = 0, nudge_x = -0.1,
                           size = 3, col = "black", segment.size = 0.2)
  
  return(p)}


#--------------------------------------------------------------------------------------
## Test different Models
#--------------------------------------------------------------------------------------

# Helper Functions
rmse <- function(sim, obs) {
  sqrt(mean((obs - sim)^2, na.rm = TRUE))}

check_data_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in data:", paste(missing_cols, collapse = ", ")))}}

fit_glm_models <- function(data, recruit_col = "R", ssb_col = "SSB") {
  # Check if columns exist
  check_data_columns(data, c(recruit_col, ssb_col))
  
  # Create formula
  formula_str <- paste(recruit_col, "~", ssb_col)
  formula_obj <- as.formula(formula_str)
  
  models <- list(
    gaussian = glm(formula_obj, data = data, family = gaussian),
    poisson = glm(formula_obj, data = data, family = poisson),
    quasipoisson = glm(formula_obj, data = data, family = quasipoisson),
    negbinom = MASS::glm.nb(formula_obj, data = data))
  
  # Calculate overdispersion
  overdispersion <- sapply(models, function(m) {
    deviance(m) / df.residual(m)})
  
  list(models = models, overdispersion = overdispersion)}

# Main Modeling Functions

# 1. Basic Models
fit_independence_model <- function(data, recruit_col = "R", ssb_col = "ssb") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  # Create formula and fit model
  formula_str <- paste(recruit_col, "~", ssb_col)
  model <- lm(as.formula(formula_str), data = data)
  
  list(model = model, fitted = fitted(model))}

fit_beverton_holt <- function(data, recruit_col = "R", ssb_col = "ssb") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  # Create formula for srStarts
  formula_str <- paste(recruit_col, "~", ssb_col)
  formula_obj <- as.formula(formula_str)
  
  tryCatch({
    sv <- FSA::srStarts(formula_obj, data = data, type = "BevertonHolt")
    bh <- FSA::srFuns("BevertonHolt")
    
    # Create log formula for nls
    log_formula_str <- paste("log(", recruit_col, ") ~ log(bh(", ssb_col, ", a, b))")
    model <- nls(as.formula(log_formula_str), data = data, start = sv)
    
    fitted_vals <- bh(data[[ssb_col]], a = coef(model))
    r2 <- cor(fitted_vals, data[[recruit_col]])^2
    
    list(model = model, fitted = fitted_vals, r2 = r2)
  }, error = function(e) {
    warning(paste("Beverton-Holt model failed:", e$message))
    list(model = NULL, fitted = NULL, r2 = NULL)})}

fit_ricker <- function(data, recruit_col = "R", ssb_col = "ssb") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  # Create formula for srStarts
  formula_str <- paste(recruit_col, "~", ssb_col)
  formula_obj <- as.formula(formula_str)
  
  tryCatch({
    sv <- FSA::srStarts(formula_obj, data = data, type = "Ricker")
    rckr <- FSA::srFuns("Ricker")
    
    # Create log formula for nls
    log_formula_str <- paste("log(", recruit_col, ") ~ log(rckr(", ssb_col, ", a, b))")
    model <- nls(as.formula(log_formula_str), data = data, start = sv)
    
    fitted_vals <- rckr(data[[ssb_col]], a = coef(model))
    
    list(model = model, fitted = fitted_vals)
  }, error = function(e) {
    warning(paste("Ricker model failed:", e$message))
    list(model = NULL, fitted = NULL)})}

# 2. Segmented Models
fit_segmented_models <- function(data, recruit_col = "R", ssb_col = "SSB") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  mean_ssb <- mean(data[[ssb_col]], na.rm = TRUE)
  mean_ssb_log <- mean(log(data[[ssb_col]]), na.rm = TRUE)
  
  results <- list()
  
  # Regular segmented
  tryCatch({
    formula_str <- paste(recruit_col, "~", ssb_col)
    base_model <- lm(as.formula(formula_str), data = data)
    seg_formula_str <- paste("~", ssb_col)
    
    seg_regular <- segmented::segmented(
      base_model, 
      seg.Z = as.formula(seg_formula_str), 
      psi = mean_ssb)
    
    results$regular <- list(
      model = seg_regular,
      fitted = seg_regular$fitted.values,
      breakpoint = seg_regular$psi[2],
      breakpoint_se = seg_regular$psi[3])
  }, error = function(e) {
    warning(paste("Regular segmented model failed:", e$message))
    results$regular <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)})
  
  # Log-transformed segmented
  tryCatch({
    data$r_log <- log(data[[recruit_col]])
    data$ssb_log <- log(data[[ssb_col]])
    
    base_model_log <- lm(r_log ~ ssb_log, data = data)
    seg_log <- segmented::segmented(
      base_model_log, 
      seg.Z = ~ssb_log, 
      psi = mean_ssb_log)
    
    results$log <- list(
      model = seg_log,
      fitted = seg_log$fitted.values,
      breakpoint = seg_log$psi[2],
      breakpoint_se = seg_log$psi[3])
  }, error = function(e) {
    warning(paste("Log segmented model failed:", e$message))
    results$log <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)})
  
  # Negative binomial segmented
  tryCatch({
    # First fit regular GLM with quasipoisson (handles overdispersion)
    formula_str <- paste(recruit_col, "~", ssb_col)
    base_model_qpois <- glm(as.formula(formula_str), data = data, family = quasipoisson)
    seg_formula_str <- paste("~", ssb_col)
    
    seg_qpois <- segmented::segmented(
      base_model_qpois, 
      seg.Z = as.formula(seg_formula_str), 
      psi = mean_ssb)
    
    results$negbinom <- list(
      model = seg_qpois,
      fitted = seg_qpois$fitted.values,
      breakpoint = seg_qpois$psi[2],
      breakpoint_se = seg_qpois$psi[3],
      note = "Using quasipoisson instead of negative binomial for segmented model")
  }, error = function(e) {
    warning(paste("Quasipoisson segmented model failed:", e$message))
    
    # Fallback: try with regular gaussian segmented
    tryCatch({
      formula_str <- paste(recruit_col, "~", ssb_col)
      base_model_gaus <- lm(as.formula(formula_str), data = data)
      seg_formula_str <- paste("~", ssb_col)
      
      seg_gaus <- segmented::segmented(
        base_model_gaus, 
        seg.Z = as.formula(seg_formula_str), 
        psi = mean_ssb)
      
      results$negbinom <- list(
        model = seg_gaus,
        fitted = seg_gaus$fitted.values,
        breakpoint = seg_gaus$psi[2],
        breakpoint_se = seg_gaus$psi[3],
        note = "Using gaussian fallback for segmented model")
    }, error = function(e2) {
      warning(paste("Gaussian segmented fallback also failed:", e2$message))
      results$negbinom <- list(model = NULL, fitted = NULL, breakpoint = NULL, breakpoint_se = NULL)})})
  
  return(results)}

# 3. Structural Change Model
fit_strucchange <- function(data, recruit_col = "R", ssb_col = "SSB") {
  check_data_columns(data, c(recruit_col, ssb_col))
  
  tryCatch({
    formula_str <- paste(recruit_col, "~", ssb_col)
    bpts <- strucchange::breakpoints(as.formula(formula_str), data = data)
    bpts_sum <- summary(bpts)
    
    # Find optimal breakpoints
    if (length(opt_brks) > 0 && opt_brks[1] > 0) {
      bpts2 <- strucchange::breakpoints(bpts, breaks = opt_brks)
      best_brk <- data[[ssb_col]][bpts2$breakpoints]
      
      # Fit segmented model (simplified for 2 breakpoints)
      if (length(best_brk) >= 2) {
        # Create the complex formula
        formula_complex <- as.formula(paste(
          recruit_col, "~", ssb_col, "* (", ssb_col, "<=", best_brk[1], ") +",
          ssb_col, "* (", ssb_col, ">=", best_brk[1], "&", ssb_col, "<=", best_brk[2], ") +",
          ssb_col, "* (", ssb_col, ">=", best_brk[2], ")"))
        
        model <- lm(formula_complex, data = data)
        
        list(model = model, breakpoints = best_brk, bpts_obj = bpts)
      } else {
        list(model = NULL, breakpoints = NULL, bpts_obj = bpts)}
    } else {
      list(model = NULL, breakpoints = NULL, bpts_obj = bpts)}
  }, error = function(e) {
    warning(paste("Structural change model failed:", e$message))
    list(model = NULL, breakpoints = NULL, bpts_obj = NULL)})}

# Model Comparison Function ----
compare_models <- function(models_list, observed_data) {
  model_names <- names(models_list)
  n_models <- length(models_list)
  
  # Initialize comparison dataframe
  comparison <- data.frame(
    Model = model_names,
    AIC = numeric(n_models),
    RMSE = numeric(n_models),
    stringsAsFactors = FALSE)
  
  for (i in seq_along(models_list)) {
    model_info <- models_list[[i]]
    
    if (!is.null(model_info) && !is.null(model_info$model) && !is.null(model_info$fitted)) {
      tryCatch({
        comparison$AIC[i] <- AIC(model_info$model)
        comparison$RMSE[i] <- rmse(model_info$fitted, observed_data)
      }, error = function(e) {
        comparison$AIC[i] <- NA
        comparison$RMSE[i] <- NA})
    } else {
      comparison$AIC[i] <- NA
      comparison$RMSE[i] <- NA}}
  
  return(comparison)}

# Main Analysis Workflow ----
run_srr_analysis <- function(SSB_lag_data, SSB_data, 
                             lag_recruit_col = "R", lag_SSB_col = "ssb",
                             norm_recruit_col = "R", norm_SSB_col = "SSB") {
  
  cat("Starting SRR analysis...\n")
  
  # Initialize results
  models <- list()
  
  # Basic models (using SSB_lag_data)
  cat("Fitting independence model...\n")
  models$independence <- fit_independence_model(SSB_lag_data, lag_recruit_col, lag_SSB_col)
  
  cat("Fitting Beverton-Holt model...\n")
  models$beverton_holt <- fit_beverton_holt(SSB_lag_data, lag_recruit_col, lag_SSB_col)
  
  cat("Fitting Ricker model...\n")
  models$ricker <- fit_ricker(SSB_lag_data, lag_recruit_col, lag_SSB_col)
  
  # Segmented models (using plaice data)
  cat("Fitting segmented models...\n")
  segmented_models <- fit_segmented_models(SSB_data, norm_recruit_col, norm_SSB_col)
  models$segmented <- segmented_models
  
  # Structural change model
  cat("Fitting structural change model...\n")
  models$strucchange <- fit_strucchange(SSB_data, norm_recruit_col, norm_SSB_col)
  
  # GLM model comparison
  cat("Comparing GLM models...\n")
  glm_results <- fit_glm_models(SSB_data, norm_recruit_col, norm_SSB_col)
  
  # Model comparison
  models_for_comparison <- list(
    independence = models$independence,
    beverton_holt = models$beverton_holt,
    ricker = models$ricker,
    segmented_regular = segmented_models$regular,
    segmented_log = segmented_models$log,
    segmented_negbi = segmented_models$negbinom)
  
  cat("Comparing models...\n")
  comparison <- compare_models(models_for_comparison, SSB_data[[norm_recruit_col]])
  
  cat("Analysis complete!\n")
  
  return(list(
    models = models,
    glm_results = glm_results,
    comparison = comparison))}


#--------------------------------------------------------------------------------------
## Plot SRR
#--------------------------------------------------------------------------------------

plot_SRR <- function(data, break_years, title_stock, used_model,
                     Blim = 828874,
                     colors = c("steelblue", "darkorange", "purple", "lightgreen", "indianred", "darkred", "darkblue")) {
  
  # Determine number of break years and resulting phases
  n_breaks <- length(break_years)
  n_phases <- n_breaks + 1
  
  data$phase <- 1
  for (i in 1:n_breaks) {
    data$phase[data$year > break_years[i]] <- i + 1}
  
  # Add initial plot
  p <- ggplot(data = data, aes(x = SSB / 1000000, y = R / 1000000)) +
    geom_path(colour = "grey") +
    geom_point(aes(color = factor(phase))) +
    scale_color_manual(values = colors[1:n_phases]) +
    geom_vline(xintercept = Blim / 1000000, linetype = "dashed", color = "gray30") +
    geom_label(x = Blim / 1000000, y = 7, label = expression("B"[lim]),
               color = "gray30", size = 3.5, fontface = "bold") +
    labs(title = paste("Stock-Recruitment Relationship for", title_stock), subtitle = used_model,
         x = "SSB in millions", y = "R in billions") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
          plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
          legend.position = "none",
          axis.text = element_text(size = 12))
  
  # Add geom_smooth for each phase
  for (i in 1:n_phases) {
    phase_data <- data %>% dplyr::filter(phase == i)
    if (nrow(phase_data) > 0) {
      p <- p + geom_smooth(data = phase_data,
                           mapping = aes(x = SSB / 1000000, y = R / 1000000),
                           col = colors[i], method = "lm")}}
  
  # Define nudge parameters for text labels to avoid overlap
  nudge_params <- list(
    list(nudge_y = -5, nudge_x = -0.2), # first year
    list(nudge_y = 0, nudge_x = -0.5),  # breakpoint
    list(nudge_y = -5, nudge_x = -0.2), # last year
    list(nudge_y = 5, nudge_x = -0.2),  # more breakpoints
    list(nudge_y = -0, nudge_x = -0.2))
  
  # Add text labels for the first and last years
  p <- p +  geom_text_repel(data = data[1, ], aes(label = year),
                           point.padding = 0.2, nudge_y = nudge_params[[1]]$nudge_y,
                           nudge_x = nudge_params[[1]]$nudge_x,
                           size = 3, col = "gray30", segment.size = 0.2) +
            geom_text_repel(data = data[nrow(data)-1, ], aes(label = year),
                            point.padding = 0.2, nudge_y = nudge_params[[3]]$nudge_y,
                            nudge_x = nudge_params[[3]]$nudge_x,
                            size = 3, col = "gray30", segment.size = 0.2)
  
  # Add text labels for each breakpoint year
  for (i in 1:n_breaks) {
    brk_year <- break_years[i]
    break_year_data <- data %>% dplyr::filter(year == brk_year)
    if (nrow(break_year_data) > 0) {
      nudge_idx <- (i %% length(nudge_params)) + 1
      p <- p + geom_text_repel(data = break_year_data, aes(label = year),
                               point.padding = 0.2,
                               nudge_y = nudge_params[[nudge_idx]]$nudge_y,
                               nudge_x = nudge_params[[nudge_idx]]$nudge_x,
                               size = 3, col = "gray30", segment.size = 0.2)}}

  return(p)}


#--------------------------------------------------------------------------------------
## tGAM Analysis
#--------------------------------------------------------------------------------------

run_threshold_gam <- function(data, 
                              response_var, 
                              pressure_var, 
                              threshold_var, 
                              time_var = "year") {
  
  # Create variables - use [[ ]] to access columns by variable name
  y <- data[[response_var]]
  x <- data[[pressure_var]]
  x2 <- data[[threshold_var]]
  time <- data[[time_var]]
  
  mod <- gam(y ~ s(x, k = 3)) 
  tmod <- thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = threshold_var,
                     k = 4, a = 0.2, b = 0.8)                             
  
  # Test interaction
  print("Leave one out crossvalidation result:")
  print(loocv_result <- loocv_thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = threshold_var, k = 4, a = 0.2, b = 0.8, time = time))
  
  print("tmod summary")
  print(summary(tmod))
  print("tmod mr")
  print(tmod$mr)
  
  tmod$train_na <- rep(FALSE, times = length(y))
  
  print("tmod diagnostic plots")
  print(plot_diagnostics(tmod)$all_plots)
  
  # Add vector with predicted values to data set
  return(tgam_pred <- predict(tmod))}