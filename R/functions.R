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
## Process CPR data
#--------------------------------------------------------------------------------------

process_food_availability <- function(cpr_data, 
                                      shapefile_path = NULL, 
                                      polygon_id_column = "NAME") {
  
  # Helper function to assign seasons
  assign_season <- function(month) {
    case_when(
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer", 
      month %in% c(9, 10, 11) ~ "Autumn",
      month %in% c(12, 1, 2) ~ "Winter",
      TRUE ~ NA_character_
    )
  }
  
  # Step 1: Calculate food availability for each sample
  processed_data <- cpr_data %>%
    rowwise() %>%
    mutate(
      food_availability = sum(c_across(starts_with("X")), na.rm = TRUE),
      Season = assign_season(Month)
    ) %>%
    ungroup() %>%
    # Remove rows with missing temporal data
    filter(!is.na(Year), !is.na(Season))
  
  # Step 2: Handle spatial aggregation if shapefile provided
  if (!is.null(shapefile_path)) {
    
    cat("Processing with spatial aggregation...\n")
    
    # Load shapefile
    polygons <- st_read(shapefile_path, quiet = TRUE)
    
    # Convert to spatial points (remove rows with missing coordinates)
    spatial_data <- processed_data %>%
      filter(!is.na(Longitude), !is.na(Latitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(polygons))
    
    # Spatial join
    points_in_polygons <- st_join(spatial_data, polygons)
    
    # Aggregate by region, year, and season
    result <- points_in_polygons %>%
      st_drop_geometry() %>%
      filter(!is.na(.data[[polygon_id_column]])) %>%
      group_by(region = .data[[polygon_id_column]], Year, Season) %>%
      summarise(
        total_food = sum(food_availability, na.rm = TRUE),
        average_food = mean(food_availability, na.rm = TRUE),
        sample_count = length(food_availability),
        .groups = 'drop'
      ) %>%
      arrange(region, Year, Season)
    
    cat("Spatial aggregation complete.\n")
    cat("Regions found:", length(unique(result$region)), "\n")
    
  } else {
    
    cat("Processing without spatial aggregation...\n")
    
    # Aggregate by year and season only (no spatial separation)
    result <- processed_data %>%
      group_by(Year, Season) %>%
      summarise(
        total_food = sum(food_availability, na.rm = TRUE),
        average_food = mean(food_availability, na.rm = TRUE),
        sample_count = length(food_availability),
        .groups = 'drop'
      ) %>%
      arrange(Year, Season)
    
    cat("Temporal aggregation complete.\n")
  }
  
  # Step 3: Add summary information
  cat("\n=== PROCESSING SUMMARY ===\n")
  cat("Input samples:", nrow(cpr_data), "\n")
  cat("Output records:", nrow(result), "\n")
  cat("Year range:", min(result$Year), "-", max(result$Year), "\n")
  cat("Seasons:", paste(unique(result$Season), collapse = ", "), "\n")
  
  if ("region" %in% names(result)) {
    cat("Spatial regions:", length(unique(result$region)), "\n")
  }
  
  # Data quality summary
  cat("\n=== DATA QUALITY ===\n")
  cat("Mean samples per record:", round(mean(result$sample_count), 1), "\n")
  cat("Records with <5 samples:", sum(result$sample_count < 5), "\n")
  cat("Mean food availability:", round(mean(result$average_food), 1), "\n")
  
  return(result)
}


#--------------------------------------------------------------------------------------
## CPR testing
#--------------------------------------------------------------------------------------

# ==========================================
# SIMPLIFIED DATA QUALITY TESTING
# ==========================================

test_data_quality <- function(food_data) {
  
  cat("=== DATA QUALITY ASSESSMENT ===\n\n")
  
  # Test both average and total food
  cat("--- TESTING AVERAGE FOOD ---\n")
  avg_results <- assess_column(food_data, "average_food")
  
  cat("\n--- TESTING TOTAL FOOD ---\n")
  total_results <- assess_column(food_data, "total_food")
  
  # Simple recommendation
  cat("\n=== FINAL RECOMMENDATION ===\n")
  if (avg_results$usable && total_results$usable) {
    cat("✓ Both metrics usable - use average_food for standardized comparisons\n")
  } else if (avg_results$usable) {
    cat("! Use average_food - total_food has quality issues\n")
  } else if (total_results$usable) {
    cat("! Use total_food - average_food has quality issues\n")
  } else {
    cat("X Both metrics have issues - preprocessing required\n")
  }
  
  # Return nothing (invisible)
  invisible()
}

# Internal function to assess individual columns
assess_column <- function(food_data, test_column) {
  
  test_values <- food_data[[test_column]][is.finite(food_data[[test_column]])]
  
  # Basic metrics
  n <- length(test_values)
  zero_count <- sum(test_values == 0)
  zero_percent <- (zero_count / n) * 100
  
  if (n > 1) {
    cv <- sd(test_values, na.rm = TRUE) / mean(test_values, na.rm = TRUE)
    mean_val <- mean(test_values, na.rm = TRUE)
    median_val <- median(test_values, na.rm = TRUE)
  } else {
    cv <- NA
    mean_val <- test_values[1]
    median_val <- test_values[1]
  }
  
  # Outlier detection
  if (n >= 4) {
    Q1 <- quantile(test_values, 0.25, na.rm = TRUE)
    Q3 <- quantile(test_values, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    extreme_outliers <- sum(test_values < (Q1 - 3*IQR) | test_values > (Q3 + 3*IQR))
    extreme_outlier_percent <- (extreme_outliers / n) * 100
  } else {
    extreme_outlier_percent <- 0
  }
  
  # Quality assessment
  critical_issues <- c()
  major_issues <- c()
  minor_issues <- c()
  
  # Print quality checks as we go
  
  # Critical checks
  if (n < 10) {
    critical_issues <- c(critical_issues, "insufficient_sample_size")
    cat("X Sample size =", n, "(< 10) - Insufficient for robust statistics\n")
  } else {
    cat("✓ Sample size =", n, "(>= 10)\n")
  }
  
  # Major checks
  if (zero_percent > 30) {
    major_issues <- c(major_issues, "excessive_zeros")
    cat("X Zero values =", round(zero_percent, 1), "% (> 30%) - Excessive zeros\n")
  } else if (zero_percent > 15) {
    minor_issues <- c(minor_issues, "moderate_zeros")
    cat("! Zero values =", round(zero_percent, 1), "% (> 15%) - Moderate level\n")
  } else {
    cat("✓ Zero values =", round(zero_percent, 1), "% (<= 15%)\n")
  }
  
  if (is.finite(cv)) {
    if (cv > 4) {
      major_issues <- c(major_issues, "extreme_variability")
      cat("X CV =", round(cv, 2), "(> 4.0) - Extreme variability\n")
    } else if (cv > 2.5) {
      minor_issues <- c(minor_issues, "high_variability")
      cat("! CV =", round(cv, 2), "(> 2.5) - High but acceptable\n")
    } else {
      cat("✓ CV =", round(cv, 2), "(<= 2.5)\n")
    }
  }
  
  if (extreme_outlier_percent > 10) {
    major_issues <- c(major_issues, "excessive_extreme_outliers")
    cat("X Extreme outliers =", round(extreme_outlier_percent, 1), "% (> 10%) - Likely data errors\n")
  } else {
    cat("✓ Outliers =", round(extreme_outlier_percent, 1), "% (<= 10%) - Acceptable range\n")
  }
  
  # Mean-median ratio check
  if (is.finite(mean_val) && is.finite(median_val) && median_val > 0) {
    mean_median_ratio <- mean_val / median_val
    if (mean_median_ratio > 5 || mean_median_ratio < 0.2) {
      minor_issues <- c(minor_issues, "extreme_skewness")
      cat("! Mean/Median ratio =", round(mean_median_ratio, 2), "- Highly skewed\n")
    } else {
      cat("✓ Mean/Median ratio =", round(mean_median_ratio, 2), "- Acceptable skewness\n")
    }
  }
  
  # Negative values check
  min_val <- min(test_values, na.rm = TRUE)
  if (min_val < 0) {
    major_issues <- c(major_issues, "negative_values")
    cat("X Negative values detected (min =", min_val, ") - Invalid for abundance\n")
  } else {
    cat("✓ All values >= 0 - Appropriate for abundance data\n")
  }
  
  # Final determination
  if (length(critical_issues) > 0) {
    usable <- FALSE
    cat("X QUALITY: UNUSABLE - Critical issues detected\n")
  } else if (length(major_issues) > 2) {
    usable <- FALSE  
    cat("X QUALITY: UNUSABLE - Too many major issues\n")
  } else if (length(major_issues) > 0) {
    usable <- TRUE
    cat("! QUALITY: USABLE WITH CAUTION - Minor issues detected\n")
  } else if (length(minor_issues) > 3) {
    usable <- TRUE
    cat("! QUALITY: USABLE - Multiple minor issues but acceptable\n")
  } else {
    usable <- TRUE
    cat("✓ QUALITY: GOOD - Suitable for analysis\n")
  }
  
  return(list(usable = usable))
}


#--------------------------------------------------------------------------------------
## Subset environmental data by components with shapefile
#--------------------------------------------------------------------------------------

subset_environmental_data_shapefile <- function(combined_data, shapefile_path, area_column = "name", remove_na = TRUE) {
  
  ### Load shapefile
  areas_sf <- st_read(shapefile_path)
  
  ### Create spatial points from lon/lat coordinates
  coords_df <- expand.grid(lon = combined_data$lon, lat = combined_data$lat)
  coords_sf <- st_as_sf(coords_df, coords = c("lon", "lat"), crs = st_crs(areas_sf))
  
  ### Initialise list
  environmental_subsets_by_area <- list()
  
  ### Loop through each polygon in the shapefile
  for (i in 1:nrow(areas_sf)) {
    area_name <- areas_sf[[area_column]][i]
    
    ### Find points that fall within this polygon
    within_area <- st_within(coords_sf, areas_sf[i, ], sparse = FALSE)[, 1]
    
    ### Convert logical vector back to lon/lat indices
    within_indices <- which(within_area)
    
    if (length(within_indices) == 0) {
      warning(paste("No data points found within area:", area_name))
      next}
    
    ### Extract lon/lat indices from the grid positions
    lon_indices <- ((within_indices - 1) %% length(combined_data$lon)) + 1
    lat_indices <- ((within_indices - 1) %/% length(combined_data$lon)) + 1
    
    ### Get unique indices
    unique_lon_indices <- sort(unique(lon_indices))
    unique_lat_indices <- sort(unique(lat_indices))
    
    ### Initial subset of the environmental arrays
    subset_SST <- combined_data$SST[unique_lon_indices, unique_lat_indices, , drop = FALSE]
    subset_SBT <- combined_data$SBT[unique_lon_indices, unique_lat_indices, , drop = FALSE]
    subset_SSS <- combined_data$SSS[unique_lon_indices, unique_lat_indices, , drop = FALSE]
    subset_lon <- combined_data$lon[unique_lon_indices]
    subset_lat <- combined_data$lat[unique_lat_indices]
    
    ### Remove NA locations if requested
    if (remove_na) {
      ### Identify grid cells that have data (not all NA across time)
      ### Check if any time slice has non-NA values for any variable
      has_data <- array(FALSE, dim = c(length(unique_lon_indices), length(unique_lat_indices)))
      
      for (lon_idx in 1:length(unique_lon_indices)) {
        for (lat_idx in 1:length(unique_lat_indices)) {
          ### Check if this location has any non-NA values across all time steps
          sst_vals <- subset_SST[lon_idx, lat_idx, ]
          sbt_vals <- subset_SBT[lon_idx, lat_idx, ]
          sss_vals <- subset_SSS[lon_idx, lat_idx, ]
          
          ### Location has data if any variable has at least one non-NA value
          has_data[lon_idx, lat_idx] <- any(!is.na(sst_vals)) | 
            any(!is.na(sbt_vals)) | 
            any(!is.na(sss_vals))}}
      
      ### Find indices of locations with data
      valid_lon_indices <- which(apply(has_data, 1, any))
      valid_lat_indices <- which(apply(has_data, 2, any))
      
      ### Check if any valid data remains
      if (length(valid_lon_indices) == 0 || length(valid_lat_indices) == 0) {
        warning(paste("No valid data points (all NAs) found within area:", area_name))
        next}
      
      ### Subset to only valid locations
      subset_SST <- subset_SST[valid_lon_indices, valid_lat_indices, , drop = FALSE]
      subset_SBT <- subset_SBT[valid_lon_indices, valid_lat_indices, , drop = FALSE]
      subset_SSS <- subset_SSS[valid_lon_indices, valid_lat_indices, , drop = FALSE]
      subset_lon <- subset_lon[valid_lon_indices]
      subset_lat <- subset_lat[valid_lat_indices]
      
      ### Report how many locations were removed
      n_removed <- (length(unique_lon_indices) * length(unique_lat_indices)) - 
        (length(valid_lon_indices) * length(valid_lat_indices))
      if (n_removed > 0) {
        message(paste("Removed", n_removed, "NA-only grid cells from area:", area_name))}}
    
    ### Create final subset list
    subset_env_list <- list(
      SST = subset_SST,
      SBT = subset_SBT,
      SSS = subset_SSS,
      lon = subset_lon,
      lat = subset_lat,
      years = combined_data$years,
      months = combined_data$months,
      dates = combined_data$dates,
      area_polygon = areas_sf[i, ])  # Store the polygon for reference
    
    ### Store data in the 'environmental_subsets_by_area' list
    environmental_subsets_by_area[[area_name]] <- subset_env_list}
  
  return(environmental_subsets_by_area)}


#--------------------------------------------------------------------------------------
## Plot environmental data
#--------------------------------------------------------------------------------------

plot_env_data <- function(env_df_full, env_df_subset, 
                          effort_data = NULL, copepod_data = NULL) {
  
  # Setup
  areas <- if(is.factor(env_df_subset$Region)) levels(env_df_subset$Region) else unique(env_df_subset$Region)
  column_names <- c("Full Stock", areas)
  colors <- setNames(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E"), column_names)
  
  # Simple placeholder data generation
  generate_placeholder <- function(n_rows) rep(1, n_rows)
  
  # Create time series plot
  create_ts_plot <- function(data, y_var, y_label, column_name = NULL, 
                             is_bottom = FALSE, is_first_col = FALSE, color = "black") {
    
    p <- ggplot(data, aes(x = year, y = !!sym(y_var))) +
      geom_line(color = color, size = 0.6) +
      geom_point(color = "grey20", size = 1) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.3),
        panel.background = element_rect(fill = "white", color = "grey80"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.title.x = if(is_bottom) element_text(size = 9) else element_blank(),
        axis.text.x = if(is_bottom) element_text(size = 8) else element_blank(),
        axis.title.y = if(is_first_col) element_text(size = 9) else element_blank(),
        plot.margin = margin(5, 5, 5, 5)
      ) +
      labs(
        x = if(is_bottom) "Year" else "",
        y = if(is_first_col) y_label else "")
    
    # Add column header
    if (!is.null(column_name)) {
      p <- p + annotate("text", x = Inf, y = Inf, label = column_name, 
                        hjust = 1.1, vjust = 1.5, size = 3, fontface = "bold")}
    
    return(p)}
  
  # Create plots for each column
  create_column_plots <- function(data_env, column_name, is_first_col = FALSE) {
    color <- colors[[column_name]]
    n_rows <- nrow(data_env)
    
    # Get or generate effort data
    effort_vals <- if (!is.null(effort_data) && column_name %in% names(effort_data)) {
      effort_data[[column_name]]
    } else {
      generate_placeholder(n_rows)}
    
    # Get or generate copepod data
    copepod_vals <- if (!is.null(copepod_data) && column_name %in% names(copepod_data)) {
      copepod_data[[column_name]]
    } else {
      generate_placeholder(n_rows)}
    
    # Create data frames
    effort_df <- data.frame(year = data_env$year, effort = effort_vals)
    copepod_df <- data.frame(year = data_env$year, copepod = copepod_vals)
    
    # Variable mappings for environmental data
    env_vars <- if (column_name == "Full Stock") {
      list(sst = "mean_SST", sbt = "mean_SBT", sss = "mean_SSS")
    } else {
      list(sst = "Mean_SST", sbt = "Mean_SBT", sss = "Mean_SSS")}
    
    # Create all plots
    plots <- list(
      create_ts_plot(effort_df, "effort", "Effort (thousands hours)", 
                     if(is_first_col) column_name else column_name, FALSE, is_first_col, color),
      create_ts_plot(copepod_df, "copepod", expression("Big Copepod (ind m"^"-3"*")"), 
                     NULL, FALSE, is_first_col, color),
      create_ts_plot(data_env, env_vars$sst, "SST (°C)", 
                     NULL, FALSE, is_first_col, color),
      create_ts_plot(data_env, env_vars$sbt, "SBT (°C)", 
                     NULL, FALSE, is_first_col, color),
      create_ts_plot(data_env, env_vars$sss, "SSS (PSU)", 
                     NULL, TRUE, is_first_col, color))
    
    return(wrap_plots(plots, ncol = 1))}
  
  # Generate all columns
  plot_list <- list(create_column_plots(env_df_full, "Full Stock", TRUE))
  
  for (area in areas) {
    area_data <- env_df_subset %>% filter(Region == area) %>% arrange(year)
    plot_list <- append(plot_list, list(create_column_plots(area_data, area, FALSE)))}
  
  # Combine into final plot
  return(wrap_plots(plot_list, nrow = 1))}


# Functions for "data_analysis.Rmd"
#--------------------------------------------------------------------------------------
## Analyse SSB change-points
#--------------------------------------------------------------------------------------

changepoint_analysis <- function(data, 
                                 ssb_col = "SSB_component", 
                                 year_col = "year",
                                 region_name = "Region",
                                 Q = 6,
                                 bcp_threshold = 0.7,
                                 consensus_tolerance = 1,
                                 min_years_between = 5,
                                 plot_results = TRUE) {
  
  # Print region being analyzed
  cat("=== Changepoint Analysis for", region_name, "===\n")
  
  # Validate inputs
  if (!ssb_col %in% names(data)) {
    stop("SSB column '", ssb_col, "' not found in data")
  }
  if (!year_col %in% names(data)) {
    stop("Year column '", year_col, "' not found in data")
  }
  
  # Remove rows with missing values
  analysis_data <- data[complete.cases(data[c(ssb_col, year_col)]), ]
  
  if(nrow(analysis_data) < nrow(data)) {
    cat("# Removed", nrow(data) - nrow(analysis_data), "rows with missing values\n")
  }
  
  cat("# Data range:", range(analysis_data[[year_col]])[1], "-", 
      range(analysis_data[[year_col]])[2], "\n")
  cat("# Analysis parameters: Q =", Q, ", BCP threshold =", bcp_threshold, "\n")
  
  # CPT Analysis (BinSeg method)
  cat("\n## CPT Analysis (BinSeg)\n")
  ssbcpts <- cpt.mean(data = analysis_data[[ssb_col]], method = "BinSeg", Q = Q)
  cpt_indices <- cpts(ssbcpts)
  cpt_years <- analysis_data[[year_col]][cpt_indices]
  
  cat("# CPT changepoint indices:", paste(cpt_indices, collapse = ", "), "\n")
  cat("# CPT changepoint years:", paste(cpt_years, collapse = ", "), "\n")
  
  if(plot_results) {
    plot(ssbcpts, type = "l", cpt.col = "navyblue", 
         xlab = "Index", pt.width = 4,
         main = paste("CPT Analysis -", region_name))
  }
  
  # BCP Analysis
  cat("\n## BCP Analysis\n")
  bcp.ssb <- bcp(analysis_data[[ssb_col]])
  bcp_indices <- which(bcp.ssb$posterior.prob >= bcp_threshold)
  bcp_years <- analysis_data[[year_col]][bcp_indices]
  
  cat("# BCP changepoint indices:", paste(bcp_indices, collapse = ", "), "\n")
  cat("# BCP changepoint years:", paste(bcp_years, collapse = ", "), "\n")
  
  if(plot_results) {
    plot(bcp.ssb, main = paste("BCP Analysis -", region_name))
  }
  
  # Consensus Analysis
  cat("\n## Consensus Analysis\n")
  consensus_years <- c()
  
  # Find consensus points (within tolerance)
  for (cpt_year in cpt_years) {
    if (any(abs(bcp_years - cpt_year) <= consensus_tolerance)) {
      consensus_years <- c(consensus_years, cpt_year)
    }
  }
  
  # Apply minimum spacing rule
  if (length(consensus_years) > 1) {
    consensus_years <- sort(consensus_years)
    filtered_consensus <- consensus_years[1]
    
    for (i in 2:length(consensus_years)) {
      if (consensus_years[i] - tail(filtered_consensus, 1) >= min_years_between) {
        filtered_consensus <- c(filtered_consensus, consensus_years[i])
      }
    }
    consensus_years <- filtered_consensus
  }
  
  # Identify method-specific changepoints
  cpt_only_years <- setdiff(cpt_years, consensus_years)
  bcp_only_years <- setdiff(bcp_years, consensus_years)
  
  cat("# Consensus changepoints (±", consensus_tolerance, "yr, min", min_years_between, "yr apart):", 
      paste(consensus_years, collapse = ", "), "\n")
  
  if (length(cpt_only_years) > 0) {
    cat("# CPT only:", paste(cpt_only_years, collapse = ", "), "\n")
  }
  if (length(bcp_only_years) > 0) {
    cat("# BCP only:", paste(bcp_only_years, collapse = ", "), "\n")
  }
  
  # Create results structure
  results <- list(
    region = region_name,
    parameters = list(
      Q = Q,
      bcp_threshold = bcp_threshold,
      consensus_tolerance = consensus_tolerance,
      min_years_between = min_years_between
    ),
    data_info = list(
      n_observations = nrow(analysis_data),
      year_range = range(analysis_data[[year_col]]),
      ssb_range = range(analysis_data[[ssb_col]], na.rm = TRUE)
    ),
    cpt_analysis = list(
      model = ssbcpts,
      changepoint_indices = cpt_indices,
      changepoint_years = cpt_years,
      n_changepoints = length(cpt_years)
    ),
    bcp_analysis = list(
      model = bcp.ssb,
      changepoint_indices = bcp_indices,
      changepoint_years = bcp_years,
      n_changepoints = length(bcp_years),
      threshold_used = bcp_threshold
    ),
    consensus = list(
      changepoint_years = consensus_years,
      cpt_only_years = cpt_only_years,
      bcp_only_years = bcp_only_years,
      n_consensus = length(consensus_years),
      criteria = paste("Both methods ±", consensus_tolerance, "year, min", min_years_between, "years between points")
    )
  )
  
  cat("\n")
  return(results)
}


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
    labs(title = component, 
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
## Breakpoint analysis
#--------------------------------------------------------------------------------------

breakpoint_analysis <- function(data, ssb_col, f_col, year_col, 
                                lag_years = 1, 
                                region_name = "Region", 
                                plot_breakpoints = TRUE) {
  
  # Print region being analyzed
  cat("=== Analysis for", region_name, "===\n")
  cat("# Using", lag_years, "year lag\n")
  
  # Remove rows with missing values for analysis
  analysis_data <- data[complete.cases(data[c(ssb_col, f_col, "SSB_lag")]), ]
  
  if(nrow(analysis_data) < nrow(data)) {
    cat("# Removed", nrow(data) - nrow(analysis_data), "rows with missing values\n")}
  
  # Break-point analysis
  bpts <- strucchange::breakpoints(analysis_data$SSB_lag/1000000 ~ analysis_data[[f_col]])
  
  if(plot_breakpoints) {
    plot(bpts, main = paste("Breakpoints for", region_name))}
  
  # Get summary and find optimal breaks
  bpts_sum <- summary(bpts)
  opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
  
  cat("# Optimal number of breaks:", opt_brks[1], "\n")
  
  # Get breakpoints with optimal number of breaks
  bpts2 <- strucchange::breakpoints(bpts, breaks = opt_brks[1])
  best_brk <- analysis_data[[f_col]][bpts2$breakpoints]
  
  cat("# Best breakpoint F values:\n")
  cat("####", paste(round(best_brk, 3), collapse = " "), "\n")
  
  # Get breakpoint years
  best_brk_years <- analysis_data[[year_col]][bpts2$breakpoints]
  
  cat("# Best breakpoint years:\n") 
  cat("####", paste(best_brk_years, collapse = ", "), "\n")
  
  # Create confidence interval plot
  par(mfrow = c(1,1))
  ci_mod <- confint(bpts, breaks = opt_brks[1])
  
  plot(analysis_data$SSB_lag/1000000 ~ analysis_data[[f_col]], type = "p",
       xlab = "F", ylab = "SSB (millions)",
       main = paste("SSB vs F with Breakpoints -", region_name))
  
  # Add confidence interval lines
  for (i in 1:opt_brks[1]) {
    abline(v = analysis_data[[f_col]][ci_mod$confint[i,2]], col = "blue", lwd = 2)
    abline(v = analysis_data[[f_col]][ci_mod$confint[i,1]], col = "red", lty = 3)
    abline(v = analysis_data[[f_col]][ci_mod$confint[i,3]], col = "red", lty = 3)}
  
  legend("topright", legend = c("Best estimate", "Confidence limits"), 
         col = c("blue", "red"), lty = c(1, 3), lwd = c(2, 1))
  
  # Return results as a list
  results <- list(
    region = region_name,
    lag_used = lag_years,
    breakpoints = bpts2,
    optimal_breaks = opt_brks[1],
    break_f_values = best_brk,
    break_years = best_brk_years,
    confidence_intervals = ci_mod)
  
  cat("\n")
  return(results)}


#--------------------------------------------------------------------------------------
## Plot Hysteresis
#--------------------------------------------------------------------------------------

plot_hysteresis <- function(data, break_years, component,
                            msy_btrigger = 1130747, 
                            fmsy = 0.32,
                            show_msy_btrigger = TRUE,  # New parameter to control MSY B trigger display
                            show_fmsy = TRUE,           # Optional: also control F_MSY display
                            colors = c("steelblue", "darkorange", "purple", "lightgreen", "indianred"),
                            nudge_params = NULL) {      # New parameter for nudge parameters
  
  msy_btrigger <- as.numeric(msy_btrigger)
  fmsy <- as.numeric(fmsy)
  
  # Determine number of break years
  n_breaks <- length(break_years)
  n_phases <- n_breaks + 1
  
  # Set default nudge parameters if not provided
  if (is.null(nudge_params)) {
    nudge_params <- list(
      list(nudge_y = 0, nudge_x = 0.2),
      list(nudge_y = -0.5, nudge_x = 0.2),
      list(nudge_y = 0, nudge_x = -0.2),
      list(nudge_y = 0, nudge_x = 0.2)
    )
  }
  
  # Create phase assignment vector
  phase_assignment <- rep(1, nrow(data))
  for (i in 1:n_breaks) {
    phase_assignment[data$year > break_years[i]] <- i + 1
  }
  
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
      phase_data_list[[i]] <- data %>% filter(year > break_years[i-1] & year <= break_years[i])
    }
  }
  
  # Create the base plot
  p <- ggplot(data = data, aes(x = F, y = SSB_lag/1000000)) +
    geom_path(colour = "grey") +
    geom_point(colour = hyst_phases) +
    labs(title = component, x = "F", y = "SSB in millions") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Add MSY B trigger line and label only if show_msy_btrigger is TRUE
  if (show_msy_btrigger) {
    p <- p + 
      geom_hline(yintercept = msy_btrigger/1000000, linetype = "dashed", color = "gray30") +
      geom_label(x = 1, y = msy_btrigger/1000000, label = expression("MSY B"[trigger]), 
                 color = "gray30", size = 3.5, fontface = "bold")
  }
  
  # Add F_MSY line and label only if show_fmsy is TRUE
  if (show_fmsy) {
    p <- p + 
      geom_vline(xintercept = fmsy, linetype = "dashed", color = "gray30") +
      geom_label(x = fmsy, y = 0.5, label = expression("F"[MSY]), 
                 color = "gray30", size = 3.5)
  }
  
  # Add geom_smooth for each phase (only if data exists)
  for (i in 1:n_phases) {
    if (nrow(phase_data_list[[i]]) > 0) {
      p <- p + geom_smooth(data = phase_data_list[[i]], aes(x = F, y = SSB_lag/1000000),
                           method = "lm", colour = colors[i])
    }
  }
  
  # Add text labels for break years (only for existing break years)
  for (i in 1:n_breaks) {
    break_year_data <- data %>% filter(year == break_years[i])
    if (nrow(break_year_data) > 0) {
      nudge_y <- if (i <= length(nudge_params)) nudge_params[[i]]$nudge_y else 0
      nudge_x <- if (i <= length(nudge_params)) nudge_params[[i]]$nudge_x else 0
      
      p <- p + geom_text_repel(data = break_year_data, aes(label = year),
                               point.padding = 0.2, nudge_y = nudge_y, nudge_x = nudge_x,
                               size = 3, col = "black", segment.size = 0.2)
    }
  }
  
  # Add labels for first and last years
  # Use the last element of nudge_params for start/end years, or default values
  end_nudge_y <- if (length(nudge_params) >= 5) nudge_params[[5]]$nudge_y else 0
  end_nudge_x <- if (length(nudge_params) >= 5) nudge_params[[5]]$nudge_x else -0.1
  
  p <- p + geom_text_repel(data = data[c(1, length(data$year)),], aes(label = year),
                           point.padding = 0.2, nudge_y = end_nudge_y, nudge_x = end_nudge_x,
                           size = 3, col = "black", segment.size = 0.2)
  
  return(p)
}


#--------------------------------------------------------------------------------------
## Extract SSR Breakpoints
#--------------------------------------------------------------------------------------

srr_breakpoint_analysis <- function(data, ssb_col = "SSB", r_col = "Recruitment", year_col = "year",
                                    region_name = "Region", plot_breakpoints = TRUE, 
                                    method = "strucchange", initial_psi = NULL) {
  
  # Print region being analyzed
  cat("=== SRR Analysis for", region_name, "===\n")
  cat("# Method:", method, "\n")
  
  # Remove rows with missing values for analysis
  analysis_data <- data[complete.cases(data[c(ssb_col, r_col, year_col)]), ]
  
  if(nrow(analysis_data) < nrow(data)) {
    cat("# Removed", nrow(data) - nrow(analysis_data), "rows with missing values\n")
  }
  
  # Initialize results list
  results <- list(
    region = region_name,
    method = method,
    data_used = analysis_data
  )
  
  if(method == "strucchange") {
    # Original strucchange analysis
    bpts_SRR <- strucchange::breakpoints(analysis_data[[r_col]] ~ analysis_data[[ssb_col]])
    
    if(plot_breakpoints) {
      plot(bpts_SRR, main = paste("SRR Breakpoints for", region_name))
    }
    
    # Get summary and find optimal breaks
    bpts_SRR_sum <- summary(bpts_SRR)
    opt_brks_SRR <- opt_bpts(bpts_SRR_sum$RSS["BIC",])
    
    cat("# Optimal number of breaks:", opt_brks_SRR[1], "\n")
    
    # Get breakpoints with optimal number of breaks
    bpts2_SRR <- strucchange::breakpoints(bpts_SRR, breaks = opt_brks_SRR[1])
    best_brk_SRR <- analysis_data[[ssb_col]][bpts2_SRR$breakpoints]
    
    cat("# Best breakpoint SSB values:\n")
    cat("#", paste(round(best_brk_SRR, 1), collapse = ", "), "\n")
    
    # Get breakpoint years
    best_brk_years_SRR <- analysis_data[[year_col]][bpts2_SRR$breakpoints]
    
    cat("# Best breakpoint years:\n") 
    cat("#", paste(best_brk_years_SRR, collapse = ", "), "\n")
    
    # Create stock-recruitment plot with breakpoints
    par(mfrow = c(1,1))
    ci_mod_SRR <- confint(bpts_SRR, breaks = opt_brks_SRR[1])
    
    plot(analysis_data[[r_col]] ~ analysis_data[[ssb_col]], type = "p",
         xlab = "SSB", ylab = "Recruitment (R)",
         main = paste("Stock-Recruitment Relationship with Breakpoints -", region_name))
    
    # Add confidence interval lines
    for (i in 1:opt_brks_SRR[1]) {
      abline(v = analysis_data[[ssb_col]][ci_mod_SRR$confint[i,2]], col = "blue", lwd = 2)
      abline(v = analysis_data[[ssb_col]][ci_mod_SRR$confint[i,1]], col = "red", lty = 3)
      abline(v = analysis_data[[ssb_col]][ci_mod_SRR$confint[i,3]], col = "red", lty = 3)
    }
    
    legend("topright", legend = c("Best estimate", "Confidence limits"), 
           col = c("blue", "red"), lty = c(1, 3), lwd = c(2, 1))
    
    # Add strucchange-specific results to output
    results <- c(results, list(
      breakpoints = bpts2_SRR,
      optimal_breaks = opt_brks_SRR[1],
      break_ssb_values = best_brk_SRR,
      break_years = best_brk_years_SRR,
      confidence_intervals = ci_mod_SRR,
      summary = bpts_SRR_sum
    ))
    
  } else if(method == "segmented") {
    # Segmented model analysis
    cat("# Running segmented model analysis\n")
    
    # Set initial psi value
    if(is.null(initial_psi)) {
      initial_psi <- mean(analysis_data[[ssb_col]], na.rm = TRUE)
      cat("# Using mean SSB as initial psi:", round(initial_psi, 2), "\n")
    } else {
      cat("# Using provided initial psi:", round(initial_psi, 2), "\n")
    }
    
    # Fit linear model first
    lm_model <- lm(formula(paste(r_col, "~", ssb_col)), data = analysis_data)
    
    # Fit segmented model
    tryCatch({
      seg_formula <- formula(paste("~", ssb_col))
      seg_model <- segmented::segmented(lm_model, seg.Z = seg_formula, psi = initial_psi)
      
      # Extract results
      seg_summary <- summary(seg_model)
      breakpoint_ssb <- seg_model$psi[2]  # breakpoint estimate
      breakpoint_se <- seg_model$psi[3]   # standard error
      
      # Find corresponding year for breakpoint
      breakpoint_year <- analysis_data[[year_col]][which.min(abs(analysis_data[[ssb_col]] - breakpoint_ssb))]
      
      cat("# Segmented model breakpoint:\n")
      cat("# SSB breakpoint:", round(breakpoint_ssb, 1), "±", round(breakpoint_se, 1), "\n")
      cat("# Approximate year:", breakpoint_year, "\n")
      
      # Create plot with segmented fit
      if(plot_breakpoints) {
        plot(analysis_data[[ssb_col]], analysis_data[[r_col]], type = "p",
             xlab = "SSB", ylab = "Recruitment (R)",
             main = paste("Segmented Stock-Recruitment Relationship -", region_name))
        
        # Add fitted line
        plot(seg_model, add = TRUE, col = "red", lwd = 2)
        
        # Add breakpoint line
        abline(v = breakpoint_ssb, col = "blue", lwd = 2, lty = 2)
        
        # Add confidence interval for breakpoint
        abline(v = breakpoint_ssb - 1.96 * breakpoint_se, col = "red", lty = 3)
        abline(v = breakpoint_ssb + 1.96 * breakpoint_se, col = "red", lty = 3)
        
        legend("topright", 
               legend = c("Segmented fit", "Breakpoint", "95% CI"), 
               col = c("red", "blue", "red"), 
               lty = c(1, 2, 3), 
               lwd = c(2, 2, 1))
      }
      
      # Add segmented-specific results to output
      results <- c(results, list(
        segmented_model = seg_model,
        linear_model = lm_model,
        breakpoint_ssb = breakpoint_ssb,
        breakpoint_se = breakpoint_se,
        breakpoint_year = breakpoint_year,
        fitted_values = seg_model$fitted.values,
        coefficients = coef(seg_model),
        summary = seg_summary,
        initial_psi = initial_psi
      ))
      
    }, error = function(e) {
      cat("# Error in segmented analysis:", e$message, "\n")
      cat("# Try adjusting the initial_psi value\n")
      
      results <<- c(results, list(
        error = e$message,
        initial_psi = initial_psi
      ))
    })
    
  } else {
    stop("Method must be either 'strucchange' or 'segmented'")
  }
  
  cat("\n")
  return(results)
}


#--------------------------------------------------------------------------------------
## Plot SRR
#--------------------------------------------------------------------------------------

plot_SRR <- function(data, break_years, title_stock, used_model,
                     ssb_col = "SSB", r_col = "Recruitment", year_col = "year",
                     Blim = 828874,
                     show_Blim = TRUE,  # New argument to control Blim display
                     colors = c("steelblue", "darkorange", "purple", "lightgreen", "indianred", "darkred", "darkblue"),
                     nudge_params = NULL) {  # New argument for nudge parameters
  
  # Check if specified columns exist in the data
  required_cols <- c(ssb_col, r_col, year_col)
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Determine number of break years and resulting phases
  n_breaks <- length(break_years)
  n_phases <- n_breaks + 1
  
  # Create phase column based on break years
  data$phase <- 1
  for (i in 1:n_breaks) {
    data$phase[data[[year_col]] > break_years[i]] <- i + 1
  }
  
  # Set default nudge parameters if not provided
  if (is.null(nudge_params)) {
    nudge_params <- list(
      list(nudge_y = -5, nudge_x = -0.2), # first year
      list(nudge_y = 0, nudge_x = -0.5),  # breakpoint
      list(nudge_y = -5, nudge_x = -0.2), # last year
      list(nudge_y = 5, nudge_x = -0.2),  # more breakpoints
      list(nudge_y = -0, nudge_x = -0.2)
    )
  }
  
  # Create the plot using the specified column names
  p <- ggplot(data = data, aes(x = .data[[ssb_col]] / 1000000, y = .data[[r_col]] / 1000000)) +
    geom_path(colour = "grey") +
    geom_point(aes(color = factor(phase))) +
    scale_color_manual(values = colors[1:n_phases]) +
    labs(title = title_stock, 
         subtitle = used_model,
         x = "SSB in million t", 
         y = "R in billions") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
          plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
          legend.position = "none",
          axis.text = element_text(size = 12))
  
  # Add Blim line and label only if show_Blim is TRUE
  if (show_Blim) {
    p <- p + 
      geom_vline(xintercept = Blim / 1000000, linetype = "dashed", color = "gray30") +
      geom_label(x = Blim / 1000000, y = 7, label = expression("B"[lim]),
                 color = "gray30", size = 3.5, fontface = "bold")
  }
  
  # Add geom_smooth for each phase
  for (i in 1:n_phases) {
    phase_data <- data %>% dplyr::filter(phase == i)
    if (nrow(phase_data) > 0) {
      p <- p + geom_smooth(data = phase_data,
                           mapping = aes(x = .data[[ssb_col]] / 1000000, 
                                         y = .data[[r_col]] / 1000000),
                           col = colors[i], method = "lm")
    }
  }
  
  # Add text labels for the first and last years
  p <- p + geom_text_repel(data = data[1, ], 
                           aes(label = .data[[year_col]]),
                           point.padding = 0.2, 
                           nudge_y = nudge_params[[1]]$nudge_y,
                           nudge_x = nudge_params[[1]]$nudge_x,
                           size = 3, col = "gray30", segment.size = 0.2) +
    geom_text_repel(data = data[nrow(data)-1, ], 
                    aes(label = .data[[year_col]]),
                    point.padding = 0.2, 
                    nudge_y = nudge_params[[3]]$nudge_y,
                    nudge_x = nudge_params[[3]]$nudge_x,
                    size = 3, col = "gray30", segment.size = 0.2)
  
  # Add text labels for each breakpoint year
  for (i in 1:n_breaks) {
    brk_year <- break_years[i]
    break_year_data <- data %>% dplyr::filter(.data[[year_col]] == brk_year)
    if (nrow(break_year_data) > 0) {
      nudge_idx <- (i %% length(nudge_params)) + 1
      p <- p + geom_text_repel(data = break_year_data, 
                               aes(label = .data[[year_col]]),
                               point.padding = 0.2,
                               nudge_y = nudge_params[[nudge_idx]]$nudge_y,
                               nudge_x = nudge_params[[nudge_idx]]$nudge_x,
                               size = 3, col = "gray30", segment.size = 0.2)
    }
  }
  
  return(p)
}


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