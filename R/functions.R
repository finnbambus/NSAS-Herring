# Functions for "data_cleanup.Rmd"
#--------------------------------------------------------------------------------------
## Import and combine .nc files
#--------------------------------------------------------------------------------------

load_environmental_data <- function(sst_file = NULL, sbt_file = NULL, sss_file = NULL, 
                                             sst_var = NULL, sbt_var = NULL, sss_var = NULL,
                                             auto_detect = TRUE) {
  
  # Variable name patterns
  sst_patterns <- c("TEMP", "thetao", "sst", "temperature", "temp", "SST", "sea_surface_temperature")
  sbt_patterns <- c("bottomT", "sbt", "bottom_temperature", "SBT", "sea_bottom_temperature")
  sss_patterns <- c("PSAL", "so", "sss", "salinity", "SSS", "sea_surface_salinity", "salt")
  
  # Coordinate patterns
  lon_patterns <- c("longitude", "lon", "x", "LONGITUDE", "LON")
  lat_patterns <- c("latitude", "lat", "y", "LATITUDE", "LAT")
  time_patterns <- c("time", "t", "TIME", "T")
  
  env_data_full <- list()
  
  # Find variable in NetCDF file
  find_variable <- function(nc_file, patterns, specified_var = NULL) {
    if (!is.null(specified_var) && specified_var %in% names(nc_file$var)) {
      return(specified_var)
    }
    
    for (pattern in patterns) {
      if (pattern %in% names(nc_file$var)) {
        return(pattern)
      }
    }
    return(NULL)
  }
  
  # Find coordinate variables
  find_coordinate <- function(nc_file, patterns) {
    for (pattern in patterns) {
      if (pattern %in% names(nc_file$var)) {
        return(pattern)
      }
    }
    return(NULL)
  }
  
  # Time parsing function
  parse_time_robust <- function(time_values, time_units) {
    units_clean <- tolower(trimws(time_units))
    
    if (grepl("days since", units_clean)) {
      origin_part <- gsub(".*days since ", "", units_clean)
      origin_part <- gsub(" .*", "", origin_part)
      
      possible_formats <- c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y", "%Y%m%d")
      
      origin_date <- NULL
      for (fmt in possible_formats) {
        tryCatch({
          origin_date <- as.Date(origin_part, format = fmt)
          if (!is.na(origin_date)) break
        }, error = function(e) {})
      }
      
      if (is.null(origin_date) || is.na(origin_date)) {
        tryCatch({
          origin_date <- as.Date(origin_part)
        }, error = function(e) {
          origin_date <- as.Date("1900-01-01")
        })
      }
      
      dates <- as.Date(time_values, origin = origin_date)
      
    } else if (grepl("seconds since", units_clean)) {
      origin_part <- gsub(".*seconds since ", "", units_clean)
      origin_part <- gsub(" .*", "", origin_part)
      
      origin_date <- NULL
      possible_formats <- c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y", "%Y%m%d")
      
      for (fmt in possible_formats) {
        tryCatch({
          origin_date <- as.Date(origin_part, format = fmt)
          if (!is.na(origin_date)) break
        }, error = function(e) {})
      }
      
      if (is.null(origin_date) || is.na(origin_date)) {
        origin_date <- as.Date(origin_part)
        if (is.na(origin_date)) {
          origin_date <- as.Date("1970-01-01")
        }
      }
      
      days_from_origin <- time_values / (24 * 60 * 60)
      dates <- origin_date + days_from_origin
      
    } else if (grepl("hours since", units_clean)) {
      origin_part <- gsub(".*hours since ", "", units_clean)
      origin_part <- gsub(" .*", "", origin_part)
      
      origin_date <- as.Date(origin_part)
      if (is.na(origin_date)) {
        origin_date <- as.Date("1900-01-01")
      }
      
      dates <- as.Date(time_values / 24, origin = origin_date)
      
    } else {
      start_date <- as.Date("2000-01-01")
      dates <- seq(start_date, by = "month", length.out = length(time_values))
    }
    
    years <- as.numeric(format(dates, "%Y"))
    months <- as.numeric(format(dates, "%m"))
    
    return(list(dates = dates, years = years, months = months))
  }
  
  # Initialize coordinate and time variables
  lon <- NULL
  lat <- NULL
  dates <- NULL
  years <- NULL
  months <- NULL
  
  ### Load SST data
  if (!is.null(sst_file) && file.exists(sst_file)) {
    nc_sst <- nc_open(sst_file)
    
    sst_var_name <- if (auto_detect) {
      find_variable(nc_sst, sst_patterns, sst_var)
    } else {
      sst_var
    }
    
    if (!is.null(sst_var_name)) {
      sst_data_raw <- ncvar_get(nc_sst, sst_var_name)
      
      # Handle 4D data (extract surface layer)
      if (length(dim(sst_data_raw)) == 4) {
        sst_data_raw <- sst_data_raw[, , 1, ]
      }
      
      # Get coordinates from first file
      lon_var <- find_coordinate(nc_sst, lon_patterns)
      lat_var <- find_coordinate(nc_sst, lat_patterns)
      time_var <- find_coordinate(nc_sst, time_patterns)
      
      # Extract coordinates
      if (!is.null(lon_var)) {
        lon <- ncvar_get(nc_sst, lon_var)
      } else {
        tryCatch({
          lon_dim <- nc_sst$dim$longitude
          if (!is.null(lon_dim$vals) && length(lon_dim$vals) > 1) {
            lon <- lon_dim$vals
          }
        }, error = function(e) {
          lon <- seq(1, dim(sst_data_raw)[1])
        })
      }
      
      if (!is.null(lat_var)) {
        lat <- ncvar_get(nc_sst, lat_var)
      } else {
        tryCatch({
          lat_dim <- nc_sst$dim$latitude
          if (!is.null(lat_dim$vals) && length(lat_dim$vals) > 1) {
            lat <- lat_dim$vals
          }
        }, error = function(e) {
          lat <- seq(1, dim(sst_data_raw)[2])
        })
      }
      
      # Extract and parse time
      if (!is.null(time_var)) {
        time_raw <- ncvar_get(nc_sst, time_var)
        time_units_obj <- ncatt_get(nc_sst, time_var, "units")
        
        if (!is.null(time_units_obj) && !is.null(time_units_obj$value)) {
          time_units <- time_units_obj$value
          time_info <- parse_time_robust(time_raw, time_units)
          dates <- time_info$dates
          years <- time_info$years
          months <- time_info$months
        } else {
          n_time <- length(time_raw)
          dates <- seq(as.Date("2000-01-01"), by = "month", length.out = n_time)
          years <- as.numeric(format(dates, "%Y"))
          months <- as.numeric(format(dates, "%m"))
        }
      } else {
        # Try to get time from dimension
        if ("time" %in% names(nc_sst$dim)) {
          time_dim <- nc_sst$dim$time
          
          if (!is.null(time_dim$vals)) {
            time_raw <- time_dim$vals
            
            if (!is.null(time_dim$units)) {
              time_info <- parse_time_robust(time_raw, time_dim$units)
              dates <- time_info$dates
              years <- time_info$years
              months <- time_info$months
            } else {
              if (all(time_raw > -2e9 & time_raw < 2e9)) {
                time_info <- parse_time_robust(time_raw, "seconds since 1970-01-01 00:00:00")
                dates <- time_info$dates
                years <- time_info$years
                months <- time_info$months
              } else {
                n_time <- length(time_raw)
                dates <- seq(as.Date("2000-01-01"), by = "month", length.out = n_time)
                years <- as.numeric(format(dates, "%Y"))
                months <- as.numeric(format(dates, "%m"))
              }
            }
          } else {
            n_time <- time_dim$len
            dates <- seq(as.Date("2000-01-01"), by = "month", length.out = n_time)
            years <- as.numeric(format(dates, "%Y"))
            months <- as.numeric(format(dates, "%m"))
          }
        }
      }
      
      # Handle NAs
      sst_data_raw[is.nan(sst_data_raw)] <- NA
      sst_data_raw[sst_data_raw == -32768] <- NA
      
      env_data_full$SST <- sst_data_raw
      message(paste("Loaded SST data using variable:", sst_var_name))
    } else {
      message("Could not find SST variable in file. Available variables:")
      print(names(nc_sst$var))
    }
    
    nc_close(nc_sst)
  }
  
  ### Load SBT data
  if (!is.null(sbt_file) && file.exists(sbt_file)) {
    nc_sbt <- nc_open(sbt_file)
    
    sbt_var_name <- if (auto_detect) {
      find_variable(nc_sbt, sbt_patterns, sbt_var)
    } else {
      sbt_var
    }
    
    if (!is.null(sbt_var_name)) {
      sbt_data_raw <- ncvar_get(nc_sbt, sbt_var_name)
      
      # Handle 4D data
      if (length(dim(sbt_data_raw)) == 4) {
        sbt_data_raw <- sbt_data_raw[, , 1, ]
      }
      
      # Handle NAs
      sbt_data_raw[is.nan(sbt_data_raw)] <- NA
      sbt_data_raw[sbt_data_raw == -32768] <- NA
      
      env_data_full$SBT <- sbt_data_raw
      message(paste("Loaded SBT data using variable:", sbt_var_name))
    } else {
      message("Could not find SBT variable in file. Available variables:")
      print(names(nc_sbt$var))
    }
    
    nc_close(nc_sbt)
  }
  
  ### Load SSS data
  if (!is.null(sss_file) && file.exists(sss_file)) {
    nc_sss <- nc_open(sss_file)
    
    sss_var_name <- if (auto_detect) {
      find_variable(nc_sss, sss_patterns, sss_var)
    } else {
      sss_var
    }
    
    if (!is.null(sss_var_name)) {
      sss_data_raw <- ncvar_get(nc_sss, sss_var_name)
      
      # Handle 4D data
      if (length(dim(sss_data_raw)) == 4) {
        sss_data_raw <- sss_data_raw[, , 1, ]
      }
      
      # Handle NAs
      sss_data_raw[is.nan(sss_data_raw)] <- NA
      sss_data_raw[sss_data_raw == -32768] <- NA
      
      env_data_full$SSS <- sss_data_raw
      message(paste("Loaded SSS data using variable:", sss_var_name))
    } else {
      message("Could not find SSS variable in file. Available variables:")
      print(names(nc_sss$var))
    }
    
    nc_close(nc_sss)
  }
  
  # Add coordinate and time information
  env_data_full$lon <- lon
  env_data_full$lat <- lat
  
  # Ensure time variables exist
  if (is.null(years) || is.null(months) || is.null(dates)) {
    # Get time dimension from available data
    if ("SST" %in% names(env_data_full)) {
      n_time <- ifelse(length(dim(env_data_full$SST)) >= 3, dim(env_data_full$SST)[3], 1)
    } else if ("SSS" %in% names(env_data_full)) {
      n_time <- ifelse(length(dim(env_data_full$SSS)) >= 3, dim(env_data_full$SSS)[3], 1)
    } else {
      n_time <- 1
    }
    
    dates <- seq(as.Date("2000-01-01"), by = "month", length.out = n_time)
    years <- as.numeric(format(dates, "%Y"))
    months <- as.numeric(format(dates, "%m"))
  }
  
  env_data_full$years <- years
  env_data_full$months <- months
  env_data_full$dates <- dates
  
  # Summary message
  loaded_vars <- names(env_data_full)[names(env_data_full) %in% c("SST", "SBT", "SSS")]
  if (length(loaded_vars) > 0) {
    message(paste("Successfully loaded:", paste(loaded_vars, collapse = ", ")))
  } else {
    message("No environmental data variables were loaded successfully")
  }
  
  return(env_data_full)
}


#--------------------------------------------------------------------------------------
## Process env data
#--------------------------------------------------------------------------------------

process_env_data_seasonal <- function(env_data, 
                                      variables = c("SST", "SBT", "SSS"),
                                      shapefile_path = NULL,
                                      polygon_id_column = "NAME",
                                      remove_na = TRUE,
                                      include_sbt = TRUE) {
  
  # Filter variables based on include_sbt parameter
  if (!include_sbt) {
    variables <- variables[variables != "SBT"]
    if (length(variables) == 0) {
      stop("No variables to process after removing SBT")
    }
  }
  
  # Helper function to assign seasons based on month
  assign_season <- function(month) {
    case_when(
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer", 
      month %in% c(9, 10, 11) ~ "Autumn",
      month %in% c(12, 1, 2) ~ "Winter",
      TRUE ~ NA_character_)}
  
  # Get time dimension info
  years <- env_data$years
  months <- env_data$months
  n_time <- length(years)
  
  # Handle spatial aggregation if shapefile provided
  if (!is.null(shapefile_path)) {
    
    cat("Processing environmental data with spatial aggregation (efficient subsetting approach)...\n")
    cat("Variables to process:", paste(variables, collapse = ", "), "\n")
    
    # Validate spatial coordinates
    if (!all(c("lon", "lat") %in% names(env_data))) {
      stop("Environmental data must contain 'lon' and 'lat' arrays for spatial aggregation")}
    
    # Load shapefile
    areas_sf <- st_read(shapefile_path, quiet = TRUE)
    cat("Loaded", nrow(areas_sf), "polygons\n")
    
    # Create spatial points from lon/lat coordinates (once)
    coords_df <- expand.grid(lon = env_data$lon, lat = env_data$lat)
    coords_sf <- st_as_sf(coords_df, coords = c("lon", "lat"), crs = st_crs(areas_sf))
    cat("Created spatial grid:", nrow(coords_df), "points\n")
    
    # Initialize results list
    all_region_results <- list()
    
    # Loop through each polygon in the shapefile
    for (i in 1:nrow(areas_sf)) {
      area_name <- areas_sf[[polygon_id_column]][i]
      cat("Processing region:", area_name, "(", i, "of", nrow(areas_sf), ")\n")
      
      # Find points that fall within this polygon
      within_area <- st_within(coords_sf, areas_sf[i, ], sparse = FALSE)[, 1]
      within_indices <- which(within_area)
      
      if (length(within_indices) == 0) {
        warning(paste("No data points found within area:", area_name))
        next}
      
      # Extract lon/lat indices from the grid positions
      lon_indices <- ((within_indices - 1) %% length(env_data$lon)) + 1
      lat_indices <- ((within_indices - 1) %/% length(env_data$lon)) + 1
      
      # Get unique indices
      unique_lon_indices <- sort(unique(lon_indices))
      unique_lat_indices <- sort(unique(lat_indices))
      
      cat("  Spatial subset size:", length(unique_lon_indices), "x", length(unique_lat_indices), "=", 
          length(unique_lon_indices) * length(unique_lat_indices), "grid cells\n")
      
      # Process each variable for this region
      region_data_list <- list()
      
      for (var in variables) {
        if (!var %in% names(env_data)) {
          warning(paste("Variable", var, "not found in env_data"))
          next}
        
        # Subset the environmental array for this region
        subset_var <- env_data[[var]][unique_lon_indices, unique_lat_indices, , drop = FALSE]
        
        # Remove NA locations if requested
        if (remove_na) {
          # Identify grid cells that have data (not all NA across time)
          has_data <- array(FALSE, dim = c(length(unique_lon_indices), length(unique_lat_indices)))
          
          for (lon_idx in 1:length(unique_lon_indices)) {
            for (lat_idx in 1:length(unique_lat_indices)) {
              # Check if this location has any non-NA values across all time steps
              vals <- subset_var[lon_idx, lat_idx, ]
              has_data[lon_idx, lat_idx] <- any(!is.na(vals))}}
          
          # Find indices of locations with data
          valid_lon_indices <- which(apply(has_data, 1, any))
          valid_lat_indices <- which(apply(has_data, 2, any))
          
          # Check if any valid data remains
          if (length(valid_lon_indices) == 0 || length(valid_lat_indices) == 0) {
            warning(paste("No valid data points (all NAs) found for variable", var, "in area:", area_name))
            next}
          
          # Subset to only valid locations
          subset_var <- subset_var[valid_lon_indices, valid_lat_indices, , drop = FALSE]
          n_removed <- (length(unique_lon_indices) * length(unique_lat_indices)) - 
            (length(valid_lon_indices) * length(valid_lat_indices))
          if (n_removed > 0) {
            cat("    Removed", n_removed, "NA-only grid cells for", var, "\n")}}
        
        # Convert to long format efficiently
        dims <- dim(subset_var)
        
        if (prod(dims) > 0) {  # Check if we have any data
          # Create vectors for the data frame
          values <- as.vector(subset_var)
          years_rep <- rep(years, each = dims[1] * dims[2])
          months_rep <- rep(months, each = dims[1] * dims[2])
          
          # Remove NA values and corresponding indices
          valid_mask <- !is.na(values)
          
          if (sum(valid_mask) > 0) {
            var_df <- data.frame(
              Value = values[valid_mask],
              year = years_rep[valid_mask],
              month = months_rep[valid_mask],
              Variable = var,
              stringsAsFactors = FALSE)
            
            region_data_list[[var]] <- var_df}}}
      
      # Combine all variables for this region
      if (length(region_data_list) > 0) {
        region_data <- bind_rows(region_data_list)
        
        # Add region identifier and season
        region_data$Region <- area_name
        region_data$Season <- assign_season(region_data$month)
        
        # Remove rows with missing season
        region_data <- region_data[!is.na(region_data$Season), ]
        
        if (nrow(region_data) > 0) {
          all_region_results[[area_name]] <- region_data
          cat("  Processed", nrow(region_data), "valid data points\n")}}}
    
    # Combine all regions
    if (length(all_region_results) > 0) {
      cat("\nCombining results from", length(all_region_results), "regions...\n")
      all_data <- bind_rows(all_region_results)
      
      # Aggregate by region, year, season, and variable
      result <- all_data %>%
        group_by(Region, year, Season, Variable) %>%
        summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        # Remove incomplete last year if needed
        filter(year != max(year)) %>%
        # Pivot to wide format
        pivot_wider(names_from = Variable, values_from = Mean_Value, names_prefix = "Mean_") %>%
        arrange(Region, year, Season)
      
    } else {
      stop("No valid data found in any regions")}
    
    cat("Spatial aggregation complete.\n")
    cat("Regions found:", length(unique(result$Region)), "\n")
    
  } else {
    
    cat("Processing environmental data without spatial aggregation...\n")
    cat("Variables to process:", paste(variables, collapse = ", "), "\n")
    
    # Process each variable (non-spatial case) - optimized approach
    env_df_seasonal <- variables %>%
      map_dfr(~ {
        # Extract the 3D array for this variable
        var_data <- env_data[[.x]]
        
        if (is.null(var_data)) {
          warning(paste("Variable", .x, "not found in env_data"))
          return(NULL)}
        
        # Get dimensions [lon, lat, time]
        dims <- dim(var_data)
        
        # Create expanded data frame using the actual years and months from the data
        data.frame(
          Value = as.vector(var_data),
          year = rep(years, each = dims[1] * dims[2]),
          month = rep(months, each = dims[1] * dims[2]),
          Variable = .x
        ) %>%
          # Add season
          mutate(Season = assign_season(month)) %>%
          # Remove rows with missing data
          filter(!is.na(Value), !is.na(Season))})
    
    # Aggregate directly to seasonal data (no spatial separation)
    result <- env_df_seasonal %>%
      group_by(year, Season, Variable) %>%
      summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      # Remove incomplete last year if needed
      filter(year != max(year)) %>%
      # Pivot to wide format
      pivot_wider(names_from = Variable, values_from = Mean_Value, names_prefix = "Mean_") %>%
      arrange(year, Season)
    
    cat("Temporal aggregation complete.\n")}
  
  # Add summary information
  cat("\n=== ENVIRONMENTAL DATA PROCESSING SUMMARY ===\n")
  cat("Variables processed:", paste(variables, collapse = ", "), "\n")
  if (!include_sbt) {
    cat("SBT processing: DISABLED\n")
  }
  cat("Output records:", nrow(result), "\n")
  cat("Year range:", min(result$year), "-", max(result$year), "\n")
  cat("Seasons:", paste(unique(result$Season), collapse = ", "), "\n")
  
  if ("Region" %in% names(result)) {
    cat("Spatial regions:", length(unique(result$Region)), "\n")}
  
  return(result)}

# Helper function for the efficient spatial subsetting (standalone version)
subset_environmental_data_shapefile <- function(combined_data, 
                                                shapefile_path, 
                                                area_column = "name", 
                                                remove_na = TRUE,
                                                include_sbt = TRUE) {
  
  ### Load shapefile
  areas_sf <- st_read(shapefile_path)
  
  ### Create spatial points from lon/lat coordinates
  coords_df <- expand.grid(lon = combined_data$lon, lat = combined_data$lat)
  coords_sf <- st_as_sf(coords_df, coords = c("lon", "lat"), crs = st_crs(areas_sf))
  
  ### Initialize list
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
    subset_SSS <- combined_data$SSS[unique_lon_indices, unique_lat_indices, , drop = FALSE]
    subset_lon <- combined_data$lon[unique_lon_indices]
    subset_lat <- combined_data$lat[unique_lat_indices]
    
    # Only subset SBT if include_sbt is TRUE and SBT exists
    subset_SBT <- NULL
    if (include_sbt && "SBT" %in% names(combined_data) && !is.null(combined_data$SBT)) {
      subset_SBT <- combined_data$SBT[unique_lon_indices, unique_lat_indices, , drop = FALSE]
    }
    
    ### Remove NA locations if requested
    if (remove_na) {
      ### Identify grid cells that have data (not all NA across time)
      has_data <- array(FALSE, dim = c(length(unique_lon_indices), length(unique_lat_indices)))
      
      for (lon_idx in 1:length(unique_lon_indices)) {
        for (lat_idx in 1:length(unique_lat_indices)) {
          ### Check if this location has any non-NA values across all time steps
          sst_vals <- subset_SST[lon_idx, lat_idx, ]
          sss_vals <- subset_SSS[lon_idx, lat_idx, ]
          
          ### Start with SST and SSS
          has_data_here <- any(!is.na(sst_vals)) | any(!is.na(sss_vals))
          
          ### Add SBT check only if SBT is included and available
          if (include_sbt && !is.null(subset_SBT)) {
            sbt_vals <- subset_SBT[lon_idx, lat_idx, ]
            has_data_here <- has_data_here | any(!is.na(sbt_vals))
          }
          
          has_data[lon_idx, lat_idx] <- has_data_here
        }
      }
      
      ### Find indices of locations with data
      valid_lon_indices <- which(apply(has_data, 1, any))
      valid_lat_indices <- which(apply(has_data, 2, any))
      
      ### Check if any valid data remains
      if (length(valid_lon_indices) == 0 || length(valid_lat_indices) == 0) {
        warning(paste("No valid data points (all NAs) found within area:", area_name))
        next}
      
      ### Subset to only valid locations
      subset_SST <- subset_SST[valid_lon_indices, valid_lat_indices, , drop = FALSE]
      subset_SSS <- subset_SSS[valid_lon_indices, valid_lat_indices, , drop = FALSE]
      subset_lon <- subset_lon[valid_lon_indices]
      subset_lat <- subset_lat[valid_lat_indices]
      
      # Only subset SBT if it exists
      if (!is.null(subset_SBT)) {
        subset_SBT <- subset_SBT[valid_lon_indices, valid_lat_indices, , drop = FALSE]
      }
      
      ### Report how many locations were removed
      n_removed <- (length(unique_lon_indices) * length(unique_lat_indices)) - 
        (length(valid_lon_indices) * length(valid_lat_indices))
      if (n_removed > 0) {
        message(paste("Removed", n_removed, "NA-only grid cells from area:", area_name))}}
    
    ### Create final subset list
    subset_env_list <- list(
      SST = subset_SST,
      SSS = subset_SSS,
      lon = subset_lon,
      lat = subset_lat,
      years = combined_data$years,
      months = combined_data$months,
      dates = combined_data$dates,
      area_polygon = areas_sf[i, ])
    
    # Only add SBT to the list if it's included and available
    if (include_sbt && !is.null(subset_SBT)) {
      subset_env_list$SBT <- subset_SBT
    }
    
    ### Store data in the 'environmental_subsets_by_area' list
    environmental_subsets_by_area[[area_name]] <- subset_env_list}
  
  return(environmental_subsets_by_area)}


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
      TRUE ~ NA_character_)}
  
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
    
    cat("Temporal aggregation complete.\n")}
  
  # Step 3: Add summary information
  cat("\n=== PROCESSING SUMMARY ===\n")
  cat("Input samples:", nrow(cpr_data), "\n")
  cat("Output records:", nrow(result), "\n")
  cat("Year range:", min(result$Year), "-", max(result$Year), "\n")
  cat("Seasons:", paste(unique(result$Season), collapse = ", "), "\n")
  
  if ("region" %in% names(result)) {
    cat("Spatial regions:", length(unique(result$region)), "\n")}
  
  # Data quality summary
  cat("\n=== DATA QUALITY ===\n")
  cat("Mean samples per record:", round(mean(result$sample_count), 1), "\n")
  cat("Records with <5 samples:", sum(result$sample_count < 5), "\n")
  cat("Mean food availability:", round(mean(result$average_food), 1), "\n")
  
  return(result)}


#--------------------------------------------------------------------------------------
## CPR testing
#--------------------------------------------------------------------------------------

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
    cat("X Both metrics have issues - preprocessing required\n")}
  
  # Return nothing (invisible)
  invisible()}

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
    median_val <- test_values[1]}
  
  # Outlier detection
  if (n >= 4) {
    Q1 <- quantile(test_values, 0.25, na.rm = TRUE)
    Q3 <- quantile(test_values, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    extreme_outliers <- sum(test_values < (Q1 - 3*IQR) | test_values > (Q3 + 3*IQR))
    extreme_outlier_percent <- (extreme_outliers / n) * 100
  } else {
    extreme_outlier_percent <- 0}
  
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
    cat("✓ Sample size =", n, "(>= 10)\n")}
  
  # Major checks
  if (zero_percent > 30) {
    major_issues <- c(major_issues, "excessive_zeros")
    cat("X Zero values =", round(zero_percent, 1), "% (> 30%) - Excessive zeros\n")
  } else if (zero_percent > 15) {
    minor_issues <- c(minor_issues, "moderate_zeros")
    cat("! Zero values =", round(zero_percent, 1), "% (> 15%) - Moderate level\n")
  } else {
    cat("✓ Zero values =", round(zero_percent, 1), "% (<= 15%)\n")}
  
  if (is.finite(cv)) {
    if (cv > 4) {
      major_issues <- c(major_issues, "extreme_variability")
      cat("X CV =", round(cv, 2), "(> 4.0) - Extreme variability\n")
    } else if (cv > 2.5) {
      minor_issues <- c(minor_issues, "high_variability")
      cat("! CV =", round(cv, 2), "(> 2.5) - High but acceptable\n")
    } else {
      cat("✓ CV =", round(cv, 2), "(<= 2.5)\n")}}
  
  if (extreme_outlier_percent > 10) {
    major_issues <- c(major_issues, "excessive_extreme_outliers")
    cat("X Extreme outliers =", round(extreme_outlier_percent, 1), "% (> 10%) - Likely data errors\n")
  } else {
    cat("✓ Outliers =", round(extreme_outlier_percent, 1), "% (<= 10%) - Acceptable range\n")}
  
  # Mean-median ratio check
  if (is.finite(mean_val) && is.finite(median_val) && median_val > 0) {
    mean_median_ratio <- mean_val / median_val
    if (mean_median_ratio > 5 || mean_median_ratio < 0.2) {
      minor_issues <- c(minor_issues, "extreme_skewness")
      cat("! Mean/Median ratio =", round(mean_median_ratio, 2), "- Highly skewed\n")
    } else {
      cat("✓ Mean/Median ratio =", round(mean_median_ratio, 2), "- Acceptable skewness\n")
    }}
  
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
    cat("✓ QUALITY: GOOD - Suitable for analysis\n")}
  
  return(list(usable = usable))}


#--------------------------------------------------------------------------------------
## Plot environmental data
#--------------------------------------------------------------------------------------

plot_env_data_dual <- function(env_df_full_seasonal, env_df_subset_seasonal, 
                               cpr_full_data = NULL, cpr_component_data = NULL,
                               include_sbt = TRUE, threshold_values = NULL) {
  
  # Setup
  areas <- if(is.factor(env_df_subset_seasonal$Region)) levels(env_df_subset_seasonal$Region) else unique(env_df_subset_seasonal$Region)
  column_names <- c("Full Stock", areas)
  colors <- setNames(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E"), column_names)
  
  # Function to aggregate CPR data for both yearly and autumn data
  aggregate_cpr_data <- function(cpr_data, region_name = NULL, season_filter = "All") {
    if (is.null(cpr_data)) return(NULL)
    
    # Filter by season if specified
    if (season_filter != "All") {
      cpr_data <- cpr_data %>% filter(Season == season_filter)}
    
    if (is.null(region_name)) {
      # For full stock data
      if (season_filter == "All") {
        # Yearly averages across all seasons
        result <- cpr_data %>%
          group_by(Year) %>%
          summarise(average_food = mean(average_food, na.rm = TRUE), .groups = 'drop') %>%
          rename(year = Year)
      } else {
        # Seasonal data (already filtered above)
        result <- cpr_data %>%
          rename(year = Year) %>%
          dplyr::select(year, average_food)}
    } else {
      # For regional data
      if (season_filter == "All") {
        # Yearly averages across all seasons
        result <- cpr_data %>%
          filter(region == region_name) %>%
          group_by(Year) %>%
          summarise(average_food = mean(average_food, na.rm = TRUE), .groups = 'drop') %>%
          rename(year = Year)
      } else {
        # Seasonal data (already filtered above)
        result <- cpr_data %>%
          filter(region == region_name) %>%
          rename(year = Year) %>%
          dplyr::select(year, average_food)}}
    
    return(result)}
  
  # Function to get CPR data for a specific region/column
  get_cpr_data <- function(cpr_data, region_name, data_years, season_filter = "All") {
    if (is.null(cpr_data)) return(rep(1, length(data_years)))
    
    agg_data <- aggregate_cpr_data(cpr_data, region_name, season_filter)
    
    if (is.null(agg_data) || nrow(agg_data) == 0) {
      return(rep(1, length(data_years)))}
    
    # Match years and fill missing with interpolation only within data range
    matched_data <- data.frame(year = data_years) %>%
      left_join(agg_data, by = "year")
    
    # Only interpolate within the range of available data, leave gaps at ends
    if (any(is.na(matched_data$average_food)) && sum(!is.na(matched_data$average_food)) > 1) {
      data_range <- range(matched_data$year[!is.na(matched_data$average_food)])
      within_range <- matched_data$year >= data_range[1] & matched_data$year <= data_range[2]
      
      # Only interpolate for years within the data range
      interpolated_vals <- approx(x = matched_data$year[!is.na(matched_data$average_food)], 
                                  y = matched_data$average_food[!is.na(matched_data$average_food)], 
                                  xout = matched_data$year[within_range], 
                                  rule = 1)$y
      
      matched_data$average_food[within_range] <- interpolated_vals}
    
    return(matched_data$average_food)}
  
  # Helper function to create desaturated colors
  desaturate_color <- function(color, amount = 0.5) {
    # Convert to HSV and reduce saturation
    hsv_color <- rgb2hsv(col2rgb(color))
    hsv_color[2] <- hsv_color[2] * amount  # Reduce saturation
    return(hsv(hsv_color[1], hsv_color[2], hsv_color[3]))}
  
  # Create time series plot with dual layers (yearly + autumn) and optional threshold
  create_ts_plot_dual <- function(data_yearly, data_autumn, y_var_yearly, y_var_autumn, y_label, 
                                  column_name = NULL, is_bottom = FALSE, is_first_col = FALSE, 
                                  color = "black", threshold = NULL, var_type = NULL) {
    
    # Create desaturated color for yearly data
    color_yearly <- desaturate_color(color, 0.4)
    
    p <- ggplot() +
      # Yearly data (background layer with desaturated color and dashed line)
      geom_line(data = data_yearly, aes(x = year, y = !!sym(y_var_yearly)), 
                color = color_yearly, size = 0.5, linetype = "dashed", alpha = 0.7) +
      geom_point(data = data_yearly, aes(x = year, y = !!sym(y_var_yearly)), 
                 color = color_yearly, size = 0.7, alpha = 0.7) +
      # Autumn data (foreground layer with full color and solid line)
      geom_line(data = data_autumn, aes(x = year, y = !!sym(y_var_autumn)), 
                color = color, size = 0.8, alpha = 1.0) +
      geom_point(data = data_autumn, aes(x = year, y = !!sym(y_var_autumn)), 
                 color = color, size = 0.7, alpha = 1.0) +
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
        plot.margin = margin(5, 5, 5, 5)) +
      labs(
        x = if(is_bottom) "Year" else "",
        y = if(is_first_col) y_label else "")
    
    # Add threshold line if specified and this is an SST plot
    if (!is.null(threshold) && !is.null(var_type) && var_type == "sst") {
      p <- p + geom_hline(yintercept = threshold, color = "black", linetype = "dotted", size = 0.8, alpha = 0.8)
    }
    
    # Add column header
    if (!is.null(column_name)) {
      p <- p + annotate("text", x = Inf, y = Inf, label = column_name, 
                        hjust = 1.1, vjust = 1.5, size = 3, fontface = "bold")}
    
    return(p)}
  
  # Create plots for each column
  create_column_plots <- function(data_env_seasonal, column_name, is_first_col = FALSE) {
    color <- colors[[column_name]]
    
    # Create yearly averages from seasonal data
    data_env_yearly <- data_env_seasonal %>%
      group_by(year) %>%
      summarise(across(starts_with("Mean_"), mean, na.rm = TRUE), .groups = 'drop')
    
    # Filter autumn data
    data_env_autumn <- data_env_seasonal %>%
      filter(Season == "Autumn")
    
    data_years <- data_env_yearly$year
    
    # Get phytoplankton and zooplankton data for both yearly and autumn
    if (column_name == "Full Stock") {
      phyto_vals_yearly <- get_cpr_data(cpr_full_data$phyto, NULL, data_years, "All")
      phyto_vals_autumn <- get_cpr_data(cpr_full_data$phyto, NULL, data_env_autumn$year, "Autumn")
      small_vals_yearly <- get_cpr_data(cpr_full_data$small, NULL, data_years, "All")
      small_vals_autumn <- get_cpr_data(cpr_full_data$small, NULL, data_env_autumn$year, "Autumn")
    } else {
      phyto_vals_yearly <- get_cpr_data(cpr_component_data$phyto, column_name, data_years, "All")
      phyto_vals_autumn <- get_cpr_data(cpr_component_data$phyto, column_name, data_env_autumn$year, "Autumn")
      small_vals_yearly <- get_cpr_data(cpr_component_data$small, column_name, data_years, "All")
      small_vals_autumn <- get_cpr_data(cpr_component_data$small, column_name, data_env_autumn$year, "Autumn")}
    
    # Create data frames
    phyto_df_yearly <- data.frame(year = data_years, phytoplankton = phyto_vals_yearly)
    phyto_df_autumn <- data.frame(year = data_env_autumn$year, phytoplankton = phyto_vals_autumn)
    small_df_yearly <- data.frame(year = data_years, small_zooplankton = small_vals_yearly)
    small_df_autumn <- data.frame(year = data_env_autumn$year, small_zooplankton = small_vals_autumn)
    
    # Variable mappings for environmental data
    env_vars <- list(sst = "Mean_SST", sss = "Mean_SSS")
    if (include_sbt) {
      env_vars$sbt <- "Mean_SBT"
    }
    
    # Get threshold for this region/column (if available)
    region_threshold <- NULL
    if (!is.null(threshold_values) && column_name %in% names(threshold_values)) {
      region_threshold <- threshold_values[[column_name]]
    }
    
    # Start building the plot list with consistent elements
    plots <- list(
      create_ts_plot_dual(phyto_df_yearly, phyto_df_autumn, "phytoplankton", "phytoplankton", 
                          "Phytoplankton (ind/sample)", 
                          if(is_first_col) column_name else column_name, FALSE, is_first_col, color),
      create_ts_plot_dual(small_df_yearly, small_df_autumn, "small_zooplankton", "small_zooplankton", 
                          "Small Zooplankton (ind/sample)", 
                          NULL, FALSE, is_first_col, color),
      create_ts_plot_dual(data_env_yearly, data_env_autumn, env_vars$sst, env_vars$sst, 
                          "SST (°C)", 
                          NULL, FALSE, is_first_col, color, region_threshold, "sst"))
    
    # Add SBT plot only if include_sbt is TRUE and the variable exists in data
    if (include_sbt && "Mean_SBT" %in% names(data_env_yearly)) {
      plots <- append(plots, list(
        create_ts_plot_dual(data_env_yearly, data_env_autumn, env_vars$sbt, env_vars$sbt, 
                            "SBT (°C)", 
                            NULL, FALSE, is_first_col, color)))
    }
    
    # Add SSS plot (always last, so it gets is_bottom = TRUE)
    plots <- append(plots, list(
      create_ts_plot_dual(data_env_yearly, data_env_autumn, env_vars$sss, env_vars$sss, 
                          "SSS (PSU)", 
                          NULL, TRUE, is_first_col, color)))
    
    return(wrap_plots(plots, ncol = 1))}
  
  # Generate all columns
  plot_list <- list(create_column_plots(env_df_full_seasonal, "Full Stock", TRUE))
  
  for (area in areas) {
    area_data <- env_df_subset_seasonal %>% filter(Region == area) %>% arrange(year, Season)
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
    stop("SSB column '", ssb_col, "' not found in data")}
  if (!year_col %in% names(data)) {
    stop("Year column '", year_col, "' not found in data")}
  
  # Remove rows with missing values
  analysis_data <- data[complete.cases(data[c(ssb_col, year_col)]), ]
  
  if(nrow(analysis_data) < nrow(data)) {
    cat("# Removed", nrow(data) - nrow(analysis_data), "rows with missing values\n")}
  
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
         xlab = "Index", lwd = 4,
         main = paste("CPT Analysis -", region_name))}
  
  # BCP Analysis
  cat("\n## BCP Analysis\n")
  bcp.ssb <- bcp(analysis_data[[ssb_col]])
  bcp_indices <- which(bcp.ssb$posterior.prob >= bcp_threshold)
  bcp_years <- analysis_data[[year_col]][bcp_indices]
  
  cat("# BCP changepoint indices:", paste(bcp_indices, collapse = ", "), "\n")
  cat("# BCP changepoint years:", paste(bcp_years, collapse = ", "), "\n")
  
  if(plot_results) {
    plot(bcp.ssb, main = paste("BCP Analysis -", region_name))}
  
  # Consensus Analysis
  cat("\n## Consensus Analysis\n")
  consensus_years <- c()
  
  # Find consensus points (within tolerance)
  for (cpt_year in cpt_years) {
    if (any(abs(bcp_years - cpt_year) <= consensus_tolerance)) {
      consensus_years <- c(consensus_years, cpt_year)}}
  
  # Apply minimum spacing rule
  if (length(consensus_years) > 1) {
    consensus_years <- sort(consensus_years)
    filtered_consensus <- consensus_years[1]
    
    for (i in 2:length(consensus_years)) {
      if (consensus_years[i] - tail(filtered_consensus, 1) >= min_years_between) {
        filtered_consensus <- c(filtered_consensus, consensus_years[i])}}
    consensus_years <- filtered_consensus}
  
  # Identify method-specific changepoints
  cpt_only_years <- setdiff(cpt_years, consensus_years)
  bcp_only_years <- setdiff(bcp_years, consensus_years)
  
  cat("# Consensus changepoints (±", consensus_tolerance, "yr, min", min_years_between, "yr apart):", 
      paste(consensus_years, collapse = ", "), "\n")
  
  if (length(cpt_only_years) > 0) {
    cat("# CPT only:", paste(cpt_only_years, collapse = ", "), "\n")
  }
  if (length(bcp_only_years) > 0) {
    cat("# BCP only:", paste(bcp_only_years, collapse = ", "), "\n")}
  
  # Create results structure
  results <- list(
    region = region_name,
    parameters = list(
      Q = Q,
      bcp_threshold = bcp_threshold,
      consensus_tolerance = consensus_tolerance,
      min_years_between = min_years_between),
    data_info = list(
      n_observations = nrow(analysis_data),
      year_range = range(analysis_data[[year_col]]),
      ssb_range = range(analysis_data[[ssb_col]], na.rm = TRUE)),
    cpt_analysis = list(
      model = ssbcpts,
      changepoint_indices = cpt_indices,
      changepoint_years = cpt_years,
      n_changepoints = length(cpt_years)),
    bcp_analysis = list(
      model = bcp.ssb,
      changepoint_indices = bcp_indices,
      changepoint_years = bcp_years,
      n_changepoints = length(bcp_years),
      threshold_used = bcp_threshold),
    consensus = list(
      changepoint_years = consensus_years,
      cpt_only_years = cpt_only_years,
      bcp_only_years = bcp_only_years,
      n_consensus = length(consensus_years),
      criteria = paste("Both methods ±", consensus_tolerance, "year, min", min_years_between, "years between points")))
  
  cat("\n")
  return(results)}

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
      ylim(0, 7)}
  
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
  n <- length(x)
  lowest <- vector("logical", length = n-1)
  lowest[1] <- FALSE
  for (i in 2:n) {
    lowest[i] <- x[i] < x[i-1] & x[i] < x[i+1]}
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
                            show_msy_btrigger = TRUE,
                            show_fmsy = TRUE,
                            colors = c("steelblue", "darkorange", "purple", "lightgreen", "indianred", "navyblue"),
                            auto_nudge = TRUE,           # New: automatic intelligent nudging
                            nudge_force = 1,             # New: control nudging strength
                            label_box_padding = 0.35,    # New: padding around labels
                            label_point_padding = 0.5,   # New: padding from points
                            manual_nudge = NULL) {       # Simplified manual override
  
  msy_btrigger <- as.numeric(msy_btrigger)
  fmsy <- as.numeric(fmsy)
  
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
      phase_data_list[[i]] <- data %>% filter(year <= break_years[1])
    } else if (i == n_phases) {
      phase_data_list[[i]] <- data %>% filter(year > break_years[n_breaks])
    } else {
      phase_data_list[[i]] <- data %>% filter(year > break_years[i-1] & year <= break_years[i])}}
  
  # Calculate data ranges for intelligent nudging
  f_range <- range(data$F, na.rm = TRUE)
  ssb_range <- range(data$SSB_lag/1000000, na.rm = TRUE)
  f_span <- diff(f_range)
  ssb_span <- diff(ssb_range)
  
  # Create the base plot
  p <- ggplot(data = data, aes(x = F, y = SSB_lag/1000000)) +
    geom_path(colour = "grey") +
    geom_point(colour = hyst_phases) +
    labs(title = component, x = "F", y = "SSB in millions") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Add MSY B trigger line and label
  if (show_msy_btrigger) {
    p <- p + 
      geom_hline(yintercept = msy_btrigger/1000000, linetype = "dashed", color = "gray30") +
      annotate("text", x = f_range[1] + f_span * 0.02, y = msy_btrigger/1000000, 
               label = expression("MSY B"[trigger]), color = "gray30", size = 3.5, 
               fontface = "bold", hjust = 0, vjust = -0.5)}
  
  # Add F_MSY line and label
  if (show_fmsy) {
    p <- p + 
      geom_vline(xintercept = fmsy, linetype = "dashed", color = "gray30") +
      annotate("text", x = fmsy, y = ssb_range[1] + ssb_span * 0.02, 
               label = expression("F"[MSY]), color = "gray30", size = 3.5,
               hjust = -0.2, vjust = 0)}
  
  # Add geom_smooth for each phase
  for (i in 1:n_phases) {
    if (nrow(phase_data_list[[i]]) > 0) {
      p <- p + geom_smooth(data = phase_data_list[[i]], aes(x = F, y = SSB_lag/1000000),
                           method = "lm", colour = colors[i])}}
  
  # Prepare data for labeling
  label_data <- data.frame()
  
  # Add break years
  for (i in 1:n_breaks) {
    break_year_data <- data %>% filter(year == break_years[i])
    if (nrow(break_year_data) > 0) {
      label_data <- rbind(label_data, break_year_data)}}
  
  # Add first and last years
  first_last_data <- data[c(1, nrow(data)), ]
  label_data <- rbind(label_data, first_last_data)
  
  # Remove duplicates (in case break years include first/last years)
  label_data <- label_data[!duplicated(label_data$year), ]
  
  # Apply nudging strategy
  if (auto_nudge && is.null(manual_nudge)) {
    # Intelligent automatic nudging
    p <- p + geom_text_repel(
      data = label_data, 
      aes(label = year),
      box.padding = label_box_padding,
      point.padding = label_point_padding,
      force = nudge_force,
      force_pull = nudge_force * 0.5,
      max.overlaps = Inf,
      min.segment.length = 0,
      segment.size = 0.3,
      segment.alpha = 0.6,
      size = 3.2,
      fontface = "italic",
      color = "black",
      direction = "both",
      seed = 42)
    
  } else if (!is.null(manual_nudge)) {
    # Manual nudging with simplified interface
    # manual_nudge should be a named list: list("2010" = c(x_nudge, y_nudge), ...)
    
    for (i in 1:nrow(label_data)) {
      year_str <- as.character(label_data$year[i])
      
      if (year_str %in% names(manual_nudge)) {
        nudge_x <- manual_nudge[[year_str]][1] * f_span * 0.1
        nudge_y <- manual_nudge[[year_str]][2] * ssb_span * 0.1
      } else {
        nudge_x <- 0
        nudge_y <- 0}
      
      p <- p + geom_text_repel(
        data = label_data[i, ], 
        aes(label = year),
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        box.padding = label_box_padding,
        point.padding = label_point_padding,
        size = 3.2,
        fontface = "bold",
        color = "black",
        segment.size = 0.3,
        segment.alpha = 0.6)}
    
  } else {
    # Simple fallback with minimal nudging
    p <- p + geom_text_repel(
      data = label_data, 
      aes(label = year),
      box.padding = 0.5,
      point.padding = 0.3,
      size = 3,
      color = "black",
      segment.size = 0.2)}
  
  return(p)}

# Helper function to create manual nudge parameters easily
create_nudge_params <- function(...) {
  # Usage: create_nudge_params("2010" = c(1, -1), "2015" = c(-1, 1))
  # Values are relative: positive x = right, negative x = left
  #                     positive y = up, negative y = down
  list(...)}


#--------------------------------------------------------------------------------------
## Extract SRR Breakpoints
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
    cat("# Removed", nrow(data) - nrow(analysis_data), "rows with missing values\n")}
  
  # Initialize results list
  results <- list(
    region = region_name,
    method = method,
    data_used = analysis_data)
  
  if(method == "strucchange") {
    # Original strucchange analysis
    bpts_SRR <- strucchange::breakpoints(analysis_data[[r_col]] ~ analysis_data[[ssb_col]])
    
    if(plot_breakpoints) {
      plot(bpts_SRR, main = paste("SRR Breakpoints for", region_name))}
    
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
      abline(v = analysis_data[[ssb_col]][ci_mod_SRR$confint[i,3]], col = "red", lty = 3)}
    
    legend("topright", legend = c("Best estimate", "Confidence limits"), 
           col = c("blue", "red"), lty = c(1, 3), lwd = c(2, 1))
    
    # Add strucchange-specific results to output
    results <- c(results, list(
      breakpoints = bpts2_SRR,
      optimal_breaks = opt_brks_SRR[1],
      break_ssb_values = best_brk_SRR,
      break_years = best_brk_years_SRR,
      confidence_intervals = ci_mod_SRR,
      summary = bpts_SRR_sum))
    
  } else if(method == "segmented") {
    # Segmented model analysis
    cat("# Running segmented model analysis\n")
    
    # Set initial psi value
    if(is.null(initial_psi)) {
      initial_psi <- mean(analysis_data[[ssb_col]], na.rm = TRUE)
      cat("# Using mean SSB as initial psi:", round(initial_psi, 2), "\n")
    } else {
      cat("# Using provided initial psi:", round(initial_psi, 2), "\n")}
    
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
               lwd = c(2, 2, 1))}
      
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
        initial_psi = initial_psi))
      
    }, error = function(e) {
      cat("# Error in segmented analysis:", e$message, "\n")
      cat("# Try adjusting the initial_psi value\n")
      
      results <<- c(results, list(
        error = e$message,
        initial_psi = initial_psi))})
    
  } else {
    stop("Method must be either 'strucchange' or 'segmented'")}
  
  cat("\n")
  return(results)}


#--------------------------------------------------------------------------------------
## Plot SRR Residuals
#--------------------------------------------------------------------------------------

plot_herring_summary <- function(srr_data, ssb_col, recruit_col, year_col, f_col) {
  
  # --- Step 1: VALIDATE COLUMN NAMES ---
  required_cols <- c(ssb_col, recruit_col, year_col, f_col)
  missing_cols <- required_cols[!required_cols %in% names(srr_data)]
  
  if (length(missing_cols) > 0) {
    stop(paste("ERROR: The following required column(s) were not found:",
               paste(missing_cols, collapse = ", ")))}
  
  # --- Step 2: Calculate SRR log residuals ---
  residual_data <- dplyr::select(srr_data,
                                 Year = dplyr::all_of(year_col),
                                 SSB = dplyr::all_of(ssb_col),
                                 R = dplyr::all_of(recruit_col)) %>%
    stats::na.omit() %>%
    dplyr::mutate(
      SSB_log = log(SSB),
      R_log = log(R))
  
  if(nrow(residual_data) == 0) {
    stop("ERROR: No complete rows left after removing NAs from SSB and Recruitment columns.")}
  
  linear_model_log <- stats::lm(R_log ~ SSB_log, data = residual_data)
  segmented_model_log <- segmented::segmented(linear_model_log, seg.Z = ~SSB_log, psi = mean(residual_data$SSB_log, na.rm=TRUE))
  residual_data$log_residuals <- stats::residuals(segmented_model_log)
  
  # --- Step 3: Prepare the final dataframe for plotting ---
  # THIS SECTION IS NOW CORRECTED
  plot_data <- dplyr::select(srr_data,
                             Year = dplyr::all_of(year_col),
                             SSB = dplyr::all_of(ssb_col),
                             Recruitment = dplyr::all_of(recruit_col),
                             F_2_6 = dplyr::all_of(f_col)) %>%
    # First select the columns, THEN perform calculations with mutate()
    dplyr::mutate(
      SSB = SSB / 1000000,
      Recruitment = Recruitment / 1000000
    ) %>%
    dplyr::left_join(dplyr::select(residual_data, Year, log_residuals), by = "Year") %>%
    dplyr::mutate(
      residual_color = ifelse(log_residuals < 0, "negative", "positive"))
  
  # --- Step 4: Create each individual panel with your new aesthetics ---
  theme_stacked <- ggplot2::theme_classic() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(), plot.margin = ggplot2::margin(t = 5, r = 5, b = 0, l = 5))
  
  # Panel (a): SSB - with navyblue points
  p_ssb <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = SSB)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point(shape = 21, fill = "navyblue", color = "white", size = 2) +
    ggplot2::labs(y = "SSB (million t)") + theme_stacked
  
  # Panel (b): Recruits - with darkgrey fill and no outline
  p_recruits <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Recruitment)) +
    ggplot2::geom_col(fill = "grey25", color = NA, width = 0.8) +
    ggplot2::labs(y = "Recruits (billion)") + theme_stacked
  
  # Panel (c): Fishing Mortality - with navyblue points
  p_f <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = F_2_6)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point(shape = 21, fill = "navyblue", color = "white", size = 2) +
    ggplot2::labs(y = "Fishing mortality") + theme_stacked
  
  # Panel (d): SRR log residuals - with conditional color and no outline
  p_residuals <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = log_residuals)) +
    ggplot2::geom_col(ggplot2::aes(fill = residual_color), color = NA, width = 0.8) +
    ggplot2::scale_fill_manual(values = c("negative" = "navyblue", "positive" = "grey25"), guide = "none", na.value="grey80") +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::labs(y = "SRR log residuals", x = "Year") +
    ggplot2::theme_classic() + ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5))
  
  # --- Step 5: Combine the plots with explicit patchwork:: calls ---
  combined_plot <- patchwork::wrap_plots(p_ssb, p_recruits, p_f, p_residuals, ncol = 1)
  
  final_plot <- combined_plot + patchwork::plot_annotation(tag_levels = 'a', tag_suffix = ')') &
    ggplot2::theme(plot.tag = ggplot2::element_text(face = 'bold'))
  
  print(final_plot)}


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
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))}
  
  # Determine number of break years and resulting phases
  n_breaks <- length(break_years)
  n_phases <- n_breaks + 1
  
  # Create phase column based on break years
  data$phase <- 1
  for (i in 1:n_breaks) {
    data$phase[data[[year_col]] > break_years[i]] <- i + 1}
  
  # Set default nudge parameters if not provided
  if (is.null(nudge_params)) {
    nudge_params <- list(
      list(nudge_y = -5, nudge_x = -0.2), # first year
      list(nudge_y = 0, nudge_x = -0.5),  # breakpoint
      list(nudge_y = -5, nudge_x = -0.2), # last year
      list(nudge_y = 5, nudge_x = -0.2),  # more breakpoints
      list(nudge_y = -0, nudge_x = -0.2))}
  
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
                 color = "gray30", size = 3.5, fontface = "bold")}
  
  # Add geom_smooth for each phase
  for (i in 1:n_phases) {
    phase_data <- data %>% dplyr::filter(phase == i)
    if (nrow(phase_data) > 0) {
      p <- p + geom_smooth(data = phase_data,
                           mapping = aes(x = .data[[ssb_col]] / 1000000, 
                                         y = .data[[r_col]] / 1000000),
                           col = colors[i], method = "lm")}}
  
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
                               size = 3, col = "gray30", segment.size = 0.2)}}
  
  return(p)}


#--------------------------------------------------------------------------------------
## Threshold GAM Analysis Function
#--------------------------------------------------------------------------------------

run_threshold_gam <- function(data, 
                              response_var, 
                              pressure_var, 
                              threshold_var, 
                              time_var = "year",
                              debug = FALSE) {
  
  # Required variables for the analysis
  required_vars <- c(response_var, pressure_var, threshold_var, time_var)
  
  # Find complete data range
  complete_rows <- complete.cases(data[required_vars])
  if (sum(complete_rows) < 10) {
    stop("Insufficient data: Need at least 10 complete observations for GAM analysis")}
  
  # Get indices for complete data range
  first_complete <- which.max(complete_rows)
  last_complete <- max(which(complete_rows))
  
  # Subset to optimal range and remove any remaining NAs
  analysis_data <- data[first_complete:last_complete, ]
  analysis_data <- analysis_data[complete.cases(analysis_data[required_vars]), ]
  
  # Extract variables
  y <- analysis_data[[response_var]]
  x <- analysis_data[[pressure_var]]
  x2 <- analysis_data[[threshold_var]]
  time <- analysis_data[[time_var]]
  
  # Fit models
  mod <- gam(y ~ s(x, k = 3))
  tmod <- thresh_gam(
    model = mod, 
    ind_vec = y, 
    press_vec = x, 
    t_var = x2, 
    name_t_var = threshold_var,
    k = 4, 
    a = 0.2, 
    b = 0.8)
  
  # Cross-validation
  loocv_result <- loocv_thresh_gam(
    model = mod, 
    ind_vec = y, 
    press_vec = x, 
    t_var = x2, 
    name_t_var = threshold_var, 
    k = 4, 
    a = 0.2, 
    b = 0.8, 
    time = time)
  
  # Extract model diagnostics
  loocv_passed <- loocv_result$result
  
  # Extract EDF values
  edf_before <- if (!is.null(tmod$edf) && length(tmod$edf) >= 2) tmod$edf[1] else NA
  edf_after <- if (!is.null(tmod$edf) && length(tmod$edf) >= 2) tmod$edf[2] else NA
  edf_differ <- if (!is.na(edf_before) && !is.na(edf_after)) abs(edf_before - edf_after) > 0.1 else FALSE
  
  # Extract p-values with multiple fallback methods
  p_values <- NA
  significant_slope <- FALSE
  
  tryCatch({
    if (!is.null(tmod$p_val)) {
      p_values <- tmod$p_val
    } else {
      gam_summary <- summary(tmod)
      if (!is.null(gam_summary$p.table)) {
        p_values <- gam_summary$p.table[, "Pr(>|t|)"]
      } else if (!is.null(gam_summary$s.table)) {
        p_values <- gam_summary$s.table[, "p-value"]
      } else if (!is.null(tmod$coefficients) && !is.null(tmod$Vp)) {
        coefs <- tmod$coefficients
        se <- sqrt(diag(tmod$Vp))
        t_values <- coefs / se
        p_values <- 2 * (1 - pnorm(abs(t_values)))}}
    
    if (!is.na(p_values[1])) {
      significant_slope <- any(p_values < 0.05, na.rm = TRUE)}
  }, error = function(e) {
    if (debug) cat("P-value extraction error:", e$message, "\n")})
  
  # Extract GCV data
  gcv_data <- if (!is.null(tmod$gcvv) && !is.null(tmod$t_val)) {
    data.frame(
      threshold_values = tmod$t_val,
      gcv_values = tmod$gcvv,
      selected_threshold = tmod$mr)
  } else NULL
  
  # Determine acceptance status
  threshold_accepted <- loocv_passed && edf_differ && significant_slope
  
  acceptance_status <- if (!loocv_passed) {
    "No tGAM (LOOCV)"
  } else if (!edf_differ) {
    "No tGAM (EDF similar)"
  } else if (!significant_slope) {
    "No tGAM (p-values)"
  } else {
    "Yes"}
  
  if (debug) {
    cat("\n=== THRESHOLD GAM RESULTS ===\n")
    cat("LOOCV passed:", loocv_passed, "\n")
    cat("EDF differ:", edf_differ, "(", round(edf_before, 3), "vs", round(edf_after, 3), ")\n")
    cat("Significant slope:", significant_slope, "\n")
    cat("Overall accepted:", threshold_accepted, "\n")
    cat("Status:", acceptance_status, "\n\n")}
  
  # Return comprehensive results
  return(list(
    # Basic metrics
    n = length(y),
    edf_before = if(!is.na(edf_before)) round(edf_before, 3) else NA,
    edf_after = if(!is.na(edf_after)) round(edf_after, 3) else NA,
    p_values = if(!is.na(p_values[1])) round(p_values, 3) else NA,
    threshold = round(tmod$mr, 1),
    threshold_accepted = acceptance_status,
    
    # Acceptance criteria (NEW: detailed breakdown)
    acceptance_criteria = list(
      loocv_passed = loocv_passed,
      edf_differ = edf_differ,
      significant_slope = significant_slope,
      overall_accepted = threshold_accepted),
    
    # Model objects and data
    gcv_data = gcv_data,
    loocv_result = loocv_result,
    threshold_model = tmod,
    base_model = mod,
    
    # Analysis metadata (NEW)
    analysis_info = list(
      time_range = c(min(time), max(time)),
      data_range = list(
        start_idx = first_complete,
        end_idx = last_complete,
        n_complete = length(y)
      ),
      variables = list(
        response = response_var,
        pressure = pressure_var,
        threshold = threshold_var,
        time = time_var)),
    
    # Summary table row (NEW: for easy compilation)
    summary_row = data.frame(
      Variable = threshold_var,
      n = length(y),
      edf_before = if(!is.na(edf_before)) round(edf_before, 3) else NA,
      edf_after = if(!is.na(edf_after)) round(edf_after, 3) else NA,
      p_value_min = if(!is.na(p_values[1])) {
        min_p <- min(p_values, na.rm = TRUE)
        if (min_p < 0.001) sprintf("%.2e", min_p) else round(min_p, 3)
      } else NA,
      Threshold = round(tmod$mr, 1),
      Threshold_accepted = acceptance_status,
      stringsAsFactors = FALSE)))}

#--------------------------------------------------------------------------------------
## Automated Threshold Analysis Workflow
#--------------------------------------------------------------------------------------

run_threshold_analysis <- function(herring_region_data, 
                                   env_data, 
                                   region_name = NULL,
                                   aggregation_mode = "all",
                                   test_variables = "all",
                                   include_sbt = TRUE,
                                   debug = FALSE) {
  
  # Validate inputs
  valid_modes <- c("all", "autumn", "lagged", "year")
  if (!aggregation_mode %in% valid_modes) {
    stop(paste("Invalid aggregation_mode. Must be one of:", paste(valid_modes, collapse = ", ")))}
  
  # Define variables to test based on include_sbt parameter and available data
  base_variables <- c("Mean_SST", "Mean_SSS")
  base_var_labels <- c("SST", "SSS")
  
  # Add SBT if requested and available in the data
  if (include_sbt && "Mean_SBT" %in% names(env_data)) {
    base_variables <- c(base_variables, "Mean_SBT")
    base_var_labels <- c(base_var_labels, "SBT")
  } else if (include_sbt && !"Mean_SBT" %in% names(env_data)) {
    warning("SBT requested but not found in env_data. Proceeding without SBT.")
  }
  
  all_variables <- base_variables
  all_var_labels <- base_var_labels
  
  if (identical(test_variables, "all")) {
    variables <- all_variables
    var_labels <- all_var_labels
  } else {
    invalid_vars <- test_variables[!test_variables %in% all_variables]
    if (length(invalid_vars) > 0) {
      stop(paste("Invalid test_variables:", paste(invalid_vars, collapse = ", ")))
    }
    variables <- test_variables
    var_labels <- all_var_labels[all_variables %in% test_variables]}
  
  # Filter data by region if specified
  if (!is.null(region_name)) {
    env_region <- env_data %>% filter(Region == region_name)
  } else {
    env_region <- env_data
    region_name <- "FULL DATASET"}
  
  # Initialize comprehensive results structure
  results <- list(
    # Analysis metadata
    analysis_metadata = list(
      region = region_name,
      aggregation_mode = aggregation_mode,
      test_variables = var_labels,
      n_variables = length(variables),
      include_sbt = include_sbt,
      timestamp = Sys.time()),
    
    # Results by mode
    autumn = list(),
    lagged = list(),
    year = list(),
    
    # Summary tables
    summary_tables = list(
      autumn = data.frame(),
      lagged = data.frame(),
      year = data.frame()),
    
    # Accepted thresholds
    significant_thresholds = list(
      autumn = list(),
      lagged = list(),
      year = list()),
    
    # Model diagnostics summary
    diagnostics_summary = list(
      autumn = list(),
      lagged = list(),
      year = list()))
  
  # Print analysis header
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("THRESHOLD GAM ANALYSIS FOR", toupper(region_name), "\n")
  if (aggregation_mode != "all") cat("AGGREGATION MODE:", toupper(aggregation_mode), "\n")
  if (!identical(test_variables, "all")) cat("TESTING VARIABLES:", paste(var_labels, collapse = ", "), "\n")
  cat("ENVIRONMENTAL VARIABLES:", paste(var_labels, collapse = ", "), "\n")
  if (!include_sbt) cat("SBT ANALYSIS: DISABLED\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Detect SSB column name
  ssb_col <- if ("SSB" %in% names(herring_region_data)) {
    "SSB"
  } else if ("SSB_component" %in% names(herring_region_data)) {
    "SSB_component"
  } else {
    stop("Neither 'SSB' nor 'SSB_component' found in herring data. Available columns: ", 
         paste(names(herring_region_data), collapse = ", "))
  }
  
  # Data preparation functions (only environmental variables)
  prepare_data <- list(
    autumn = function() {
      # Create base dataset with herring data
      base_data <- herring_region_data
      
      # Join environmental data (autumn only)
      env_autumn <- env_region %>% filter(Season == "Autumn")
      
      if (nrow(env_autumn) > 0) {
        base_data <- base_data %>% left_join(env_autumn, by = "year")
      } else {
        # Add missing environmental variables as NA
        for (var in all_variables) {
          if (!var %in% names(base_data)) base_data[[var]] <- NA
        }
      }
      
      # Select required columns
      required_cols <- c("year", ssb_col, "Recruitment", variables)
      existing_cols <- intersect(required_cols, names(base_data))
      final_data <- base_data[, existing_cols, drop = FALSE]
      
      # Add missing columns as NA
      missing_cols <- setdiff(required_cols, existing_cols)
      for (col in missing_cols) {
        final_data[[col]] <- NA
      }
      
      return(final_data[, required_cols, drop = FALSE])
    },
    
    lagged = function() {
      # Create base dataset with herring data
      base_data <- herring_region_data
      
      # Join environmental data (autumn, lagged by +1 year)
      env_autumn_lagged <- env_region %>% 
        filter(Season == "Autumn") %>% 
        mutate(year = year + 1)
      
      if (nrow(env_autumn_lagged) > 0) {
        base_data <- base_data %>% left_join(env_autumn_lagged, by = "year")
      } else {
        # Add missing environmental variables as NA
        for (var in all_variables) {
          if (!var %in% names(base_data)) base_data[[var]] <- NA
        }
      }
      
      # Select required columns
      required_cols <- c("year", ssb_col, "Recruitment", variables)
      existing_cols <- intersect(required_cols, names(base_data))
      final_data <- base_data[, existing_cols, drop = FALSE]
      
      # Add missing columns as NA
      missing_cols <- setdiff(required_cols, existing_cols)
      for (col in missing_cols) {
        final_data[[col]] <- NA
      }
      
      return(final_data[, required_cols, drop = FALSE])
    },
    
    year = function() {
      # Create base dataset with herring data
      base_data <- herring_region_data
      
      # Prepare environmental data (yearly averages)
      env_summary <- NULL
      if (!is.null(env_region) && nrow(env_region) > 0) {
        env_summary <- env_region %>%
          group_by(year) %>%
          summarise(across(starts_with("Mean_"), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')
        base_data <- base_data %>% left_join(env_summary, by = "year")
      } else {
        # Add missing environmental variables as NA
        for (var in all_variables) {
          if (!var %in% names(base_data)) base_data[[var]] <- NA
        }
      }
      
      # Select required columns
      required_cols <- c("year", ssb_col, "Recruitment", variables)
      existing_cols <- intersect(required_cols, names(base_data))
      final_data <- base_data[, existing_cols, drop = FALSE]
      
      # Add missing columns as NA
      missing_cols <- setdiff(required_cols, existing_cols)
      for (col in missing_cols) {
        final_data[[col]] <- NA
      }
      
      return(final_data[, required_cols, drop = FALSE])
    })
  
  # Main analysis function for each mode
  run_mode_analysis <- function(mode_name) {
    cat("## ", stringr::str_to_title(mode_name), " based analysis\n")
    
    analysis_data <- prepare_data[[mode_name]]()
    mode_results <- list()
    summary_table <- data.frame()
    
    for (i in seq_along(variables)) {
      cat("### Variable:", var_labels[i], "\n")
      
      # Check if variable has any data
      var_data <- analysis_data[[variables[i]]]
      if (all(is.na(var_data))) {
        cat("#### Warning: No data available for", var_labels[i], "\n")
        result <- list(
          n = 0, edf_before = NA, edf_after = NA, p_values = NA,
          threshold = NA, threshold_accepted = "No data",
          acceptance_criteria = list(loocv_passed = FALSE, edf_differ = FALSE, 
                                     significant_slope = FALSE, overall_accepted = FALSE),
          summary_row = data.frame(Variable = var_labels[i], n = 0, edf_before = NA, 
                                   edf_after = NA, p_value_min = NA, Threshold = NA,
                                   Threshold_accepted = "No data", stringsAsFactors = FALSE))
      } else {
        # Run threshold GAM analysis
        result <- tryCatch({
          run_threshold_gam(analysis_data, "Recruitment", ssb_col, variables[i], "year", debug = debug)
        }, error = function(e) {
          cat("#### Error:", e$message, "\n")
          list(
            n = NA, edf_before = NA, edf_after = NA, p_values = NA,
            threshold = NA, threshold_accepted = paste("Error:", e$message),
            acceptance_criteria = list(loocv_passed = FALSE, edf_differ = FALSE, 
                                       significant_slope = FALSE, overall_accepted = FALSE),
            summary_row = data.frame(Variable = var_labels[i], n = NA, edf_before = NA, 
                                     edf_after = NA, p_value_min = NA, Threshold = NA,
                                     Threshold_accepted = paste("Error:", e$message), stringsAsFactors = FALSE))
        })
      }
      
      # Store results
      mode_results[[var_labels[i]]] <- result
      summary_table <- rbind(summary_table, result$summary_row)
      
      # Track accepted thresholds
      if (!is.na(result$threshold_accepted) && result$threshold_accepted == "Yes") {
        results$significant_thresholds[[mode_name]][[var_labels[i]]] <- result$threshold}
      
      # Store diagnostics
      results$diagnostics_summary[[mode_name]][[var_labels[i]]] <- result$acceptance_criteria}
    
    # Store mode results
    results[[mode_name]] <<- mode_results
    results$summary_tables[[mode_name]] <<- summary_table
    
    # Print summary table
    cat("\n")
    print(summary_table, row.names = FALSE)
    cat("\n")}
  
  # Run analysis based on aggregation mode
  modes_to_run <- if (aggregation_mode == "all") c("autumn", "lagged", "year") else aggregation_mode
  
  for (mode in modes_to_run) {
    run_mode_analysis(mode)
    if (length(modes_to_run) > 1) cat("\n")}
  
  # Print summary of accepted thresholds
  cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("SUMMARY OF ACCEPTED THRESHOLDS:\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  significant_found <- FALSE
  for (mode in modes_to_run) {
    if (length(results$significant_thresholds[[mode]]) > 0) {
      for (var in names(results$significant_thresholds[[mode]])) {
        cat(sprintf("- %s: %s (Threshold: %.1f)\n", 
                    stringr::str_to_title(mode), var, results$significant_thresholds[[mode]][[var]]))
        significant_found <- TRUE}}}
  
  if (!significant_found) cat("No environmental thresholds accepted.\n")
  
  # Add overall summary statistics
  results$overall_summary <- list(
    total_analyses = length(variables) * length(modes_to_run),
    total_accepted = sum(sapply(results$significant_thresholds[modes_to_run], length)),
    acceptance_rate = round(sum(sapply(results$significant_thresholds[modes_to_run], length)) / 
                              (length(variables) * length(modes_to_run)) * 100, 1),
    modes_analyzed = modes_to_run,
    variables_tested = var_labels,
    include_sbt = include_sbt)
  
  return(results)}


#--------------------------------------------------------------------------------------
## Enhanced GCV Plotting Functions
#--------------------------------------------------------------------------------------

plot_gcv_threshold <- function(result, variable_name, mode_name = "") {
  if (is.null(result$gcv_data)) {
    cat("No GCV data available for", variable_name, "\n")
    return(NULL)
  }
  
  gcv_data <- result$gcv_data
  
  plot(gcv_data$threshold_values, gcv_data$gcv_values,
       type = "l", xlab = paste(variable_name, "Threshold"), ylab = "GCV Values",
       main = paste("GCV Plot for", variable_name, mode_name),
       col = "blue", lwd = 2)
  
  abline(v = gcv_data$selected_threshold, col = "red", lty = 2, lwd = 2)
  text(x = gcv_data$selected_threshold, y = max(gcv_data$gcv_values, na.rm = TRUE) * 0.9,
       labels = paste("Threshold:", round(gcv_data$selected_threshold, 2)), pos = 4, col = "red")
  grid()
  
  # Valley assessment
  min_gcv_idx <- which.min(gcv_data$gcv_values)
  threshold_idx <- which.min(abs(gcv_data$threshold_values - gcv_data$selected_threshold))
  valley_assessment <- if (abs(min_gcv_idx - threshold_idx) <= 2) "DEEP VALLEY CONFIRMED" else "NO CLEAR VALLEY"
  
  mtext(valley_assessment, side = 3, line = 0.5, 
        col = if(valley_assessment == "DEEP VALLEY CONFIRMED") "green" else "orange")
  
  return(valley_assessment)
}

plot_all_gcv_thresholds <- function(analysis_results, aggregation_mode = "autumn") {
  if (!aggregation_mode %in% names(analysis_results)) {
    cat("No results found for aggregation mode:", aggregation_mode, "\n")
    return(NULL)
  }
  
  mode_results <- analysis_results[[aggregation_mode]]
  n_vars <- length(mode_results)
  
  if (n_vars > 1) par(mfrow = c(ceiling(n_vars/2), 2), mar = c(4, 4, 3, 1))
  
  valley_assessments <- list()
  for (var_name in names(mode_results)) {
    result <- mode_results[[var_name]]
    valley_assessments[[var_name]] <- plot_gcv_threshold(result, var_name, paste("(", aggregation_mode, ")"))
  }
  
  if (n_vars > 1) par(mfrow = c(1, 1))
  
  cat("\nGCV Valley Assessment Summary for", toupper(aggregation_mode), ":\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  for (var in names(valley_assessments)) {
    if (!is.null(valley_assessments[[var]])) {
      cat(sprintf("%-20s: %s\n", var, valley_assessments[[var]]))
    }
  }
  
  return(valley_assessments)
}

#--------------------------------------------------------------------------------------
## Enhanced Summary Functions
#--------------------------------------------------------------------------------------

# Extract comprehensive summary from analysis results
get_analysis_summary <- function(analysis_results) {
  summary_list <- list(
    metadata = analysis_results$analysis_metadata,
    overall = analysis_results$overall_summary,
    by_mode = list(),
    combined_summary_table = data.frame()
  )
  
  # Compile results by mode
  for (mode in c("autumn", "lagged", "year")) {
    if (length(analysis_results[[mode]]) > 0) {
      summary_list$by_mode[[mode]] <- list(
        summary_table = analysis_results$summary_tables[[mode]],
        accepted_thresholds = analysis_results$significant_thresholds[[mode]],
        diagnostics = analysis_results$diagnostics_summary[[mode]]
      )
      
      # Add mode column to summary table for combined view
      mode_table <- analysis_results$summary_tables[[mode]]
      if (nrow(mode_table) > 0) {
        mode_table$Mode <- mode
        summary_list$combined_summary_table <- rbind(summary_list$combined_summary_table, mode_table)
      }
    }
  }
  
  return(summary_list)
}

# Print formatted analysis summary
print_analysis_summary <- function(analysis_results) {
  summary <- get_analysis_summary(analysis_results)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("COMPREHENSIVE ANALYSIS SUMMARY\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  cat("Region:", summary$metadata$region, "\n")
  
  
  cat("Variables tested:", paste(summary$metadata$test_variables, collapse = ", "), "\n")
  cat("Total analyses:", summary$overall$total_analyses, "\n")
  cat("Accepted thresholds:", summary$overall$total_accepted, "\n")
  cat("Success rate:", summary$overall$acceptance_rate, "%\n\n")
  
  if (nrow(summary$combined_summary_table) > 0) {
    cat("COMBINED RESULTS TABLE:\n")
    print(summary$combined_summary_table, row.names = FALSE)
  }
  
  return(invisible(summary))
}


#--------------------------------------------------------------------------------------
## Plot tGAM
#--------------------------------------------------------------------------------------

plot_tgam_complete <- function(processed_data, 
                               threshold,
                               scale_factor = 1000000) {
  
  # Set up variables for threshold GAM
  y <- processed_data$Recruitment
  x <- processed_data$SSB_lag
  x2 <- processed_data$Mean_SST
  
  # Remove any rows with missing values to ensure vectors are same length
  complete_cases <- complete.cases(y, x, x2)
  y <- y[complete_cases]
  x <- x[complete_cases]
  x2 <- x2[complete_cases]
  
  # Create properly formatted data frame for GAM
  gam_data <- data.frame(y = y, x = x)
  
  # Fit initial GAM model
  mod <- gam(y ~ s(x, k = 3), data = gam_data)
  
  # Fit threshold GAM
  tmod <- thresh_gam(
    model = mod, 
    ind_vec = y, 
    press_vec = x, 
    t_var = x2, 
    name_t_var = "x2",
    k = 4, 
    a = 0.2, 
    b = 0.8
  )
  
  # Add predictions to processed data
  final_data <- processed_data[complete_cases, ] %>%
    mutate(
      tgam_pred = predict(tmod),
      tgam_ab = ifelse(Mean_SST <= threshold, "low_temp", "high_temp")
    )
  
  # Create and return plot
  final_data %>%
    ggplot(aes(x = SSB_lag/scale_factor, y = Recruitment/scale_factor)) +
    geom_point(data = . %>% filter(tgam_ab == "low_temp"), col = "red", size = 2) +
    geom_point(data = . %>% filter(tgam_ab == "high_temp"), col = "blue", size = 2) +
    geom_smooth(data = final_data, mapping = aes(x = SSB_lag/scale_factor, y = tgam_pred/scale_factor, col = tgam_ab), 
                method = "lm", se = TRUE) +
    scale_color_manual(values = c("low_temp" = "red", "high_temp" = "blue"),
                       labels = c("Below Threshold", "Above Threshold"),
                       name = "SST Category") +
    labs(x = paste0("SSB (lag 3) million t"),
         y = paste0("Recruitment billions")) +
    theme_minimal() +
    theme(legend.position = "bottom")
}