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
    geom_label(x = fmsy, y = 3, label = expression("F"[MSY]), 
               color = "gray30", size = 3.5) +
    geom_label(x = 0.8, y = msy_btrigger/1000000, label = expression("MSY B"[trigger]), 
               color = "gray30", size = 3.5, fontface = "bold") +
    geom_point(colour = hyst_phases) +
    theme_minimal() +
    labs(title = paste("Hysteresis in", component), x = "F", y = "SSB in millions") +
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
    list(nudge_y = -0.2, nudge_x = 0.05),
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
