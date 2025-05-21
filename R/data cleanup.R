setwd("~/R Studio/NSAS Herring/NSAS-Herring")

# load packages
library(tidyverse)
library(ncdf4)
library(raster)
library(ggplot2)

# load files
## driver data
NS_SBT_raw <- nc_open("data raw/NS SBT Monthly.nc")
NS_SST_raw <- nc_open("data raw/NS SST Monthly.nc")
NS_SSS_raw <- nc_open("data raw/NS SSS Monthly.nc")

## LAI & SSB data
LAI_raw <- read.table("data raw/NSAS LAI.txt", sep = "", header = TRUE, strip.white = TRUE) %>%
  rename(component = Area, year = Year, LAI_unit = LAIUnit, fortnight = Fortnight, LAI_9mm = L..9) %>%  # rename columns
  mutate(year = ifelse(year >= 100, 2000 + (year - 100),1900 + year),
         component = case_when(component == "Or/Sh" ~ "Shetland/Orkney", 
                               component == "CNS" ~ "Banks",
                               component == "SNS" ~ "Downs", TRUE ~ component),
         component = as.factor(component),
         LAI_unit = as.factor(LAI_unit),
         fortnight = as.factor(fortnight))                                                        #convert Year from century break to full year & rename components to unify across data

LAI_component_raw <- read_csv("data raw/NSAS LAI Components 2025.csv") %>%
  pivot_longer(cols = -c(1), names_to = "year", values_to = "LAI_perc.") %>%                      # convert data into long format
  rename(component = ...1) %>%                                                                    # rename first column after pivot transform
  mutate(component = case_when(component == "LAI-ORSH" ~ "Shetland/Orkney",
                               component == "LAI-BUN" ~ "Buchan",
                               component == "LAI-CNS" ~ "Banks",
                               component == "LAI-SNS" ~ "Downs"),
         component = as.factor(component),
         year = as.numeric(year),
         LAI_perc. = LAI_perc. * 100)                                                             # rename components to unify across data, converting years to numeric & LAI perc. to %

SSB_raw <- read_csv("data raw/NSAS SSB 2025.csv") %>%
  dplyr::select(-1) %>%
  rename(SSB = value, cv = CV, l_bnd = lbnd, u_bnd = ubnd)                                        # rename columns (SSB = SSB in Tonnes, cv = coefficient of variation, l_bnd = lower limit of 95% CI & u_bnd = upper limit of 95% CI)
  

# Visual investigation
## Timeseries of LAI by component
ggplot(LAI_raw, aes(x = year, y = LAI_9mm, group = component)) +
  geom_line(color = "navyblue") +
  facet_wrap(~ component, ncol = 2, scales = "free_y") +                                          # Create a grid, 2 columns, free y-scales for better visibility
  labs(title = "Larvae Abundance Index (LAI) by Unit Area over Years", 
       x = "Year", 
       y = "LAI (Larvae Abundance Index) >= 9mm") +
  theme_minimal()

## Stacked bar chart for components percentage of LAI
ggplot(LAI_component_raw, aes(x = as.factor(year), y = LAI_perc., fill = component)) +
  geom_bar(stat = "identity", position = "stack") +                                               # Create stacked bars
  labs(title = "Percentage of Larvae Abundance Index (LAI) by Component and Year",
       x = "Year",
       y = "LAI Percentage (%)",
       fill = "Component") +                                                                      # Label for the fill legend
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),                                                   # Center the plot title
        axis.text.x = element_text(angle = 45, hjust = 1),                                        # Rotate x-axis labels for readability
        axis.title.x = element_text(margin = margin(t = 10)),                                     # Add margin to x-axis title
        axis.title.y = element_text(margin = margin(r = 10))) +                                   # Add margin to y-axis title
  scale_fill_viridis_d()

## Timeseries of SSB including confindence bounds
ggplot(SSB_raw, aes(x = year)) +
  geom_ribbon(aes(ymin = (l_bnd / 1000000), ymax = (u_bnd / 1000000)), fill = "skyblue", alpha = 0.5) +     # Add the shaded area for the confidence interval (lower_bound to upper_bound)
  geom_line(aes(y = (SSB / 1000000)), color = "navyblue", linewidth = 1) +                        # Add the line for the SSB value
  labs(title = "SSB with 95% Confidence Bounds", 
       x = "Year", 
       y = "SSB in million t") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

# Splitting SSB into components
SSB_split_raw <- SSB_raw %>%
  left_join(LAI_component_raw, by = "year") %>%
  mutate(SSB_by_component = SSB * (LAI_perc. / 100),
         l_bnd = l_bnd * (LAI_perc. / 100),
         u_bnd = u_bnd * (LAI_perc. / 100)) %>%                                                   # Calculate the SSB split & 95% bounds by component percentage, ensure LAI_perc is treated as a decimal (divide by 100)
  drop_na()

## Visualise split SSB data by component
ggplot(SSB_split_raw, aes(x = year, y = (SSB_by_component / 1000000))) +
  geom_ribbon(aes(ymin = (l_bnd / 1000000), ymax = (u_bnd / 1000000)), fill = "skyblue", alpha = 0.5) +
  geom_line(color = "navyblue", linewidth = 0.8) +                                                # Line plot for SSB by component
  facet_wrap(~ component, ncol = 2, scales = "fixed") +                                           # Grid by component, 2 columns, free y-scales
  labs(title = "SSB with 95% Confidence Bounds by Component over Years",
       x = "Year",
       y = "SSB in million tonnes by Component") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
