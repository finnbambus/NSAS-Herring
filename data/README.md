The “data” folder contains the .Rdata files used for analysis. The files
are processed data from the “data raw” folder by the code in the
“data\_cleanup.Rmd” file of the “R” folder.

-   **env\_** contains environmental (Sea-Surface-Temperature & Sea-Surface-Salinity) data including the
    dimensions longitude, latitude, years, months & dates.
    -   Variations: full (data for the entire North Sea), subset (data
        subset and limited into spawning components (Shetland / Orkneys,
        Buchan, Banks & Downs))
        -   **df**\_ contains the data compressed to 2D (spatial mean,
            yearly mean) and saved as a data frames.
    -   Type: List
    -   Variables: SST \[°C\], SSS \[PSU\], lon \[° long\],
        lat \[° lat\], years, months & dates
        -   Region, year, Mean\_SST \[°C\], Mean\_SSS
            \[PSU\]
-   **LAI\_full** contains the LAI data for each component and fortnight
    -   Type: Data frame
    -   Variables: year, LAI\_unit, component, fortnight, LAI\_9mm
-   **LAI\_aggregated** contains the LAI data summed per year per
    component.
    -   Type: Data frame
    -   Variables: year, component, LAI\_9mm
-   **Herring\_LAI** contains the Spawning-Stock-Biomass (SSB) for each
    component as well as F & R for the entire stock over time, as well
    as the percent each component contributed to the LAI for each year.
    -   Type: tibble
    -   Variables: year, component, LAI\_perc. \[%\], SSB\_component
        \[t\], l\_bnd (lower bound) \[t\], u\_bnd (upper bound) \[t\],
        F, R \[thousands\]
-   **Herring\_full** contains the SSB, F and R time-series for the full
    stock as well as a coefficient variance and lower and upper bounds
    for SSB. The F and R values are take from the recent ICES Advise
    -   Type: tibble
    -   Variables: year, SSB \[t\], cv, l\_bnd \[t\], u\_bnd \[t\], F, R
        \[thousands\]
-   **CPR** contains data from the continuous plankton recorder. It was
    requested based on the ICES areas of the NSAS Herring and to contain
    target prey species (Phyto- and Zooplankton). It is separated into
    Phyto- and small Zooplankton and spatially filtered by component
    -   Type: tibble
    -   Variables: region, Year, Season, total\_food, average\_food,
        sample\_count
