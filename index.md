# Weatherfunctions

The goal of Weatherfunctions is to support download of daily weather
data from the German weather data service (DWD), to generate
interpolations of weather data and to calculate some derived variables
like potential evapotranspiration.

## Installation

You can install the development version of Weatherfunctions from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("AgronomyKiel/Weatherfunctions")
```

## Example

This is a basic example which shows you how to download a local copy of
the historical and recent daily weather data from DWD and to interpolate
them for a certain location defined by latitude, longitude and height
above sea level.

``` r
rm(list = ls())
library(Weatherfunctions)
library(here)
library(fst)
library(dplyr)
library(tidyr)
library(sf)

# download meta data for "synoptic" weather stations from DWD
DWD_content <- getDWDStationList(historical = DWD_ftp_historical, recent = DWD_ftp_recent)

# download meta data for additional stations with precipitation data from DWD
DWDRain_content <- getDWDRainStationList(recent = DWDRain_ftp_recent, historical = DWDRain_ftp_historical)

# the package defines a standard directory for storing user specific data permanently 
outputDir <- Local_R_DWD

# save the meta data to local files
if (!dir.exists(outputDir)) {dir.create(outputDir, recursive = T)}
fn <- paste0(outputDir, fn_DWD_content)
save(DWD_content, file = fn)
fn <- paste0(outputDir, fn_DWDRain_content)
save(DWDRain_content, file = fn)


# The package supports two ways of saving locally the daily DWD weather data either as zip files or as fst files
# In general the DWD data are available as historic data (older than approx. 1 year) and as recent data (up to approx. 1 year), the latter are updated daily
# It is therefore convenient to save the historic data as zip and/or fst files locally
# This consumes some disk space and takes time for the first download, but it is much faster to access the data later on

# download the historical data to local zip files
# only download the historical data for the first time or once per year
#if (!dir.exists(LocalCopy_DWD_ftp_historical)) {dir.create(LocalCopy_DWD_ftp_historical, recursive = T)}
#Copy_DWD_ZipFiles(DWD_ftp_historical, LocalCopy_DWD_ftp_historical)


# download the recent data to local zip files is an option, not a must
#if (!dir.exists(LocalCopy_DWD_ftp_recent)) {dir.create(LocalCopy_DWD_ftp_recent, recursive = T)}
#Copy_DWD_ZipFiles(DWD_ftp_recent, LocalCopy_DWD_ftp_recent)


# update the data to local fst file from the local zip files (isloadnew = F) or directly from the DWD ftp server (isloadnew = T)
# weather data interpolation uses the fst format
#UpdateDWDData_to_fst(dataperiod = "historical", startdate = "1990-01-01", isloadnew = T)


# if the data have been stored locally, they can be read from the local fst files
fn_histDWD_data <- paste0(fn_histDWD_data_str, as.character(1990),".fst")
fn <- here(Local_R_DWD, fn_histDWD_data)
weather_historic <- read.fst(path = fn)

# select the columns that are in the core, i.e. essential data set and rename them
weather_historic_core <- weather_historic[,c("Stations_id", "Date",longNames_DWD_core)]
rm(weather_historic)

ThisYear <- as.numeric(format(Sys.Date(), "%Y"))
# take the data frame with meta data for recent stations to a separate variable
RecentRainStationList <- DWDRain_content$recent$stationlist

# update the recent data to local fst file from the local zip files (isloadnew = F) or directly from the DWD ftp server (isloadnew = T)
# if you want to update the recent data, set UpdateRecentDWDdata <- T
UpdateRecentDWDdata <- F
if (UpdateRecentDWDdata) {
  StartTime <- Sys.time()
  UpdateDWDData_to_fst(dataperiod = "recent",
                       isloadnew = T,
                       DWD_content = DWD_content,
                       startdate = paste0(as.character(ThisYear-1),"-01-01"))

  EndTime <- Sys.time()
  print(paste("Time for UpdateDWDData_to_fst:", EndTime - StartTime))
}

# the name of the file is generated
fn_recentDWD_data <- paste0("recent_weather_dat_", as.character(1990),".fst")
weather_recent <- read.fst(path = here(Local_R_DWD, fn_recentDWD_data))
weather_recent  <- weather_recent[,c("Stations_id","Date",longNames_DWD_core)]

# For estimation of weather data the coordinates and height asl of the location are needed
geoBreite <- 52.7306
geoLaenge <- 10.6298
Hoehe_m <- 85

# make location data frame
Location <- data.frame(Latitude=as.numeric(geoBreite), Longitude=as.numeric(geoLaenge))

# make an sf object from Location data frame
Location <- st_as_sf(Location, coords = c("Longitude", "Latitude")) %>%
  st_set_crs(value = "+proj=longlat +datum=WGS84")


### interpolate the historic weather data for the location

# retrieve the recent station list from the DWD content
# we need a list with the available stations for the historic data
stationlist <- DWD_content$historical$stationlist

# make an sf object from stationlist data frame
stationlist <- st_as_sf(stationlist, coords = c("geoLaenge", "geoBreite")) %>%
  st_set_crs(value = "+proj=longlat +datum=WGS84")

# add the distance to the location to the stationlist data frame
stationlist$Distance_m <- pmax(1,as.numeric(st_distance(stationlist, Location))) # minimum distance is 1 m because
stationlist$Distance_km <- as.numeric(format(stationlist$Distance_m /1000, digits = 3))
# remove the geometry column
stationlist$geometry <- NULL
stationlist <- as.data.frame(stationlist)
startdate <- "1990-01-01"

# The list of the additional stations with rain data is needed
RainStationList <- DWDRain_content$historical$stationlist

# select the 3 nearest stations with rain data, for historical data it is necessary to select more than one station
# because stations with rain data changed over time
RainStations_selected <- SelectStations (lat=geoBreite,
                                         long=geoLaenge,
                                         height_loc=Hoehe_m,
                                         stationlist = RainStationList,
                                         #DWDRain_content$recent$stationlist,
                                         minstations=3,
                                         max_stations = 3,
                                         radius=80000,
                                         startdate=startdate,
                                         max.Height.Distance_m=200)


# get the historic rain data for the selected stations
df_Rain <- GetRainData_selection(RainStations_selected,
                                 DWDRain_content,#$historical,
                                 repository=DWDRain_ftp_historical,
                                 startdate="1990-01-01")


# add the distance to the rain station to the data frame

df_Rain <- left_join(df_Rain, RainStations_selected[,c("Stations_id", "Distance_km")], by="Stations_id")

# interpolate the weather data for the location
IntPolDWDHistorical <- InterpolateFromDWD(df_DWD_core = weather_historic_core, stationlist = stationlist,
                                          geoBreite = geoBreite, geoLaenge = geoLaenge,
                                          max.Height.Distance_m=100, Hoehe_m = Hoehe_m, df_Rain = df_Rain,
                                          startdate = startdate)


IntPolDWDdataHistorical <- IntPolDWDHistorical$DWDdata

# remove the data for the current and previous year as they are in the recent data
ThisYear <- as.numeric(format(Sys.Date(), "%Y"))
FirstDayOfYear <- as.Date(paste0(ThisYear-1,"-01-01"))
ExcelFirstDayofYear <- as.numeric(FirstDayOfYear - as.Date("1899-12-30"))
IntPolDWDdataHistorical <- IntPolDWDdataHistorical %>% filter(Time < ExcelFirstDayofYear)

# Add longitude and latitude to the data frame
IntPolDWDdataHistorical$Longitude <- geoLaenge
IntPolDWDdataHistorical$Latitude <- geoBreite
```

The interpolation should deliver daily estimates of the weather data for
the location:

``` r
summary(IntPolDWDdataHistorical)
#>       Time            TMPM              Rain              LF        
#>  Min.   :32874   Min.   :-18.300   Min.   : 0.000   Min.   : 33.70  
#>  1st Qu.:35978   1st Qu.:  4.300   1st Qu.: 0.000   1st Qu.: 72.20  
#>  Median :39083   Median :  9.600   Median : 0.100   Median : 81.30  
#>  Mean   :39083   Mean   :  9.536   Mean   : 1.912   Mean   : 79.68  
#>  3rd Qu.:42187   3rd Qu.: 15.200   3rd Qu.: 2.000   3rd Qu.: 88.80  
#>  Max.   :45291   Max.   : 28.900   Max.   :56.200   Max.   :100.00  
#>        VP           Sat_def          Rad_Int         GlobRad     
#>  Min.   : 1.20   Min.   : 0.000   Min.   : 10.2   Min.   : 0.88  
#>  1st Qu.: 6.90   1st Qu.: 1.000   1st Qu.: 37.0   1st Qu.: 3.20  
#>  Median : 9.50   Median : 2.100   Median :100.5   Median : 8.68  
#>  Mean   :10.05   Mean   : 3.028   Mean   :119.5   Mean   :10.33  
#>  3rd Qu.:12.90   3rd Qu.: 4.200   3rd Qu.:189.9   3rd Qu.:16.41  
#>  Max.   :23.30   Max.   :25.100   Max.   :347.0   Max.   :29.98  
#>       Wind            TMPMN            TMPMX          Longitude    
#>  Min.   : 0.500   Min.   :-22.40   Min.   :-12.40   Min.   :10.63  
#>  1st Qu.: 2.000   1st Qu.:  0.70   1st Qu.:  7.30   1st Qu.:10.63  
#>  Median : 2.700   Median :  5.40   Median : 14.10   Median :10.63  
#>  Mean   : 3.011   Mean   :  5.21   Mean   : 14.03   Mean   :10.63  
#>  3rd Qu.: 3.800   3rd Qu.: 10.10   3rd Qu.: 20.70   3rd Qu.:10.63  
#>  Max.   :12.200   Max.   : 21.00   Max.   : 39.20   Max.   :10.63  
#>     Latitude    
#>  Min.   :52.73  
#>  1st Qu.:52.73  
#>  Median :52.73  
#>  Mean   :52.73  
#>  3rd Qu.:52.73  
#>  Max.   :52.73
```

The average distance for the estimation of the different weather
variables can be calculated:

``` r
usedstations <- IntPolDWDHistorical$df.wf
usedstations$Distance <- 1/usedstations$InvDist
Diststations <- usedstations %>% group_by(variable) %>% dplyr::summarise(Distance = mean(Distance, na.rm = T))
```

The average distance for the estimation of the different weather
variables is:

``` r
Diststations
#> # A tibble: 8 × 2
#>   variable               Distance
#>   <fct>                     <dbl>
#> 1 LUFTTEMPERATUR            25.9 
#> 2 REL_FEUCHTE               26.0 
#> 3 WINDGESCHWINDIGKEIT       41.5 
#> 4 LUFTTEMPERATUR_MAXIMUM    26.7 
#> 5 LUFTTEMPERATUR_MINIMUM    26.7 
#> 6 NIEDERSCHLAGSHOEHE         6.43
#> 7 SONNENSCHEINDAUER         35.1 
#> 8 MHoeheWind                25.6
```

Now the same procedure for the recent data:

``` r
# recent data #######

## retrieve the recent station list from the DWD content

DWDrecent <- DWD_content$recent
stationlist <- DWDrecent$stationlist

# make an sf object from stationlist data frame
stationlist <- st_as_sf(stationlist, coords = c("geoLaenge", "geoBreite")) %>%
  st_set_crs(value = "+proj=longlat +datum=WGS84")

# add the distance to the location to the stationlist data frame
stationlist$Distance_m <- pmax(1,as.numeric(st_distance(stationlist, Location))) # minimum distance is 1 m because
stationlist$Distance_km <- format(stationlist$Distance_m /1000, digits = 3)
# remove the geometry column
stationlist$geometry <- NULL
stationlist <- as.data.frame(stationlist)
startdate <- paste0(as.character(ThisYear-1),"-01-01")

# select the neart stations with rain data
RainStation_selected <- SelectStations (lat=geoBreite,
                                        long=geoLaenge,
                                        height_loc=Hoehe_m,
                                        stationlist = RecentRainStationList,
                                        #DWDRain_content$recent$stationlist,
                                        minstations=3,
                                        max_stations = 3,
                                        radius=80000,
                                        startdate=startdate,
                                        max.Height.Distance_m=200)


# get the rain data for the selected stations

df_Rain <- GetRainData_selection(RainStations_selected,
                                 DWDRain_content,
                                 repository=DWDRain_ftp_recent,
                                 startdate=startdate)


# add the distance to the rain station to the data frame

df_Rain <- left_join(df_Rain, RainStations_selected[,c("Stations_id", "Distance_km")], by="Stations_id")


# interpolate the recent weather data for the location
IntpolDWDrecent <- InterpolateFromDWD(df_DWD_core = weather_recent, stationlist = stationlist, geoBreite = geoBreite, geoLaenge = geoLaenge,
                                      max.Height.Distance_m=100, Hoehe_m = Hoehe_m, df_Rain = df_Rain,
                                      startdate = startdate)

IntPolDWDdatarecent <- IntpolDWDrecent$DWDdata



IntPolDWDdatarecent$Longitude <- geoLaenge
IntPolDWDdatarecent$Latitude <- geoBreite
```

Here the summary of the result of the interpolation procedure for the
recent data:

``` r
summary(IntPolDWDdatarecent)
#>       Time            TMPM             Rain            LF       
#>  Min.   :45292   Min.   :-7.100   Min.   : 0.0   Min.   :41.10  
#>  1st Qu.:45404   1st Qu.: 4.200   1st Qu.: 0.0   1st Qu.:73.47  
#>  Median :45517   Median : 8.900   Median : 0.0   Median :81.55  
#>  Mean   :45517   Mean   : 9.526   Mean   : 2.2   Mean   :80.01  
#>  3rd Qu.:45629   3rd Qu.:15.500   3rd Qu.: 2.4   3rd Qu.:88.22  
#>  Max.   :45741   Max.   :24.500   Max.   :28.0   Max.   :99.00  
#>                  NA's   :2                       NA's   :2      
#>        VP           Sat_def          Rad_Int          GlobRad      
#>  Min.   : 2.80   Min.   : 0.100   Min.   : 10.20   Min.   : 0.880  
#>  1st Qu.: 6.80   1st Qu.: 1.100   1st Qu.: 31.75   1st Qu.: 2.745  
#>  Median : 9.20   Median : 2.000   Median : 96.60   Median : 8.345  
#>  Mean   :10.09   Mean   : 2.939   Mean   :112.98   Mean   : 9.761  
#>  3rd Qu.:13.15   3rd Qu.: 4.200   3rd Qu.:181.60   3rd Qu.:15.690  
#>  Max.   :21.90   Max.   :14.700   Max.   :343.30   Max.   :29.660  
#>  NA's   :2       NA's   :2        NA's   :2        NA's   :2       
#>       Wind           TMPMN             TMPMX         Longitude    
#>  Min.   :0.700   Min.   :-10.700   Min.   :-2.60   Min.   :10.63  
#>  1st Qu.:1.800   1st Qu.:  0.350   1st Qu.: 7.60   1st Qu.:10.63  
#>  Median :2.500   Median :  4.550   Median :13.10   Median :10.63  
#>  Mean   :2.692   Mean   :  5.039   Mean   :14.00   Mean   :10.63  
#>  3rd Qu.:3.400   3rd Qu.: 10.100   3rd Qu.:20.82   3rd Qu.:10.63  
#>  Max.   :8.000   Max.   : 17.900   Max.   :32.50   Max.   :10.63  
#>  NA's   :2       NA's   :2         NA's   :2                      
#>     Latitude    
#>  Min.   :52.73  
#>  1st Qu.:52.73  
#>  Median :52.73  
#>  Mean   :52.73  
#>  3rd Qu.:52.73  
#>  Max.   :52.73  
#> 
```

``` r
DWDdata <- rbind(IntPolDWDdataHistorical, IntPolDWDdatarecent)
```

Here the combinded data set:

``` r
summary(DWDdata)
#>       Time            TMPM              Rain              LF        
#>  Min.   :32874   Min.   :-18.300   Min.   : 0.000   Min.   : 33.70  
#>  1st Qu.:36091   1st Qu.:  4.300   1st Qu.: 0.000   1st Qu.: 72.22  
#>  Median :39308   Median :  9.600   Median : 0.100   Median : 81.30  
#>  Mean   :39308   Mean   :  9.536   Mean   : 1.922   Mean   : 79.69  
#>  3rd Qu.:42524   3rd Qu.: 15.200   3rd Qu.: 2.000   3rd Qu.: 88.80  
#>  Max.   :45741   Max.   : 28.900   Max.   :56.200   Max.   :100.00  
#>                  NA's   :2                          NA's   :2       
#>        VP           Sat_def          Rad_Int          GlobRad     
#>  Min.   : 1.20   Min.   : 0.000   Min.   : 10.20   Min.   : 0.88  
#>  1st Qu.: 6.90   1st Qu.: 1.000   1st Qu.: 36.83   1st Qu.: 3.18  
#>  Median : 9.50   Median : 2.100   Median :100.40   Median : 8.68  
#>  Mean   :10.05   Mean   : 3.025   Mean   :119.28   Mean   :10.31  
#>  3rd Qu.:12.90   3rd Qu.: 4.200   3rd Qu.:189.60   3rd Qu.:16.38  
#>  Max.   :23.30   Max.   :25.100   Max.   :347.00   Max.   :29.98  
#>  NA's   :2       NA's   :2        NA's   :2        NA's   :2      
#>       Wind          TMPMN             TMPMX          Longitude    
#>  Min.   : 0.5   Min.   :-22.400   Min.   :-12.40   Min.   :10.63  
#>  1st Qu.: 2.0   1st Qu.:  0.700   1st Qu.:  7.30   1st Qu.:10.63  
#>  Median : 2.7   Median :  5.400   Median : 14.00   Median :10.63  
#>  Mean   : 3.0   Mean   :  5.204   Mean   : 14.03   Mean   :10.63  
#>  3rd Qu.: 3.7   3rd Qu.: 10.100   3rd Qu.: 20.70   3rd Qu.:10.63  
#>  Max.   :12.2   Max.   : 21.000   Max.   : 39.20   Max.   :10.63  
#>  NA's   :2      NA's   :2         NA's   :2                       
#>     Latitude    
#>  Min.   :52.73  
#>  1st Qu.:52.73  
#>  Median :52.73  
#>  Mean   :52.73  
#>  3rd Qu.:52.73  
#>  Max.   :52.73  
#> 
```
