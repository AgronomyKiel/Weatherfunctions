
devtools::install_github("AgronomyKiel/Weatherfunctions")

rm(list=ls())
library(Weatherfunctions)
library(here)
library(fst)
library(dplyr)
library(tidyr)
library(sf)
# download meta data from DWD

DWD_content <- getDWDStationList(historical = DWD_ftp_historical, recent = DWD_ftp_recent)


DWDRain_content <- getDWDRainStationList(recent = DWDRain_ftp_recent, historical = DWDRain_ftp_historical)

outputDir <- Local_R_DWD

if (!dir.exists(outputDir)) {dir.create(outputDir, recursive = T)}
fn <- paste0(outputDir, fn_DWD_content)
save(DWD_content, file = fn)
fn <- paste0(outputDir, fn_DWDRain_content)
save(DWDRain_content, file = fn)


# download the recent data to local zip files
#if (!dir.exists(LocalCopy_DWD_ftp_recent)) {dir.create(LocalCopy_DWD_ftp_recent, recursive = T)}
#Copy_DWD_ZipFiles(DWD_ftp_recent, LocalCopy_DWD_ftp_recent)

# download the historical data to local zip files
# only download the historical data for the first time or once per year
#if (!dir.exists(LocalCopy_DWD_ftp_historical)) {dir.create(LocalCopy_DWD_ftp_historical, recursive = T)}
#Copy_DWD_ZipFiles(DWD_ftp_historical, LocalCopy_DWD_ftp_historical)


# update the data to local fst file from the local zip files (isloadnew = F) or directly from the DWD ftp server (isloadnew = T)
#UpdateDWDData_to_fst(dataperiod = "historical", startdate = "1990-01-01", isloadnew = T)



fn_histDWD_data <- paste0(fn_histDWD_data_str, as.character(1990),".fst")

fn <- here(Local_R_DWD, fn_histDWD_data)
weather_historic <- read.fst(path = fn)

# select the columns that are in the core, i.e. essential data set and rename them
weather_historic_core <- weather_historic[,c("Stations_id", "Date",longNames_DWD_core)]
rm(weather_historic)

ThisYear <- as.numeric(format(Sys.Date(), "%Y"))
# take the data frame with meta data for recent stations to a separate variable
RecentRainStationList <- DWDRain_content$recent$stationlist

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


fn_recentDWD_data <- paste0("recent_weather_dat_", as.character(1990),".fst")
weather_recent <- read.fst(path = here(Local_R_DWD, fn_recentDWD_data))
weather_recent  <- weather_recent[,c("Stations_id","Date",longNames_DWD_core)]

# set the location of the station
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
DWDhistorical <- DWD_content$historical
stationlist <- DWDhistorical$stationlist

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
RainStationList <- DWDRain_content$historical$stationlist
# select the nearest stations with rain data, for historical data it is necessary to select more than one station
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


# get the rain data for the selected stations

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
summary(IntPolDWDdataHistorical)

usedstations <- IntPolDWDHistorical$df.wf
usedstations$Distance <- 1/usedstations$InvDist
Diststations <- usedstations %>% group_by(variable) %>% dplyr::summarise(Distance = mean(Distance, na.rm = T))
Diststations


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

summary(IntPolDWDdatarecent)


DWDdata <- rbind(IntPolDWDdataHistorical, IntPolDWDdatarecent)
summary(DWDdata)

