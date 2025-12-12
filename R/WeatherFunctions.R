# Library of functions for the handling of weather data
# directly downloaded from the DWD ftp server
# or from the local copy of the DWD ftp server
# or from files in the local RData format
# additionally it contains functions for generating radiation data from sunshine hours
# and for the calculation of potential evapotranspiration

# Libraries ---------------------------------------------------------------

#library(curl)
library(RCurl)
# library(scales)
library(stringi)
library(sf)
library(lubridate)
library(fst)
library(data.table)
library(dplyr)
library(tidyr)
library(httr)
library(stringr)
library(here)
library(zoo)
library(ggnewscale)
library(purrr)
#library(ggplot2)
library(plotly)
library(scales)

#' Internal hook to optionally initialize the data directory when the user opts
#' in via `options(weatherfunctions.auto_setup = TRUE)`.
.onLoad <- function(...) {
  if (isTRUE(getOption("weatherfunctions.auto_setup", FALSE))) {
    weatherfunctions_data_dir(create = TRUE, ask = FALSE)
  }
}



#' Return the Weatherfunctions data directory path and optionally create it.
#'
#' The directory is not created automatically on package load to avoid CRAN
#' side effects. Users can call this helper explicitly or opt into automatic
#' creation by setting `options(weatherfunctions.auto_setup = TRUE)` before
#' loading the package.
#' @param create Logical; create the directory if it does not exist.
#' @param ask Logical; when interactive and creation is requested, ask before
#'   writing to the file system.
#' @return A character string with the path used for local cache files.
#' @export weatherfunctions_data_dir
weatherfunctions_data_dir <- function(create = FALSE, ask = interactive()) {
  path <- tools::R_user_dir("Weatherfunctions", which = "data")

  if (create && !dir.exists(path)) {
    if (ask && interactive()) {
      prompt <- paste0(
        "Weatherfunctions would like to create a data directory at \"",
        path,
        "\". Create it now?"
      )

      if (!isTRUE(utils::askYesNo(prompt, default = TRUE))) {
        warning(
          "Data directory was not created; functions that cache to disk may fail until setup is completed.",
          call. = FALSE
        )
        return(path)
      }
    }

    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

#' Explicitly initialize the Weatherfunctions data directory.
#'
#' This is a user-facing helper to establish the cache directory in a way that
#' avoids performing file system writes during package load. Users can call it
#' directly (e.g., from their `.Rprofile`) or enable automatic setup via the
#' `weatherfunctions.auto_setup` option before attaching the package.
#' @inheritParams weatherfunctions_data_dir
#' @export initialize_weatherfunctions_data_dir
initialize_weatherfunctions_data_dir <- function(ask = interactive()) {
  weatherfunctions_data_dir(create = TRUE, ask = ask)
}

DataDir <- weatherfunctions_data_dir(create = FALSE)



OSname <- Sys.info()["sysname"]

if (OSname == "Windows") {
  path_del <- "\\"
} else {
  path_del <- "/"
}

# Directories -------------------------------------------------------------

.datatable.aware = TRUE

## DWD repositories for synoptic weather data ########
#' @export DWD_ftp_historical
DWD_ftp_historical <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"

#' @export DWD_ftp_recent
DWD_ftp_recent <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/"

## DWD repositories for additional rainfall data ########
#' @export DWDRain_ftp_historical
DWDRain_ftp_historical <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/more_precip/historical/"

#' @export DWDRain_ftp_recent
DWDRain_ftp_recent <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/more_precip/recent/"


# some constants
#' @export l_h_v_water
l_h_v_water <- 2.477 * 1E6 # latent evaporation energy of  water at  10 °C in [J/Kg] }

#' @export Psycro
Psycro      <- 0.000662    # Psychrometric "constant" [1/°K] }

#' @export Karman_const
Karman_const <- 0.41       # { von Karman-constant [-] }

#' @export sigma
sigma <- 4.903e-9 # Stefan-Boltzmann-Constant in [MJ m-2 d-1 K-4]

##' @export Kelvin0
Kelvin0 <- 273.15 # Kelvin 0




## Local directories for DWD data ####

### Local folders for copies of DWD data ####
#' @export LocalCopy_DWD_ftp_historical
LocalCopy_DWD_ftp_historical <- paste(DataDir,"LocalCopyDWD","kl","historical", sep = "/")

#' @export LocalCopy_DWD_ftp_recent
LocalCopy_DWD_ftp_recent <- paste(DataDir, "LocalCopyDWD","kl","recent", sep = "/")

### Local folders for copies of additional DWD rainfall data ####
#' @export LocalCopy_DWD_Rain_ftp_historical
LocalCopy_DWD_Rain_ftp_historical <- paste(DataDir, "LocalCopyDWD","more_precip","historical", sep = "/")

#' @export LocalCopy_DWD_Rain_ftp_recent
LocalCopy_DWD_Rain_ftp_recent <- paste(DataDir,  "LocalCopyDWD","more_precip","recent", sep = "/")

### local DWD Rdata paths ####
#' @export Local_DWD
Local_DWD <- paste(DataDir, "LocalCopyDWD", sep = "/")

#' @export Local_R_DWD
Local_R_DWD <- paste(DataDir, "LocalCopyDWD","Rdata", sep = "/")

#' @export Local_DWD_historical
Local_DWD_historical <- paste(DataDir, "historical", sep = "/")

#' @export Local_DWD_recent
Local_DWD_recent <- paste(DataDir, "recent", sep = "/")

#' @export Local_Rain_DWD_historical
Local_Rain_DWD_historical <- paste(DataDir, "historical", sep = "/")

#' @export Local_Rain_DWD_recent
Local_Rain_DWD_recent <- paste(DataDir, "recent", sep = "/")

#' @export Local_Rdata_path
Local_Rdata_path <- paste(DataDir, "LocalCopyDWD","Rdata", sep = "/")

#' @export path_unzip
path_unzip <- paste(DataDir, "DWD_tmp", sep="/")



### local RData file names ####
#' @export HistoricalDWD
HistoricalDWD <- "historicalDWDweather.RData"

#' @export HistoricalDWDRain
HistoricalDWDRain <- "historicalDWDRain.RData"

### outputpath ####
#' @export datapath
datapath <- paste(DataDir, "data")

#' @export figurepath
figurepath <- paste(DataDir, "figures")


## local RData file names
#' @export fn_DWD_content
fn_DWD_content <- "DWD_content.Rdata"

#' @export fn_DWDRain_content_recent
fn_DWDRain_content_recent <- "DWDRain_content_recent.Rdata"

#' @export fn_DWDRain_content
fn_DWDRain_content <- "DWDRainStationlist.RData"

#' @export fn_histDWD_data_str
fn_histDWD_data_str <- "weather_dat_"

#' @export fn_HistoricalDWD
fn_HistoricalDWD <- "historicalDWDweather.RData"

#' @export fn_HistoricalDWDRain
fn_HistoricalDWDRain <- "historicalDWDRain.RData"




# Constants ---------------------------------------------------------------

# During the data handling the following names are used for the weather data

# names_DWD is the short name for the weather data from original DWD data

#' @export names_DWD
names_DWD <- c("QN_4", # QUALITAETS_NIVEAU
               "TMK",  # LUFTTEMPERATUR
               "PM",   # LUFTDRUCK_STATIONSHOEHE
               "UPM",  # REL_FEUCHTE
               "FM",   # WINDGESCHWINDIGKEIT
               "TXK",  # LUFTTEMPERATUR_MAXIMUM
               "TNK",  # LUFTTEMPERATUR_MINIMUM
               "RSK",  # NIEDERSCHLAGSHOEHE
               "SDK"   # SONNENSCHEINDAUER
               )

# longNames_DWD is the long name for the weather data from original DWD data

#' @export longNames_DWD
longNames_DWD <- c( "QUALITAETS_NIVEAU",
                    "LUFTTEMPERATUR",
                    "LUFTDRUCK_STATIONSHOEHE",
                    "REL_FEUCHTE",
                    "WINDGESCHWINDIGKEIT",
                    "LUFTTEMPERATUR_MAXIMUM",
                    "LUFTTEMPERATUR_MINIMUM",
                    "NIEDERSCHLAGSHOEHE",
                    "SONNENSCHEINDAUER"
)


# longNames_DWD is the long name for the weather data from original DWD data

#' @export longNames_DWD_en
longNames_DWD_en <- c( "QUALITY_LEVEL",
                    "AIRTEMPERATURE",
                    "AIR_PRESSURE_STATIONHEIGHT",
                    "REL_HUMIDITY",
                    "WIND_SPEED",
                    "AIRTEMPERATURE_MAXIMUM",
                    "AIRTEMPERATURE_MINIMUM",
                    "RAINFALL",
                    "SUNSHINE_DURATION"
)



# longNames_DWD_core is the long name for the core weather data from original DWD data
# needed to calculate potential evapotranspiration according to the Penman-Monteith method

#' @export longNames_DWD_core
longNames_DWD_core <- c( "LUFTTEMPERATUR",
                    "REL_FEUCHTE",
                    "WINDGESCHWINDIGKEIT",
                    "LUFTTEMPERATUR_MAXIMUM",
                    "LUFTTEMPERATUR_MINIMUM",
                    "NIEDERSCHLAGSHOEHE",
                    "SONNENSCHEINDAUER",
                    "MHoeheWind"
)

#' @export longNames_DWD_core_en
longNames_DWD_core_en <- c( "AIRTEMPERATURE",
                         "REL_HUMIDITY",
                         "WIND_SPEED",
                         "AIRTEMPERATURE_MAXIMUM",
                         "AIRTEMPERATURE_MINIMUM",
                         "RAINFALL",
                         "SUNSHINE_DURATION",
                         "MEASUREMENT_HEIGHT_WIND"
)



#' @export RainDataColumns
RainDataColumns <- c("STATIONS_ID", "MESS_DATUM", "QN_6", "RS", "RSF", "SH_TAG", "NSH_TAG")

#' @export RainDataColumnsLong
RainDataColumnsLong <- c("Station_id", "MESS_DATUM", "QUALITAETS_NIVEAU",
                         "NIEDERSCHLAGSHOEHE", "NIEDERSCHLAGSFORM", "SCHNEEHOEHE", "NEUSCHNEEHOEHE")

#' @export RainDataUnits
RainDataUnits <- c("", "YYYYMMDD", "code", "mm", "code", "cm", "cm")

#' @export RainDataColumnsLong_en
RainDataColumnsLong_en <- c("Station_id", "Date", "Quality_level",
                         "Rainfall", "Rainfall_type", "Snow_height", "New_snow_height")

#' @export RainDataUnits_en
RainDataUnits_en <- c("", "YYYYMMDD", "code", "mm", "code", "cm", "cm")




# units of the weather data from original DWD data

#' @export units_DWD
units_DWD <- c("code","Tagesmittel der Temperatur °C", "Tagesmittel des Luftdrucks hPa",
               "Tagesmittel der Relativen Feuchte %", "Tagesmittel Windgeschwindigkeit m/s",
               "Tagesmaximum der Lufttemperatur in 2m Höhe °C", "Tagesminimum der Lufttemperatur in 2m Höhe °C",
               "tägliche Niederschlagshöhe mm", "tägliche Sonnenscheindauer h")

#' @export units_DWD_en
units_DWD_en <- c("code","Mean air temperature °C", "Mean air pressure hPa",
               "Mean relative humidity %", "Mean wind speed m/s",
               "Daily maximum air temperature °C", "Daily minimum air temperature °C",
                 "Rainfall rate mm", "Sunshine duration h")


# data frames with names and units for DWD data
#' @export df_names_DWD
df_names_DWD <- data.frame(names_DWD, longNames_DWD, units_DWD, longNames_DWD_en)

# add brackets to the units
df_names_DWD <- df_names_DWD %>% mutate(ylabels = paste0(longNames_DWD, " [", units_DWD, "]"),
                                        ylabels_en = paste0(longNames_DWD_en, " [", units_DWD_en, "]"))
df_Rainnames_DWD <- data.frame(RainDataColumns, RainDataColumnsLong, RainDataUnits, RainDataColumnsLong_en, RainDataUnits_en)

# add brackets to the units
df_Rainnames_DWD <- df_Rainnames_DWD %>% mutate(ylabels = paste0(RainDataColumnsLong, " [", RainDataUnits, "]"),
                                                ylabels_en = paste0(RainDataColumnsLong_en, " [", RainDataUnits_en, "]"))



# Names and units for Hume data
#' @export namesHUME
namesHUME <- c("Time", "TMPM", "Rain", "LF", "VP", "Sat_def", "Rad_Int", "GlobRad", "Wind", "TMPMN", "TMPMX")
#' @export unitsHUME
unitsHUME <- c("[day]", "[°C]", "[mm/d]", "[%]", "[mbar]", "[mbar]", "[W/m²]", "[MJ/m²/d]", "[m/s]", "[°C]", "[°C]")

#' @export longNamesHUME
longNamesHUME <- c("Time", "Mittel Lufttemperatur", "Niederschlagshöhe", "Luftfeuchtigkeit",
                   "Dampfdruck", "Sättigungsdampfdruckdefizit", "Globalstrahlung",
                   "Globalstrahlung", "Windgeschwindigkeit", "Minimum Lufttemperatur",
                   "Maximum Lufttemperatur")

df_names_HUME <- data.frame(names = namesHUME, longNames = longNamesHUME, units=unitsHUME)
df_names_HUME <- df_names_HUME %>% mutate(ylabels = paste0(longNamesHUME, " ",unitsHUME) )

# idvars and measvars for the weather data
idvars <- c("Date","Stationsname","geoBreite", "geoLaenge", "Stationshoehe", "Stations_id")

measvars <- c("LUFTTEMPERATUR",
              "LUFTTEMPERATUR_MAXIMUM",
              "LUFTTEMPERATUR_MINIMUM",
              "NIEDERSCHLAGSHOEHE",
              "REL_FEUCHTE",
              "WINDGESCHWINDIGKEIT",
              "EstGlobRad",
              "VP",
              "Sat_def",
              "Rad_Int")

CalcVars <- c("CumRain", "CumRad", "TempSum", "PT", "cPT", "Rn", "ra", "rc", "pETP", "cumETP", "climWbal", "cumWbal")

UnitsCalcVars <- c("[mm]", "[MJ/m²]", "[°Cd]", "[MJ/(°Cd)]", "[mm]", "[W/m²]", "[s/m]", "[s/m]", "[mm]", "[mm]", "[mm]", "[mm]")

LongNamesCalcVars <- c("Jahressumme Niederschlag",
                       "Jahressumme Globalstrahlung",
                       "Temperatursumme",
                       "Photothermalwert",
                       "Jahressumme Photothermalwert",
                       "Nettostrahlung",
                       "Aerodynamischer Widerstand",
                       "Canopy Widerstand",
                       "Potentielle Evapotranspiration",
                       "Jahressumme potentielle Evapotranspiration",
                       "Klimatische Wasserbilanz",
                       "Jahressumme klimatische Wasserbilanz")

df_names_CalcVars <- data.frame(names = CalcVars, longNames = LongNamesCalcVars, units=UnitsCalcVars)
df_names_CalcVars <- df_names_CalcVars %>% mutate(ylabels = paste0(longNames, " ",units) )

df_names <- rbind( df_names_HUME, df_names_CalcVars)




# Functions ---------------------------------------------------------------


# Utility-Functions ---------------------------------------------------------------


#' Title: ReplaceProblemChars
#'
#' @param InputString A string with characters to be replaced
#'
#' @return A string with replaced characters
#' @export
#'
# @examples ReplaceProblemChars("Test / Test")
ReplaceProblemChars <- function(InputString=NULL) {
  InputString <- trimws(InputString)
  InputString <- gsub(" / ",replacement = "_", x = InputString)
  InputString <- gsub(" (",replacement = "_", x = InputString, fixed = T)
  InputString <- gsub(")",replacement = "", x = InputString, fixed = T)
  InputString <- gsub(" ",replacement = "_", x = InputString, fixed = T)
  InputString <- gsub(".",replacement = "", x = InputString, fixed = T)
  return(InputString)
}


# Graphic routines ----------------------------------------------------------------

#' Title
#'
#' @param plot a plot to be customized
#' @param BaseSize the base font size
#'
#' @returns a plot with customized design and an adjusted font size
#' @export
#' @import ggplot2
#'
#' @examples  setplotbackground(p, BaseSize=18)
setplotbackground <-  function (plot, BaseSize=18){
  #  plot <- plot +  scale_colour_brewer(palette="Set1")
  plot <- plot + ggplot2::theme_bw(base_size = BaseSize)
  plot <- plot + ggplot2::theme(plot.background = element_rect(fill = NULL,colour = NA))
  plot <- plot + ggplot2::theme(axis.line = element_line(size = 1, linetype = "solid", color="black"))
  plot <- plot + ggplot2::theme(axis.ticks.length = unit(.15, "cm"))
  plot <- plot + ggplot2::theme(axis.ticks = element_line(colour = 'black', size = 1, linetype = 'solid'))
  plot <- plot + ggplot2::theme(axis.text=element_text(colour="black"))
  plot <- plot + ggplot2::theme(panel.border = element_rect(size=1.5, fill=NA, color="black"))
  plot <- plot + ggplot2::theme(panel.background = element_rect(fill = NULL,colour = NA))
#  plot <- plot + ggplot2::theme(plot.background = element_rect(fill = "transparent"))
  plot <- plot + ggplot2::theme(legend.background = element_blank())
}

#' Title
#'
#' @param p a plot to be customized
#' @param fontsize the basic font size
#'
#' @returns a plot with customized design and an adusted font size
#' @export
#' @import ggplot2
#'
#' @examples set_theme(p, fontsize=18)
set_theme <- function(p, fontsize){
#  p <- p + theme_bw(base_family = FontFamilie)
  p <- p + theme(text = element_text(size=fontsize))
  p <- p + theme(panel.border = element_blank())
  p <- p + theme(axis.ticks = element_line(size = 1.2, linetype = "solid", color="black"))
  p <- p + theme(axis.ticks.length=unit(0.2, "cm"))
  p <- p + theme(plot.background = element_rect(fill = NULL,colour = NA))
  p <- p + theme(axis.line = element_line(size = 1, linetype = "solid", color="black"))
  p <- p + theme(panel.border = element_rect(size=1.2, fill=NA))
  p <- p + theme(panel.background = element_blank())
  p <- p + theme(plot.background = element_rect(fill = "transparent", colour = NA))
  p <- p + theme(plot.title =  element_text( face="bold", colour = "black"))
  p <- p + theme(plot.subtitle = element_text( face="bold", colour = "black"))
  p <- p + theme(axis.text = element_text( face="bold", colour = "black"))
  p <- p + theme(axis.title = element_text( face="bold", colour = "black"))
  p <- p + theme(axis.ticks = element_line(size = 1.2, linetype = "solid", color="black"))
  p <- p + theme(axis.ticks.length=unit(0.2, "cm"))
  p <- p + theme(strip.text = element_text(face="bold"))
  p <- p + theme(strip.background =element_blank())
  p <- p + theme(panel.grid.major = element_line(size = 1, linetype = "solid", color="grey"))
  p <- p + theme(panel.grid.minor = element_line(size = 0.5, linetype = "solid", color="grey"))
  p <- p + theme(plot.background = element_rect(fill = NULL,colour = NA))
  p <- p + theme(axis.line = element_line(size = 1, linetype = "solid", color="black"))

}




#' Utility function to clean up plotly legend entries
#'
#' @param .plotly_x
#' @param .extract_str
#'
#' @returns
#' @import plotly
#'
#' @examples
clean_plotly_leg <- function(.plotly_x, .extract_str) {
  # Inpects an x$data list in a plotly object, cleans up legend values where appropriate
  if ("legendgroup" %in% names(.plotly_x)) {
    # The list includes a legend group

    .plotly_x$legendgroup <- stringr::str_extract(.plotly_x$legendgroup, .extract_str)
    .plotly_x$name <- stringr::str_extract(.plotly_x$name, .extract_str)

  }
  .plotly_x


}

#' makeplot makes time series plots from a data frame with Time in the first column as Excel date
#' and the parameter to be plotted in another selected column
#'
#' @param df data frame with weather/simulation data
#' @param parameter Column to be plotted
#' @param BaseSize Base char size for the plot
#' @param ylabel The y label string
#' @param SelYear The year to be highlighted
#' @param ShiftYears Shift the years for autumn sown crops
#' @param StartMonth The start month for the cultivation year
#' @param smoothing Smooth the historic data with a loess function
#' @param span The span for the loess function
#' @param plotly Use plotly for interactive plots
#'
#' @return A ggplot object
#' @export
#' @import ggplot2
#' @import tidyr
#'
#' @examples
makeplot <- function(df, parameter, BaseSize=18, ylabel="", SelYear=0,
                     ShiftYears=F, StartMonth=9, smoothing=F, span=0.5, plotly=F){
  if (is.null(df)) {
    stop("No data frame for plotting")
  }
  if (is.null(df$Time)){
    stop("No Time column in data frame")}

  # add date, month, day of the year and year to the data frame
  df$Date <- as.Date(df$Time, origin = "1899-12-30")
  df$Month <- month(df$Date)
  df$DOY <- yday(df$Date)
  df$Year <- year(df$Date)

  # add parameter "SumYear", a time shifted cultivation year, starting in the autumn
  # before the following harvest year
  # for calculations of year summation parameters other than the calendar year
  if (ShiftYears) {
    df$SumYear <- ifelse(df$Month >= StartMonth, df$Year+1, df$Year)
  }
  else {
    df$SumYear <- df$Year
  }

  # format as numeric and use actual year if nothing is selected
  SelYear <- as.numeric(SelYear)
  if (SelYear==0) {
    ThisYear <- substr(as.character(Sys.Date()), 1,4)
    SelYear <- ThisYear
  }
  SelYear <- as.numeric(SelYear)

  y_label <- ifelse(ylabel=="", parameter, ylabel)

  # select an y label from the df_names data frame defined in the Constants section
  tmp <- df_names %>% filter(names==parameter) %>% dplyr::select(ylabels)
  if (nrow(tmp)>0) {
    y_label <- tmp$ylabels
  }

  # select a string for the plot title from the df_names data frame defined in the Constants section
  tmp <- df_names %>% filter(names==parameter) %>% dplyr::select(longNames)
  if (nrow(tmp)>0) {
    plot_title <- tmp$longNames
  } else {plot_title <- ""}

  # select and check columns
  selcols <- c("Date", "Month", "DOY","Year", "SumYear",parameter)
  stopifnot(parameter %in% names(df))
  # select the columns
  df <- df[,selcols]
  df <- as.data.frame(df)
  df$param <- df[,parameter]

  # calculate the number of days for each year, to filter for full years
  # especially for summed up parameters
  tmpnDays <- aggregate(DOY ~ SumYear, data = df, FUN = length)
  tmpnDays$nDays <- tmpnDays$DOY
  tmpnDays$DOY <- NULL
  df <- merge(df, tmpnDays, by="SumYear")
  rm(tmpnDays)

  # set all years to the selected year and shift back one year if the month is before the start month
  # in order to have continuous time series for autumn sown crops
  df$Year <- SelYear
  if (ShiftYears) {
    df$Year <- ifelse(df$Month >= StartMonth, df$Year-1, df$Year)
    SelDateRange <- as.Date(paste0(as.character(SelYear-1),"-", StartMonth,"-01")):(as.Date(paste0(as.character(SelYear),"-", StartMonth,"-01"))-1)
  } else {
    SelDateRange <- as.Date(paste0(as.character(SelYear),"-", StartMonth,"-01")):(as.Date(paste0(as.character(SelYear+1),"-", StartMonth,"-01"))-1)
  }
  SelDateRange <- as.Date(SelDateRange, origin="1970-01-01")

  # select the data for the selected year
  df$Selected <- ifelse(df$Date %in% SelDateRange, T, F)

  # re calculate the date according to the corrected year and the day of the year
  df$Date <- as.Date(paste0(as.character(df$Year-1),"-12-31"))+df$DOY
  df[,parameter] <- NULL
  #  df <- df %>% group_by(Year) %>% mutate(nDays =  n())


  # calculate 25 and 75% percentile statistics for each julian day of the year and filter for full years
  df_LT <- df %>% filter(nDays >=364) %>% group_by(Date) %>% #DN statt 365
    summarise(param = quantile(param, c(0.25, 0.75), na.rm=T), q = c(0.25, 0.75)) %>%
    pivot_wider(id_cols = Date, values_from = param, names_from = q, names_prefix = "q")

  # calculate the min, max and median for each julian day of the year of the selected paramter
  dfExtreme <- df  %>% filter(nDays >=364) %>% group_by(Date) %>%  #DN statt 365
    summarise(max = max(param), median=median(param), min=min(param))

  # combine both data frames for parameter statistics
  df_stat <- left_join(x = df_LT, y = dfExtreme, by="Date")
  # copy back to df_LT
  df_LT <- df_stat

  # shift df extreme with min, max and median to long format for plotting with legend
  dfExtreme <- dfExtreme %>% pivot_longer(cols = c("max", "min", "median"), names_to = "stat", values_to = "value")

  # copy all statistics to long format for plotting with legend
  df_l <- df_stat %>% pivot_longer(cols = c("median", "max", "min", "q0.25","q0.75" ), names_to = "stat", values_to = "value")
  df_l <- df_l %>% mutate(lty=ifelse(stat%in% c("min","max"), 2, 1)) %>% mutate(DOY=yday(Date))

  # if smoothing is selected create smoothed data for the ribbon
  if (smoothing==T) {

    # smooth the min/max median data
    dfsmooth <- df_l %>% mutate(Datenum=as.numeric(Date)) %>%
      group_by(stat) %>%
      arrange(stat, Datenum) %>%
      nest() %>%
      mutate(pred = purrr::map(data, function(x)stats::loess(value~Datenum, span= span, data = x) %>%
                                 stats::predict(data.frame(Datenum = seq(min(x$Datenum), max(x$Datenum), 1))))) %>%
      unnest(cols = c(data, pred))

    # create df for the ribbon from smoothed data
    df_ribbon <- dfsmooth %>% filter(stat %in% c("q0.25","median", "q0.75")) %>% dplyr::select(-value) %>%
      pivot_wider(names_from = stat, values_from = pred)
  } else {
    # create df for the ribbon from unsmoothed data
    df_ribbon <- df_LT
  }

  # create custom linetype and color scales
  Myltys <- c("dotted", "solid", "dotted", "solid", "solid", "solid")
  Mycolors <- c("darkgreen", "darkblue", "darkgreen", "darkgreen", "darkgreen", "darkgreen")


  #  df_LT$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_LT$DOY

  # select the values
  SelYearValues <- df[df$Selected==T, c( "Date", "param")]
  SelYearValues <- as.data.frame(SelYearValues)
  # Check if data are forecast data
  SelYearValues$forecast <- SelYearValues$Date >= max(as.Date(Sys.Date()))


  SelYearColours <- c("darkgreen", "red", "blue","red","blue")

  # check if there are any forecast data
  IsAnyForecast <- any(SelYearValues$forecast)
  if (IsAnyForecast) {
    ForecastData <- SelYearValues[SelYearValues$forecast==T,]
    SelYearValues$forecast <- ifelse(SelYearValues$forecast, "Vorhersage", "Messung")
  }

  p <- ggplot()
  p <- p + ggtitle(plot_title)
  # p <- p + geom_ribbon(data = df_ribbon, aes(x=Date, ymax=q0.75, ymin=q0.25), fill="green", alpha=0.3)
  p <- p + geom_ribbon(data = df_ribbon, aes(x=Date, ymax=q0.75, ymin=q0.25, fill = "Quartilsabstand = mittlere 50%"), alpha=0.3)
  p <- p + geom_line(data=df_ribbon, aes(x=Date, y=median), color="darkgreen", size=0.5, lty=1)
  #  p <- p + geom_line(data=dfsmooth, aes(x=Date, y=pred, color=stat, lty=stat), size=0.5)
  p <- p + geom_line(data=dfExtreme, aes(x=Date, y=value, color=stat) , size=0.8, lty="dotted")
  p <- p + scale_color_discrete(name= "historische Wetterdaten")
  p <- p + guides(color=guide_legend(direction='vertical'))
  p <- p + scale_x_date(labels = label_date("%b"), date_breaks = "1 month")
  #  p <- p + structure(ggplot2::standardise_aes_names("colour"), class = "new_aes")
  #  p <- p + new_scale_color()
  #  browser()
  #  p <- p + geom_line(data=SelYearValues, aes(x=Date, y=param, color=forecast, group=1), size=1)
  p <- p + geom_line(data=SelYearValues, aes(x=Date, y=param), color="darkgreen", size=1)
  if (IsAnyForecast) {
    p <- p + geom_line(data=ForecastData, aes(x=Date, y=param), color="red", size=1)
  }
  p <- p + scale_color_manual(name= "Wetterdaten im akt. Jahr", values = SelYearColours)
  p <- p + guides(color=guide_legend(direction='vertical'))
  #  p <- p + guides(color=guide_legend(nrow=3))
  p <- p + scale_fill_manual(name= "" ,values = c("Quartilsabstand = mittlere 50%"="green"))
  p <- p + ylab(y_label) + xlab("Monat")
  p <- p + theme_bw(base_size = BaseSize)
  p <- p + theme(legend.position="bottom",
                 legend.box="vertical",
                 legend.box.just = "left",
                 legend.margin=margin(),
                 legend.text=element_text(size=12),
                 legend.title=element_text(size=12))
  if (plotly==T) {
    p <- p + guides(color="none", fill="none")
    p <- ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.4, y =-0.2)) # %>% layout(autosize=FALSE)
    p$x$data <- p$x$data %>%
      map(clean_plotly_leg, "[^\\(][^,]*") # ie remove the opening bracket,
  }
  #  p
  return(p)
}



#' makeScenarioplot makes time series plots from a data frame with Time in the first column as Excel date
#' and the parameter to be plotted in another selected column
#'
#' @param df_hist data frame with historic weather/simulation data
#' @param df_scen data frame with scenario weather/simulation data
#' @param parameter Column to be plotted
#' @param BaseSize Base char size for the plot
#' @param ylabel The y label string
#' @param SelYear The year to be highlighted
#' @param ShiftYears Shift the years for autumn sown crops
#' @param StartMonth The start month for the cultivation year
#' @param StartScenarioDate The start date for the scenario, i.e. the first day with unkown weather
#' @param smoothing Smooth the historic data with a loess function
#' @param span The span for the loess function
#' @param plotly Use plotly for interactive plots
#'
#' @return A ggplot object
#' @export
#'
#' @examples
makeScenarioplot <- function(df_hist,
                             df_scen,
                             parameter,
                             BaseSize=18,
                             ylabel="",
                             SelYear=0,
                             ShiftYears=F,
                             StartMonth=9,
                             StartScenarioDate=NULL,
                             # LastrecentDateDate = NULL,
                             smoothing=F,
                             span=0.5,
                             plotly=F){


  if (is.null(df_hist)) {
    stop("No historic data frame for plotting")
  }
  if (is.null(df_scen)) {
    stop("No scenario data frame for plotting")
  }
  if (is.null(df_hist$Time)){
    stop("No Time column in hist data frame")}
  if (is.null(df_scen$Time)){
    stop("No Time column in scenario data frame")}

  # add date, month, day of the year and year to the data frame
  df_hist$Date <- as.Date(df_hist$Time, origin = "1899-12-30")
  df_hist$Month <- month(df_hist$Date)
  df_hist$DOY <- yday(df_hist$Date)
  df_hist$Year <- year(df_hist$Date)

  # add parameter "SumYear", a time shifted cultivation year, starting in the autumn
  # before the following harvest year
  # for calculations of year summation parameters other than the calendar year
  if (ShiftYears) {
    df_hist$SumYear <- ifelse(df_hist$Month >= StartMonth, df_hist$Year+1, df_hist$Year)
  }else {
    df_hist$SumYear <- df_hist$Year
  }

  # add date, month, day of the year and year to the data frame
  df_scen$Date <- as.Date(df_scen$Time, origin = "1899-12-30")
  df_scen$Month <- month(df_scen$Date)
  df_scen$DOY <- yday(df_scen$Date)
  df_scen$Year <- year(df_scen$Date)

  # add parameter "SumYear", a time shifted cultivation year, starting in the autumn
  # before the following harvest year
  # for calculations of year summation parameters other than the calendar year
  if (ShiftYears) {
    df_scen$SumYear <- ifelse(df_scen$Month >= StartMonth, df_scen$Year+1, df_scen$Year)
  }else {
    df_scen$SumYear <- df_scen$Year
  }


  # format as numeric and use actual year if nothing is selected
  SelYear <- as.numeric(SelYear)
  if (SelYear==0) {
    ThisYear <- substr(as.character(Sys.Date()), 1,4)
    SelYear <- ThisYear
  }
  SelYear <- as.numeric(SelYear)

  # if label for y axis is not given use the parameter name
  y_label <- ifelse(ylabel=="", parameter, ylabel)

  # select an y label from the df_names data frame defined in the Constants section
  tmp <- df_names %>% filter(names==parameter) %>% dplyr::select(ylabels)
  if (nrow(tmp)>0) {
    y_label <- tmp$ylabels
  }

  # select a string for the plot title from the df_names data frame defined in the Constants section
  tmp <- df_names %>% filter(names==parameter) %>% dplyr::select(longNames)
  if (nrow(tmp)>0) {
    plot_title <- tmp$longNames
  } else {plot_title <- ""}

  # select and check columns
  selcols <- c("Date", "Month", "DOY","Year", "SumYear", parameter)
  stopifnot(parameter %in% names(df_hist))
  # select the columns
  df_hist <- as.data.frame(df_hist)
  df_hist <- df_hist[,selcols]
  # copy the parameter to a new column
  # reanme the parameter column to param for the ggplot function
  df_hist$param <- df_hist[,parameter]
  # same for the scenario data
  df_scen <- as.data.frame(df_scen)
  df_scen <- df_scen[,selcols]
  df_scen$param <- df_scen[,parameter]


  # calculate the number of days for each year, to filter for full years
  # especially for summed up parameters
  tmpnDays <- aggregate(DOY ~ SumYear, data = df_hist, FUN = length)
  tmpnDays$nDays <- tmpnDays$DOY
  tmpnDays$DOY <- NULL
  df_hist <- merge(df_hist, tmpnDays, by="SumYear")
  rm(tmpnDays)

  # set all years to the selected year and shift back one year if the month is before the start month
  # in order to have continuous time series for autumn sown crops
  df_hist$Year <- SelYear
  df_scen$Year <- SelYear
  if (ShiftYears) {
    df_hist$Year <- ifelse(df_hist$Month >= StartMonth, df_hist$Year-1, df_hist$Year)
    df_scen$Year <- ifelse(df_scen$Month >= StartMonth, df_scen$Year-1, df_scen$Year)
    SelDateRange <- as.Date(paste0(as.character(SelYear-1),"-", StartMonth,"-01")):(as.Date(paste0(as.character(SelYear),"-", StartMonth,"-01"))-1)
  } else {
    SelDateRange <- as.Date(paste0(as.character(SelYear),"-", StartMonth,"-01")):(as.Date(paste0(as.character(SelYear+1),"-", StartMonth,"-01"))-1)
  }
  SelDateRange <- as.Date(SelDateRange, origin="1970-01-01")

  # select the data for the selected year
  #  df$Selected <- ifelse(df$Date %in% SelDateRange, T, F)

  # recalculate the date according to the corrected year and the day of the year
  df_hist$Date <- as.Date(paste0(as.character(df_hist$Year-1),"-12-31"))+df_hist$DOY
  df_scen$Date <- as.Date(paste0(as.character(df_scen$Year-1),"-12-31"))+df_scen$DOY
  #  df_hist[,parameter] <- NULL
  #  df <- df %>% group_by(Year) %>% mutate(nDays =  n())


  # calculate 25 and 75% percentile statistics for each julian day of the year and filter for full years
  df_LT <- df_hist %>% filter(nDays >=364) %>% group_by(Date) %>% #DN
    summarise(param = quantile(param, c(0.25, 0.75), na.rm=T), q = c(0.25, 0.75)) %>%
    pivot_wider(id_cols = Date, values_from = param, names_from = q, names_prefix = "q")

  # calculate the min, max and median for each julian day of the year of the selected parameter
  dfExtreme <- df_hist %>% group_by(Date) %>% filter(nDays >=364)%>% #DN
    summarise(max = max(param), median=median(param), min=min(param))

  # combine both data frames for parameter statistics
  df_stat <- left_join(x = df_LT, y = dfExtreme, by="Date")

  # copy back to df_LT
  df_LT <- df_stat

  # shift df extreme with min, max and median to long format for plotting with legend
  dfExtreme <- dfExtreme %>% pivot_longer(cols = c("max", "min", "median"), names_to = "stat", values_to = "value")

  # copy all statistics to long format for plotting with legend
  df_l <- df_stat %>% pivot_longer(cols = c("median", "max", "min", "q0.25","q0.75" ), names_to = "stat", values_to = "value")
  df_l <- df_l %>% mutate(lty=ifelse(stat%in% c("min","max"), 2, 1)) %>% mutate(DOY=yday(Date))

  # if smoothing is selected create smoothed data for the ribbon
  if (smoothing==T) {
    # smooth the min/max median data
    dfsmooth <- df_l %>% mutate(Datenum=as.numeric(Date)) %>%
      group_by(stat) %>%
      arrange(stat, Datenum) %>%
      nest() %>%
      mutate(pred = purrr::map(data, function(x)stats::loess(value~Datenum, span= span, data = x) %>%
                                 stats::predict(data.frame(Datenum = seq(min(x$Datenum), max(x$Datenum), 1))))) %>%
      unnest(cols = c(data, pred))

    # create df for the ribbon from smoothed data
    df_ribbon <- dfsmooth %>% filter(stat %in% c("q0.25","median", "q0.75")) %>% dplyr::select(-value) %>%
      pivot_wider(names_from = stat, values_from = pred)
  } else {
    # create df for the ribbon from unsmoothed data
    df_ribbon <- df_LT
  }

  # create custom linetype and color scales
  Myltys <- c("dotted", "solid", "dotted", "solid", "solid", "solid")
  Mycolors <- c("darkgreen", "darkblue", "darkgreen", "darkgreen", "darkgreen", "darkgreen")

  #  df_LT$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_LT$DOY

  # select the values
  # calculate the mean values for the scenario data
  # MeanValues <- df_scen %>% group_by(Date) %>% summarise(value=mean(param, na.rm=T))
  MeanValues <- df_scen %>% group_by(Date) %>% summarise(value=median(param, na.rm=T))
  # add a column for the type of data, i.e. forecast, known or scenario
  MeanValues$DataSource <- MeanValues$Date >= min(StartScenarioDate, as.Date(Sys.Date()-1))
  MeanValues$DataSource <- ifelse(MeanValues$DataSource==T, "Vorhersage", "Messung")
  MeanValues[MeanValues$Date>=min(StartScenarioDate, as.Date(Sys.Date()+7)), "DataSource"] <- "Szenario"

  # filter the scenario data for the date being later or equal to the start date of the scenario
  # calculate 25 and 75% percentile statistics for each Julian day of the year and filter for full years
  df_scen <- df_scen  %>% filter(Date >= StartScenarioDate-7)
  df_LT_scen <- df_scen  %>% group_by(Date) %>%
    summarise(param = quantile(param, c(0.25, 0.75), na.rm=T), q = c(0.25, 0.75)) %>%
    pivot_wider(id_cols = Date, values_from = param, names_from = q, names_prefix = "q")

  # calculate the min, max and median for each julian day of the year of the selected paramter
  dfExtreme_scen <- df_scen %>% group_by(Date) %>%
    summarise(max = max(param), median=median(param), min=min(param))

  # combine both data frames for parameter statistics
  df_stat_scen <- left_join(x = df_LT_scen, y = dfExtreme_scen, by="Date")
  # copy back to df_LT
  df_LT_scen <- df_stat_scen

  # shift df extreme with min, max and median to long format for plotting with legend
  dfExtreme_scen <- dfExtreme_scen %>% pivot_longer(cols = c("max", "min", "median"), names_to = "stat", values_to = "value")

  # copy all statistics to long format for plotting with legend
  df_l_scen <- df_stat_scen %>% pivot_longer(cols = c("median", "max", "min", "q0.25","q0.75" ), names_to = "stat", values_to = "value")
  df_l_scen <- df_l_scen %>% mutate(lty=ifelse(stat%in% c("min","max"), 2, 1)) %>% mutate(DOY=yday(Date))

  # if smoothing is selected create smoothed data for the ribbon
  if (smoothing==T) {

    # smooth the min/max median data
    dfsmooth_scen <- df_l_scen %>% mutate(Datenum=as.numeric(Date)) %>%
      group_by(stat) %>%
      arrange(stat, Datenum) %>%
      nest() %>%
      mutate(pred = purrr::map(data, function(x)stats::loess(value~Datenum, span= span, data = x) %>%
                                 stats::predict(data.frame(Datenum = seq(min(x$Datenum), max(x$Datenum), 1))))) %>%
      unnest(cols = c(data, pred))

    # create df for the ribbon from smoothed data
    df_ribbon_scen <- dfsmooth_scen %>% filter(stat %in% c("q0.25","median", "q0.75")) %>% dplyr::select(-value) %>%
      pivot_wider(names_from = stat, values_from = pred)
  } else {
    # create df for the ribbon from unsmoothed data
    df_ribbon_scen <- df_LT_scen
  }

  ColorStat <- c("darkgreen",   "blue", "red" )

  SelYearColours <- c("red", "blue","darkgreen")

  # check if there are any forecast data
  ForecastMeanData <- MeanValues[MeanValues$DataSource=="Vorhersage",]
  ScenarioMeanData <- MeanValues[MeanValues$DataSource=="Szenario",]
  MeanValues$DataSource <- factor(MeanValues$DataSource, levels=c("Messung", "Vorhersage", "Szenario"))
  IsAnyForecast <- any(MeanValues$DataSource=="Vorhersage")


  p <- ggplot()
  p <- p + ggtitle(plot_title)
  # p <- p + geom_ribbon(data = df_ribbon, aes(x=Date, ymax=q0.75, ymin=q0.25), fill="lightgrey", alpha=0.7)
  p <- p + geom_ribbon(data = df_ribbon, aes(x=Date, ymax=q0.75, ymin=q0.25, fill = "mittlere 50%  der letzten 20 Jahre"), alpha=0.7)
  p <- p + guides(fill=guide_legend(direction='vertical'))
  p <- p + geom_line(data=df_ribbon, aes(x=Date, y=median), color="darkgrey", size=0.8, lty=1)
  # p <- p + geom_ribbon(data = df_ribbon_scen, aes(x=Date, ymax=q0.75, ymin=q0.25), fill="pink", alpha=0.7)
  p <- p + geom_ribbon(data = df_ribbon_scen, aes(x=Date, ymax=q0.75, ymin=q0.25, fill="mittlere 50%  der Wetterszenarien im aktuellen Jahr"), alpha=0.7)
  # p <- p + guides(fill=guide_legend(direction='vertical'))
  # p <- p + geom_line(data=dfExtreme_scen, aes(x=Date, y=value ), color="darkgrey" , size=0.8, lty="dotted")
  p <- p + geom_line(data = MeanValues, aes(x=Date, y=value),color="darkgreen" , size=1, lty=1)
  if (IsAnyForecast) {
    p <- p + geom_line(data = ForecastMeanData, aes(x=Date, y=value),color="red" , size=1, lty=1)
  }
  p <- p + geom_line(data = ScenarioMeanData, aes(x=Date, y=value),color="blue" , size=1, lty=1)
  p <- p + guides(color=guide_legend(direction='vertical'))
  p <- p + scale_x_date(labels = label_date("%b"), date_breaks = "1 month")
  p <- p + scale_color_manual(name="Wetterdaten im akt. Jahr", values = ColorStat)
  p <- p + scale_fill_manual(name="",values = c( "mittlere 50%  der letzten 20 Jahre" ="lightgrey","mittlere 50%  der Wetterszenarien im aktuellen Jahr"="pink"))
  #  p <- p + new_scale_color()
  #  p <- p + geom_line(data=df_ribbon_scen, aes(x=Date, y=median), color="darkgreen", size=0.8, lty=1)
  #  p <- p + geom_line(data=dfExtreme_scen, aes(x=Date, y=value, color=stat) , size=0.8, lty="dotted")
  #  p <- p + scale_color_manual(values = SelYearColours)
  p <- p + ylab(y_label) + xlab("Monat")
  p <- p + theme_bw(base_size = BaseSize)
  # p <- p + theme(legend.position="bottom", legend.box= "vertical", legend.margin = margin(), legend.text=element_text(size=14))
  if (plotly == F) {
    p <- p + theme(legend.position="bottom",
                   legend.box="vertical",
                   legend.box.just = "left",
                   legend.margin=margin(),
                   legend.text=element_text(size=12),
                   legend.title=element_text(size=12)) }
  if (plotly==T) {
    p <- p + guides(color="none", fill="none")
    p <- ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.4, y =-0.2))
    #    p <- p %>% add_trace()
    # %>% layout(autosize=FALSE, width=800, height=600)
    p$x$data <- p$x$data %>%
      map(clean_plotly_leg, "[^\\(][^,]*") # ie remove the opening bracket,

  }
  return(p)
}

Calc_Mean <- function(df,
                      parameter,
                      ShiftYears=F,
                      SelYear=0,
                      StartMonth=9,
                      StartScenarioDate=NULL){

  if (is.null(df)) {
    stop("No data frame")
  }
  if (is.null(df$Time)){
    stop("No Time column in data frame")}
  # add date, month, day of the year and year to the data frame
  df$Date <- as.Date(df$Time, origin = "1899-12-30")
  df$Month <- month(df$Date)
  df$DOY <- yday(df$Date)
  df$Year <- year(df$Date)
  # add parameter "SumYear", a time shifted cultivation year, starting in the autumn
  # before the following harvest year
  # for calculations of year summation parameters other than the calendar year
  if (ShiftYears) {
    df$SumYear <- ifelse(df$Month >= StartMonth, df$Year+1, df$Year)
  }
  else {
    df$SumYear <- df$Year
  }
  # format as numeric and use actual year if nothing is selected
  SelYear <- as.numeric(SelYear)
  if (SelYear==0) {
    ThisYear <- substr(as.character(Sys.Date()), 1,4)
    SelYear <- ThisYear
  }
  SelYear <- as.numeric(SelYear)
  # select and check columns
  selcols <- c("Date", "Month", "DOY","Year", "SumYear", parameter)
  stopifnot(parameter %in% names(df))
  # select the columns
  df <- as.data.frame(df)
  df <- df[,selcols]
  # copy the parameter to a new column
  # reanme the parameter column to param for the ggplot function
  df$param <- df[,parameter]
  # same for the scenario data
  # calculate the number of days for each year, to filter for full years
  # especially for summed up parameters
  tmpnDays <- aggregate(DOY ~ SumYear, data = df, FUN = length)
  tmpnDays$nDays <- tmpnDays$DOY
  tmpnDays$DOY <- NULL
  df <- merge(df, tmpnDays, by="SumYear")
  rm(tmpnDays)

  # set all years to the selected year and shift back one year if the month is before the start month
  # in order to have continuous time series for autumn sown crops
  df$Year <- SelYear
  if (ShiftYears) {
    df$Year <- ifelse(df$Month >= StartMonth, df$Year-1, df$Year)
    SelDateRange <- as.Date(paste0(as.character(SelYear-1),"-", StartMonth,"-01")):(as.Date(paste0(as.character(SelYear),"-", StartMonth,"-01"))-1)
  } else {
    SelDateRange <- as.Date(paste0(as.character(SelYear),"-", StartMonth,"-01")):(as.Date(paste0(as.character(SelYear+1),"-", StartMonth,"-01"))-1)
  }
  SelDateRange <- as.Date(SelDateRange, origin="1970-01-01")

  # select the data for the selected year
  #  df$Selected <- ifelse(df$Date %in% SelDateRange, T, F)

  # recalculate the date according to the corrected year and the day of the year
  df$Date <- as.Date(paste0(as.character(df$Year-1),"-12-31"))+df$DOY
  # select the values
  # filter the scenario data for the date being later or equal to the start date of the scenario
  # calculate the min, max and median for each julian day of the year of the selected paramter
  dfExtreme <- df%>% group_by(Date) %>%
    summarise(max = max(param), mean=mean(param, na.rm=T), median=median(param), min=min(param))
  return(dfExtreme)
}


#' makeplot2 a function to plot scenarios
#'
#' @param dfhist data frame with historic data
#' @param dfscen data frame with scen data
#' @param parameter Column to be plotted
#' @param BaseSize Base char size for the plot
#' @param ylabel The y label string
#' @param SelYear The year to be highlighted
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#'
#' @return A ggplot object
#' @export
#'
# @examples makeplot2(dfhist, dfscen, "TMPM", BaseSize=18, ylabel="Temperature", SelYear=2020)
makeplot2 <- function(dfhist, dfscen, parameter, BaseSize=18, ylabel="", SelYear=0){
  # select columns
  selcols <- c("Date", "DOY","Year", parameter)
  # check if parameter is in the data frame
  stopifnot(parameter %in% names(dfhist))
  # select columns
  dfhist <- dfhist[,selcols]
  # convert to data frame
  dfhist <- as.data.frame(dfhist)
  # create a new column with the parameter values
  dfhist$param <- dfhist[,parameter]
  dfhist[,parameter] <- NULL
  # calculate statistics for the historic data
  df_LT <- dfhist %>% group_by(DOY) %>%
    summarise(param = quantile(param, c(0.25, 0.5, 0.75), na.rm=T), q = c(0.25, 0.5, 0.75)) %>%
    pivot_wider(id_cols = DOY, values_from = param, names_from = q, names_prefix = "q")

  # create a new data frame with extreme values
  dfExtreme <- dfhist %>% group_by(DOY) %>%
    summarise(max = max(param), min=min(param), mean=mean(param, na.rm=T))

  # join the extreme values to the statistics
  df_LT <- left_join(x = df_LT, y = dfExtreme, by="DOY")
  rm(dfExtreme)
  # create a new column with the date
  df_LT$Date <- as.Date("2022-12-31")+df_LT$DOY
  #
  df_LT$Date <- ifelse(df_LT$Date > as.Date("2023-08-31"),
                       as.Date(paste(as.numeric(format(df_LT$Date,"%Y"))-1,
                       format(df_LT$Date,"%m"),format(df_LT$Date,"%d"),sep = "-")),
                       df_LT$Date)
  df_LT$Date <- as.Date(df_LT$Date, origin="1970-01-01")
  df_LT <- df_LT %>% arrange(Date)

  ###
  dfscen <- dfscen[,selcols]
  dfscen <- as.data.frame(dfscen)
  dfscen$param <- dfscen[,parameter]
  dfscen[,parameter] <- NULL
  df_LTscen <- dfscen %>% group_by(DOY) %>%
    summarise(param = quantile(param, c(0.25, 0.5, 0.75), na.rm=T), q = c(0.25, 0.5, 0.75)) %>%
    pivot_wider(id_cols = DOY, values_from = param, names_from = q, names_prefix = "q")
  dfExtremeScen <- dfscen %>% group_by(DOY) %>%
    summarise(max = max(param), min=min(param), mean=mean(param, na.rm=T))

  df_LTscen <- left_join(x = df_LTscen, y = dfExtremeScen, by="DOY")
  rm(dfExtremeScen)
  df_LTscen$Date <- as.Date("2022-12-31")+df_LTscen$DOY
  df_LTscen$Date <- ifelse(df_LTscen$Date > as.Date("2023-08-31"),
                       as.Date(paste(as.numeric(format(df_LTscen$Date,"%Y"))-1,
                                     format(df_LTscen$Date,"%m"),format(df_LTscen$Date,"%d"),sep = "-")),
                       #                           df_LT$Date-365,
                       df_LTscen$Date)




  df_LTscen$Date <- as.Date(df_LTscen$Date, origin="1970-01-01")
  df_LTscen <- df_LTscen %>% arrange(Date)



  ###
  y_label <- ifelse(ylabel=="", parameter, ylabel)

  if (SelYear==0) {
    ThisYear <- substr(as.character(Sys.Date()), 1,4)
    SelYear <- ThisYear
  }
#  df_LT$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_LT$DOY
#  df_LTscen$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_LTscen$DOY

#  SelYearValues <- dfhist[dfhist$Year==SelYear, c( "Date","DOY", "param")]
#  SelYearValues <- as.data.frame(SelYearValues)
#  dfscenario <- dfscen
#  dfscenario <- dfscenario[,c( "Date","DOY", parameter)]


  p <- ggplot()
  p <- p + geom_ribbon(data = df_LT, mapping = aes(x=Date, ymax=q0.75, ymin=q0.25), fill="green", alpha=0.3)
  p <- p + geom_line(data=df_LT, aes(x=Date, y=max), color="green", size=0.5, lty=2)
  p <- p + geom_line(data=df_LT, aes(x=Date, y=min), color="green", size=0.5, lty=2)
#  p <- p + geom_line(data=df_LT, aes(x=Date, y=q0.5), color="darkgreen", size=0.8, lty=1)
  p <- p + geom_line(data=df_LT, aes(x=Date, y=mean), color="darkgreen", size=0.8, lty=1)
  #  p <- p + geom_line(data=dfscenario, aes(x=Date, y=parameter), color="red", size=1)
  p <- p + geom_ribbon(data = df_LTscen, mapping = aes(x=Date, ymax=q0.75, ymin=q0.25), fill="red", alpha=0.4)
#  p <- p + geom_line(data=df_LTscen, aes(x=Date, y=max), color="red", size=0.5, lty=1)
#  p <- p + geom_line(data=df_LTscen, aes(x=Date, y=min), color="red", size=0.5, lty=1)
#  p <- p + geom_line(data=df_LTscen, aes(x=Date, y=q0.5), color="red", size=1, lty=1)
  p <- p + geom_line(data=df_LTscen, aes(x=Date, y=mean), color="red", size=1, lty=1)
  p <- p + scale_x_date(labels = label_date("%b"), date_breaks = "1 month")
  p <- p + ylab(y_label)
  p <- p + theme_bw(base_size = BaseSize)
  p
  return(p)
}



#' makeplot3 a function to plot scenarios with smoothed variation ranges
#'
#' @param dfweather data frame with weather data
#' @param parameter Column to be plotted
#' @param BaseSize Base char size for the plot
#' @param ylabel The y label string
#' @param xlabel The x label string
#' @param SelYear The year to be highlighted
#' @param IsLegend Add a legend to the plot?
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#'
#' @return A ggplot object
#' @export
#'
# @examples makeplot3(dfweather, "TMPM", BaseSize=18, ylabel="Temperature", xlabel="Datum", SelYear=2020, IsLegend=TRUE)
makeplot3 <- function(dfweather, parameter, BaseSize=18, ylabel="", xlabel="Datum", SelYear=0, IsLegend=TRUE){

  y_label <- ifelse(ylabel=="", parameter, ylabel)

  tmp <- df_names %>% filter(names==parameter) %>% dplyr::select(ylabels)
  if (nrow(tmp)>0) {
    y_label <- tmp$ylabels
  }

  tmp <- df_names %>% filter(names==parameter) %>% dplyr::select(longNames)
  if (nrow(tmp)>0) {
    title <- tmp$longNames
  }


  selcols <- c("Date", "DOY","Year", parameter)
  stopifnot(parameter %in% names(dfweather))
  dfweather <- dfweather[,selcols]
  dfweather <- as.data.frame(dfweather)
  dfweather$param <- dfweather[,parameter]
  dfweather[,parameter] <- NULL
  df_LT <- dfweather %>% group_by(DOY) %>%
    summarise(param = quantile(param, c(0.25, 0.75), na.rm=T), q = c(0.25, 0.75)) %>%
    pivot_wider(id_cols = DOY, values_from = param, names_from = q, names_prefix = "q")
  dfExtreme <- dfweather %>% group_by(DOY) %>%
    summarise(median=median(param), max = max(param), min=min(param))

  df_stat <- left_join(x = df_LT, y = dfExtreme, by="DOY")
  dfExtreme <- dfExtreme %>% dplyr::select(-median) %>% pivot_longer(cols = c("max", "min"), names_to = "stat", values_to = "value")
  df_l <- df_stat %>% pivot_longer(cols = c("median", "max", "min", "q0.25","q0.75" ), names_to = "stat", values_to = "value")
  df_l <- df_l %>% mutate(lty=ifelse(stat%in% c("min","max"), 2, 1))

  SelYear <- as.numeric(SelYear)
  if (SelYear==0) {
    ThisYear <- substr(as.character(Sys.Date()), 1,4)
    SelYear <- ThisYear
  }
  SelYear <- as.numeric(SelYear)
  df_LT$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_LT$DOY
  df_l$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_l$DOY
  dfExtreme$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+dfExtreme$DOY

  dfsmooth <- df_l %>%
    group_by(stat) %>%
    arrange(stat, DOY) %>%
    nest() %>%
    mutate(pred = purrr::map(data, function(x)stats::loess(value~DOY, span= 0.5, data = x) %>%
                               stats::predict(data.frame(DOY = seq(min(x$DOY), max(x$DOY), 1))))) %>%
    unnest(cols = c(data, pred))

  df_ribbon <- dfsmooth %>% filter(stat %in% c("q0.25", "q0.75")) %>% dplyr::select(-value) %>%
    pivot_wider(names_from = stat, values_from = pred)


  Myltys <- c("dotted", "solid", "dotted", "solid", "solid", "solid")
  Mycolors <- c("darkgreen", "darkblue", "darkgreen", "darkgreen", "darkgreen", "darkgreen")

  # SelYearValues <- dfweather[dfweather$Year==SelYear, c( "Date","DOY", "param")]
  SelYearValues <-  dfweather %>% filter(Year==SelYear) %>% dplyr::select(Date, DOY, param)
  SelYearValues <- as.data.frame(SelYearValues)
  p <- ggplot()
  p <- p + ggtitle(title)
  p <- p + geom_ribbon(data = df_ribbon, mapping = aes(x=Date, ymax=q0.75, ymin=q0.25), fill="green", alpha=0.3)
  p <- p + geom_line(data=dfsmooth, aes(x=Date, y=pred, color=stat, lty=stat), size=0.5)
  p <- p + geom_line(data=dfExtreme, aes(x=Date, y=value, color=stat) , size=0.8, lty="dotted")
  #  p <- p + geom_smooth(data=dfExtreme_l, aes(x=Date, y=value, lty = stat), color="red", size=1.2, se = F,
  #                       method = "loess", span = 0.5)
  p <- p + geom_line(data=SelYearValues, aes(x=Date, y=param), color="red", size=1)
  p <- p + scale_x_date(labels = label_date("%b"), date_breaks = "1 month")
  p <- p + ylab(y_label) + xlab(xlabel)
  #  p <- p + scale_color_identity(guide = "legend")
  p <- p + scale_linetype_manual(values = Myltys)
  p <- p + scale_color_manual(values = Mycolors)
  p <- p + theme_bw(base_size = BaseSize)
  p
  return(p)
}




# Radiation functions ----------------------------------------------


#' calcsolar: calculation of daily of maximum solar radiation
#'
#' @param dayofyear Julian day of the year.
#' @param latitude Latitude in °.
#'
#' @return a list of 3 values/arrays angot.value = Daily maximum global radiation in [MJ.m-2.d-1], declination of sun in [rad], daylength in [h]
#' @export
#'
# @examples calcsolar(200, 54)
calcsolar <- function (dayofyear, latitude)
  {
    if (!is.numeric(dayofyear) || !is.numeric(latitude)) {
      stop("Invalid input. 'dayofyear' and 'latitude' should be numeric values.")
    }
    if (min(dayofyear) < 1 | max(dayofyear) > 366) {
      stop("Invalid input. 'dayofyear' should be a value between 1 and 366.")
    }
    if (min(latitude) < -90 | max(latitude) > 90) {
      stop("Invalid input. 'latitude' should be a value between -90 and 90.")
    }
  rad <- pi/180
  dec <- -1 * asin(sin(23.45 * rad) * cos(2 * pi * (dayofyear +
                                                      10)/365))
  sinld <- sin(latitude * rad) * sin(dec)
  cosld <- cos(latitude * rad) * cos(dec)
  aob <- sinld/cosld
  dayl <- 12 * (1 + 2 * asin(aob)/pi)
  dsinb <- 3600 * (dayl * sinld + 24 * cosld * sqrt(1 - aob *
                                                      aob)/pi)
  sc <- 1370 * (1 + 0.033 * cos(2 * pi * dayofyear/365))
  angot <- sc * dsinb/1e+06
  return(list(declination = dec, daylength = dayl, angot.value = angot))
}



#'  estSg_S0: calculation of daily solar radiation from relative sunshine
#'
#' @param RelSun Relative sunshine duration of a particular day of year
#' @param Month Month of the year
#'
#' @return solar radiation per day [MJ/m2/d]
#' @details function for calculation of daily solar radiation from relative sunshine hours ####
#' the function uses an empirical model calibrated from DWD data "mod.Angstroem"
#' @examples estSg_S0(0.8, 6)
estSg_S0 <- function (RelSun, Month)
{
  if (is.numeric(Month)){Month <- as.integer(Month)}
  stopifnot(is.integer(Month))
  Month <- factor(Month, levels = as.character(1:12))
  mdf <- data.frame(RelSun = RelSun, Monat = Month)
  value <- predict.lm(object = mod.Angstroem, newdata = mdf)
  return(value)
}



#' RelRad: calculation of daily solar radiation from relative sunshine
#' the model has been fitted with observations from the DWD (relative sunshine and global radiation)
#' @param RelSun Relative sunshine duration of a particular day of year
#' @param Month Month of the year
#'
#' @return Relative Global radiation [0..1]
#' @export
#' @details New function for calculation of daily solar radiation from relative sunshine hours ####
#' the function uses directly the parameters  from  "mod.Angstroem"
# @examples RelRad(0.8, 6)
RelRad <- function (RelSun=0.8, Month=6){


  # convert to integer
  Month <- as.integer(Month)

  # RelSun out of range transformed to NA
  # RelSun <- ifelse(RelSun < 0 | RelSun > 1, NA, RelSun)
  RelSun <- pmax(0, pmin(1, RelSun))

  Month <- as.integer(Month)

  sqrtRelSun  <- sqrt(RelSun)
  # value of the intercept
  pIntercept  <- 0.161421912
  # linear effect for RelSun
  pRelSun     <- 0.107396393
  # quadratic effect for RelSun
  pSqrtRelSun <- 0.393292919
  # parameters for linear month effects
  pMonth      <- c(0, 0.007720969,0.001199219, -0.004020425, -0.012095101, -0.004887682, -0.009369806, -0.013787157,
               -0.009357840, -0.009234819, -0.011933316, -0.011964565)
  # parameters for interaction effect of RelSun and month
  pMonthRelSun <- c(0, 0.055166063, 0.119134736, 0.135627337, 0.113969224, 0.149730235, 0.136067936,
                    0.102568569, 0.093243449, 0.081928205, 0.013257431, -0.059281871)

  # parameters for interaction effect of sqrtRelSun and month
  pMonthSqrtRelSun <- c(0, -0.018514091, -0.043937525, -0.051414540, -0.024727021, -0.067051093,
                        -0.061308953, -0.034943014, -0.026797192, -0.026647466, 0.008849817,
                        0.042075264)

  value <- pIntercept+pMonth[Month]+ # intercept + month shifts
    RelSun*(pRelSun+pMonthRelSun[Month])+ # Interaction effect of RelSun
    sqrtRelSun*(pSqrtRelSun+pMonthSqrtRelSun[Month]) # Interaction effect of sqrtRelSun
  return(value)
}



# Wind speed correction ---------------------------------------------------


#' transWindSpeed
#' corrects the wind speed for the required height considering
#' the measurement height
#'
#' @param inWind original wind speed data [m/s]
#' @param inHeight measurement height of original wind speed data [m]
#' @param outHeight reference height to be wind speed should be converted [m]
#' @param vegi.height vegetation height [m]
#'
#' @return corrected wind speed data [m/s]
#' @export
#'
# @examples transWindSpeed(5, 10, 2, 0.5)
transWindSpeed<-function(inWind,inHeight,outHeight,vegi.height)
{

  # ra_f = (ln((inHeight-displHeight) / z0m) *
  #ln((inHeight-displHeight)/ z0h)/(sqr(Karman_const) * inWind)


  # ra_f = (ln((outHeight-displHeight) / z0m) *
  #ln((outHeight-displHeight)/ z0h)/(sqr(Karman_const) * outWind)

  Karman_const = 0.41
  # displHeight = 0.0804
  displHeight = 0.67*vegi.height
  # z0m = 0.01476
  z0m=0.123*vegi.height
  #Grasbestand
  z0h =0.1*z0m

  ## outWind/inWind = ra_f(outWind,outHeight) / ra_f(inWind, inHeight) =>
  #outWind = inWind*ra_f(outWind,outHeight) / ra_f(inWind, inHeight)

  outWind =
    inWind*(log((outHeight-displHeight)/z0m)*log((outHeight-displHeight)/z0h))/(log((inHeight-displHeight)/z0m)*log((inHeight-displHeight)/z0h))
  outWind

}



#' Alternative function for correction of windspeed to reference height
#'
#' @param ui original windspeed [m/s]]
#' @param zi measurement height [m]
#' @param zo reference height [m]
#'
#' @return uo corrected windspeed [m/s]
#' @export
#'
# @examples windheight(5, 10, 2)
windheight <- function(ui, zi, zo) {
  if (zo < 0.2 & zo > (5.42 / 67.8)) {
    warning("Wind-height profile function performs poorly when wind
            height is below 20 cm")
  }
  if (zo <= (5.42 / 67.8)) {
    warning(paste("wind-height profile cannot be calculated for height ",
                  zo * 100, " cm"))
    print("Height converted to 10 cm")
    zo <- 0.1
  }
  uo <- ui * log(67.8 * zo - 5.42) / log(67.8 * zi - 5.42)
  return(uo)
}






#  Handling of DWD files --------------------------------------------------

## Renaming of DWD files --------------------------------------------------




#' Rename columns of DWD weather data
#'
#' @param DWDWeather Data frame with original DWD weather data
#' @import dplyr
#'
#' @returns Data frame with readable column names
#' @export
#'
RenameDWDWeather <- function(DWDWeather){

  # stopifnot(names(DWDWeather)%in%c("Stations_id", "MESS_DATUM", "FX", "FM",  "QN_3", "WINDGESCHWINDIGKEIT",
  #                                "QN_4", "RSK", "RSKF", "SDK", "SHK_TAG", "NM", "VPM", "PM", "TMK", "UPM", "TXK",
  #                                "TNK", "TGK", "eor",  "MHoeheWind", "Date", "Stationshoehe", "geoBreite", "geoLaenge", "Stationsname"))
  # rename columns


  rename_vector <- setNames(df_names_DWD$names_DWD, df_names_DWD$longNames_DWD)
  DWDWeather <- DWDWeather %>% dplyr::rename(!!!rename_vector)
  return(DWDWeather)
}

#' abr.names shorten long column names
#'
#' @param tab Data frame for which columns should be renamed
#'
#' @return Data frame with shortened column names
#' @export
#'
# @examples abr.names(tab)
abr.names <- function(tab){
  names(tab)[names(tab)=="LUFTTEMPERATUR"] <- "TMPM"
  names(tab)[names(tab)=="LUFTDRUCK_STATIONSHOEHE"] <- "LD"
  names(tab)[names(tab)=="REL_FEUCHTE"] <- "LF"
  names(tab)[names(tab)=="WINDGESCHWINDIGKEIT"] <- "Wind"
  names(tab)[names(tab)=="LUFTTEMPERATUR_MAXIMUM"] <- "TMPMX"
  names(tab)[names(tab)=="LUFTTEMPERATUR_MINIMUM"] <- "TMPMN"
  names(tab)[names(tab)=="NIEDERSCHLAGSHOEHE"] <- "Rain"
  names(tab)[names(tab)=="SONNENSCHEINDAUER"] <- "SunDur"
  return(tab)
}



#' RenameDWDRain renames the columns of the DWD rain data
#'
#' @param DWDRain data frame with original DWD rain data
#'
#' @return data frame with readable column names
#' @export
#'
# @examples RenameDWDRain(DWDRain)
RenameDWDRain <- function(DWDRain){
 # stopifnot(names(DWDRain)%in%c("Stations_id", "MESS_DATUM","QN_6", "RS", "RSF","SH_TAG","NSH_TAG","eor",
 #                              "Date", "Stationshoehe", "geoBreite", "geoLaenge", "Stationsname"))
  # Umbenennen nach altem Schema
  names(DWDRain)[names(DWDRain)=="QN_6"]    <- "QUALITAETS_NIVEAU"
  names(DWDRain)[names(DWDRain)=="RS"]      <- "NIEDERSCHLAGSHOEHE"
  names(DWDRain)[names(DWDRain)=="NSH_TAG"] <- "NEUSCHNEEHOHE"
  names(DWDRain)[names(DWDRain)=="SH_TAG"]  <- "SCHNEEHOHE"
  return(DWDRain)
}



## Get DWD metadata --------------------------------------------------



#' getDWDContent gets the content (filenames etc.) of a DWD ftp repository
#'
#' @param repository ftp address of repository with DWD weather data
#' @param quiet Give echo or not
#' @import RCurl
#' @import utils
#' @return List with 1 data frame and one array of ZIPIDs
#' stationlist = data frame with the columns: "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe" "geoBreite"     "geoLaenge"
#' "Stationsname"  "Bundesland"
#' filelist: all filenames of the not zipped files repository
#' ziplist: names of the zip files in the repository
#' zipID: 5 digit ZipIDs
#' @export
#'
# @examples getDWDContent("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/")
getDWDContent <- function(repository, quiet=T, LoadMetaData=F){

fn_KLBeschreibung <- "KL_Tageswerte_Beschreibung_Stationen.txt"
fn_Metadaten <- "Metadaten_Parameter_klima_tag_"

  # get the content/filenames of the DWD ftp repository
  filelist <- sort(stri_split_lines1(getURL(repository, dirlistonly = TRUE)))
  # select only the zip files
  ziplist <- grep(".zip", filelist, value=TRUE)
  # extract the 5 digit zipIDs
  zipID <- gsub("tageswerte_KL_", "", ziplist)
  zipID <- stri_split_fixed(zipID, "_", n = 1, tokens_only=TRUE, simplify=TRUE)[,1]


  dir.create(path_unzip, showWarnings = FALSE, recursive = T)

  dest_file <- paste0(path_unzip, "/Stationlist.txt")
  # Hier wird es kompliziert, da der DWD ständig das File-Format ändert...
  download.file(url=paste0(repository, fn_KLBeschreibung),
                #		destfile="/DWD_tmp/Stationlist.txt", mode = "wb")
                destfile=dest_file, mode = "wb", quiet = quiet, method = "auto")

#  tmp <- stri_encode(stri_read_raw(dest_file),
#                     from="latin1", to="UTF-8")
#  tmp <- readr::read_delim(file = dest_file, delim = " ", col_names = TRUE, quote = "", escape_double = FALSE)
  tmp <- read.fwf(file = dest_file, widths = c(5, -1, 8, -1, 8,-1, 14,-3, 7, -5, 7, -1, 40,-1, 40, -1, 4), skip=2,
           col.names = c("Stations_id", "von_datum", "bis_datum", "Stationshoehe", "geoBreite", "geoLaenge", "Stationsname", "Bundesland", "Abgabe"),
           fileEncoding = "latin1", stringsAsFactors = FALSE, na.strings = "-999")

  tmp$Stationsname <- trimws(tmp$Stationsname, which = c("both"))
  tmp$Bundesland <- trimws(tmp$Bundesland, which = c("both"))

  stationlist <- tmp

  if(LoadMetaData) {
  stationlist$Parameters <- ""
  for (i in 1:length(stationlist$Stations_id)){

    id <- stationlist[i, "Stations_id"]
    ndx <- as.integer(stationlist[i, "Stations_id"])
    fn <- ziplist[i]

    if(fn %in% ziplist){


    if (dir.exists(paste0(path_unzip))==F){
      dir.create(path_unzip, recursive = T)
    }

    # download the zip file
    dest_file <- paste(path_unzip, fn, sep="/")
    download.file(url=paste0(repository, fn),
                  destfile=dest_file, mode = "wb", quiet = quiet, method = "auto")

    # unzip the file
    if (file.exists(dest_file)){
      unzip(zipfile = dest_file, exdir = path_unzip)
    # get the name of the unzipped file with the weather data
      fn_Meta <- paste(DataDir, path_unzip, paste0(fn_Metadaten, id, ".txt"))
      if(file.exists(fn_Meta)){

  # read weather data from temporary file
      Metadata <- read.table(file = fn_Meta,
                            header=TRUE, sep=";", quote="", dec=".", na.strings=c("-999", "\032"),
                           stringsAsFactors=FALSE, strip.white=TRUE, fill=TRUE, fileEncoding="latin1")
    # remove the last two rows
      Metadata <- Metadata %>% slice(1:(n()-2))
      stationlist[i, "Parameters"] <- paste0(unique(Metadata$Parameter), collapse = ", ")
      }

      }
    }
  }
  # remove temporary files
  if(file.exists(path_unzip)){
    unlink(path_unzip, recursive = TRUE, force = TRUE)
  }

  }

  # some type conversion
  stationlist$Stationshoehe <- type.convert(stationlist$Stationshoehe, dec = ".", as.is="T")
  stationlist$geoBreite <- type.convert(stationlist$geoBreite, dec = ".", as.is="T")
  stationlist$geoLaenge <- type.convert(stationlist$geoLaenge, dec = ".", as.is="T")

  # Kodierung der ID mal mit und mal ohne führende Nullen. Das sollte der Schritt einheitlich machen...
  stationlist$Stations_id <- sprintf("%05i", type.convert(stationlist$Stations_id, dec = ".", as.is="T"))
  stationlist <- stationlist %>% filter(Stations_id %in% zipID)%>%
    distinct(Stations_id, .keep_all = T)
  # add columns for the start and end year of the data
  stationlist$bisJahr <- as.numeric(substr(x =stationlist$bis_datum, 1,4))
  stationlist$vonJahr <- as.numeric(substr(x =stationlist$von_datum, 1,4))


  # return the list of stationlist, filelist, ziplist and zipID
  return(list(stationlist= stationlist,
              filelist = filelist,
              ziplist = ziplist,
              zipID = zipID))
}



#' getDWDStationList gets a station list from the DWD ftp repository
#' without ziplists and addtionally returns historical and recent stationlist (metadat)
#' as well as filelists, ziplists and zipIDs
#'
#' @param historical ftp address of historical weather data
#' @param recent ftp address of recent weather data
#'
#' @return list of stations, recent, historical objects
#' stations: merged data frame of all weather stations with columns
#' "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe"
#' "geoBreite"     "geoLaenge" "Stationsname"  "Bundesland"
#' recent and historical: lists with
#' stationlist = data frame with the columns: "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe" "geoBreite"     "geoLaenge"
#' "Stationsname"  "Bundesland"
#' filelist: all filenames of the not zipped files repository
#' ziplist: names of the zip files in the repository
#' zipID: 5 digit ZipIDs
#' @export
#' @import dplyr
#' @import stringi
#'
# @examples getDWDStationList("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/",
getDWDStationList <- function(historical, recent){
  historical <- getDWDContent(historical)
  recent <- getDWDContent(recent)
  #  stopifnot(all.equal(historical$stationlist, recent$stationlist))

  stations <- rbind(historical$stationlist, recent$stationlist)
  zipID.sum <- unique(c(historical$zipID, recent$zipID))
  #  zipID.sum <- zipID.sum[zipID.sum %in% historical$stationlist$Stations_id]
  tmp  <- stations %>% group_by(Stations_id) %>%
    summarise(von_datum=min(as.integer(von_datum)),
              bis_datum=max(as.integer(bis_datum))) %>%
    mutate(von_datum = as.character(von_datum),
           bis_datum = as.character(bis_datum))
  stations <- stations %>% dplyr::select(-von_datum, -bis_datum) %>%
    distinct()
  stations <- merge(x = tmp, y = stations, by="Stations_id")
  return(list(stations=stations, historical=historical, recent=recent))
}


#' getDWDRainContent gets the content of the DWD ftp repository for additional rain data
#'
#' @param repository ftp address of DWD repository for additional rain data
#' @param quiet option to echo off
#' @import stringi
#' @import utils
#'
#' @return List with 1 data frame and one array of ZIPIDs
#' stationlist = data frame with the columns: "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe" "geoBreite"     "geoLaenge"
#' "Stationsname"  "Bundesland"
#' filelist: all filenames of the not zipped files repository
#' ziplist: names of the zip files in the repository
#' zipID: 5 digit ZipIDs as characer vector
#' @export
#'
# @examples getDWDRainContent("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/")
getDWDRainContent <- function(repository, quiet=T){
  filelist <- sort(stri_split_lines1(getURL(repository, dirlistonly = TRUE)))
  ziplist <- grep(".zip", filelist, value=TRUE)
  zipID <- gsub("tageswerte_RR_", "", ziplist)
  zipID <- stri_split_fixed(zipID, "_", n = 1, tokens_only=TRUE, simplify=TRUE)[,1]

  dir.create(path_unzip, showWarnings = FALSE, recursive = T)

  dest_file <- paste(path_unzip,"RainStationlist.txt", sep="/")
  # Hier wird es kompliziert, da der DWD ständig das File-Format ändert...
  download.file(url=paste0(repository, "RR_Tageswerte_Beschreibung_Stationen.txt"),
                #		destfile="/DWD_tmp/Stationlist.txt", mode = "wb")
                destfile=dest_file, mode = "wb", quiet = quiet, method = "auto")


  tmp <- read.fwf(file = dest_file, widths = c(5, -1, 8, -1, 8,-1, 14,-3, 7, -5, 7, -1, 40,-1, 40, -1, 4), skip=2,
                  col.names = c("Stations_id", "von_datum", "bis_datum", "Stationshoehe", "geoBreite", "geoLaenge", "Stationsname", "Bundesland", "Abgabe"),
                  fileEncoding = "latin1", stringsAsFactors = FALSE, na.strings = "-999")

  tmp$Stationsname <- trimws(tmp$Stationsname, which = c("both"))
  tmp$Bundesland <- trimws(tmp$Bundesland, which = c("both"))


  stationlist <- tmp
  stationlist$Stationshoehe <- type.convert(stationlist$Stationshoehe, dec = ".", as.is="T")
  stationlist$geoBreite <- type.convert(stationlist$geoBreite, dec = ".", as.is="T")
  stationlist$geoLaenge <- type.convert(stationlist$geoLaenge, dec = ".", as.is="T")
  # add columns for the start and end year of the data
  stationlist$bisJahr <- as.numeric(substr(x =stationlist$bis_datum, 1,4))
  stationlist$vonJahr <- as.numeric(substr(x =stationlist$von_datum, 1,4))

  # Kodierung der ID mal mit und mal ohne führende Nullen. Das sollte der Schritt einheitlich machen...
  stationlist$Stations_id <- sprintf("%05i", type.convert(stationlist$Stations_id, dec = ".", as.is="T"))
  stationlist <- stationlist %>% filter(Stations_id %in% zipID)
  # remove temporary files
  if(file.exists(path_unzip)){
    unlink(path_unzip, recursive = TRUE, force = TRUE)
  }

  return(list(stationlist= stationlist,
              filelist=filelist, ziplist=ziplist, zipID=zipID))
}




#' getDWDRainStationList gets a station list from the DWD ftp repository
#' without ziplists and addtionally returns historical and recent stationlist (metadata)
#' as well as filelists, ziplists and zipIDs
#'
#' @param historical ftp address of historical rain data
#' @param recent ftp address of recent rain data
#' @import dplyr
#'
#' @return list of stations, recent, historical objects
#' stations: merged data frame of all weather stations with columns
#' "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe"
#' "geoBreite"     "geoLaenge" "Stationsname"  "Bundesland"
#' recent and historical: lists with
#' stationlist = data frame with the columns: "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe" "geoBreite"     "geoLaenge"
#' "Stationsname"  "Bundesland"
#' filelist: all filenames of the not zipped files repository
#' ziplist: names of the zip files in the repository
#' zipID: 5 digit ZipIDs
#' @export
#'
# @examples getDWDRainStationList("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/",
getDWDRainStationList <- function(historical, recent){
  historical <- getDWDRainContent(historical)
  recent <- getDWDRainContent(recent)
  #  stopifnot(all.equal(historical$stationlist, recent$stationlist))

  stations <- rbind(historical$stationlist, recent$stationlist)
  zipID.sum <- unique(c(historical$zipID, recent$zipID))
  #  zipID.sum <- zipID.sum[zipID.sum %in% historical$stationlist$Stations_id]
  tmp  <- stations %>% group_by(Stations_id) %>%
    summarise(von_datum=min(as.integer(von_datum)),
              bis_datum=max(as.integer(bis_datum))) %>%
    mutate(von_datum = as.character(von_datum),
           bis_datum = as.character(bis_datum))
  stations <- stations %>% dplyr::select(-von_datum, -bis_datum) %>%
    distinct()
  stations <- merge(x = tmp, y = stations, by="Stations_id")
  # add columns for the start and end year of the data
  stations$bisJahr <- as.numeric(substr(x =stations$bis_datum, 1,4))
  stations$vonJahr <- as.numeric(substr(x =stations$von_datum, 1,4))

  return(list(stations=stations, historical=historical, recent=recent))
}






#' Title Get_ZipLists_Station_ids
#'
#' @param dataperiod "recent", "historical", "both"
#' @param loadnew either true or false
#' @param localStationdata_fn name of the local file with the station meta data
#'
#' @return list with stationlist, ziplist, histziplist, recentziplist
#' @export
#'
# @examples Get_ZipLists_Station_ids("recent", T, "stationdata.RData")
#'
Get_ZipLists_Station_ids <- function(dataperiod="recent", loadnew=T,
                                     localStationdata_fn = "stationdata.RData")  {


  stopifnot(dataperiod %in% c("recent", "historical", "both"))
  period <- dataperiod
  if (period %in% c("recent", "both")) {
    if (loadnew==T) {
      recent     <- getDWDContent(DWD_ftp_recent)
    } else
    {
      load(paste(Local_R_DWD, localStationdata_fn, sep = "/"))
    }
  }

  if (period %in% c("historical","both")) {
    if (loadnew==T) {
      historical <- getDWDContent(DWD_ftp_historical)
    } else
    {
      load(paste(Local_R_DWD, localStationdata_fn, sep = "/"))
    }
  }

  if (period == "recent"){
    ziplist <- recent$ziplist
    stationlist <- recent$stationlist %>% arrange(Stations_id, desc(bis_datum))
    historical <- NULL
  }

  if (period == "historical"){
    ziplist <- historical$ziplist
    stationlist <- historical$stationlist %>% arrange(Stations_id, desc(bis_datum))
    recent <- NULL
  }
  if (period == "both"){
    ziplist <- c(recent$ziplist, historical$ziplist)
    stationlist <- rbind(recent$stationlist,historical$stationlist)
    stationlist <- stationlist %>% arrange(Stations_id, desc(bis_datum))
  }
  zipIDs <- gsub("tageswerte_KL_", "", ziplist)
  zipIDs <- unique(zipIDs)
  zipIDs <- stri_split_fixed(zipIDs, "_", n = 1, tokens_only=TRUE, simplify=TRUE)[,1]
  stationlist <- stationlist %>% filter(Stations_id %in% zipIDs)%>% distinct(Stations_id, .keep_all = T)
  return <- list(stationlist=stationlist, ziplist=ziplist, histziplist=historical$ziplist, recentziplist=recent$ziplist)
}



## Calculation of radiation from DWD weather data --------------------------------------------------

#' getRadWeather calculates radiation from DWD weather data
#'
#' @param WeatherTab Data frame with DWD data including sunshine duration without radiation
#'
#' @return Data frame with radiation in [MJ.m-2.d-1] and the other columns
#' @export
#' @details also saturation deficit is calculated
#'
# @examples getRadWeather(WeatherTab)
getRadWeather <- function(WeatherTab) {
  # Validate input parameter
  if (!is.data.frame(WeatherTab)) {
    stop("Invalid input. 'WeatherTab' should be a data frame.")
  }

  # Create a copy of the input data frame
  RadWeather <- WeatherTab

  # Convert Date column to Date class and calculate DOY (day of year)
  RadWeather$Date <- as.Date(RadWeather$Date)
  RadWeather$DOY <- as.integer(format(RadWeather$Date, "%j"))
#  if ("sf" %in% class(RadWeather)) {

  # add geoLaenge and geoBreite as separate columns
  if (!is.null(RadWeather$geometry)) {
    RadWeather <- RadWeather %>%
      mutate(geoLaenge = unlist(map(RadWeather$geometry,1)),
             geoBreite = unlist(map(RadWeather$geometry,2)))
  }

  # Calculate solar parameters using calcsolar function
  RadWeather.Sun <- calcsolar(dayofyear = RadWeather$DOY, latitude = RadWeather$geoBreite)

  # calculate Dayl and Angot
  RadWeather$Dayl <- RadWeather.Sun$daylength
  RadWeather$Angot <- RadWeather.Sun$angot.value
  rm(RadWeather.Sun)

  # Calculate relative sunshine duration (RelSun)
  RadWeather$RelSun <- RadWeather$SONNENSCHEINDAUER / RadWeather$Dayl

  # Convert Date column to Month and factorize it
  RadWeather$Monat <- factor(month(RadWeather$Date), levels = 1:12)

  # Calculate estimated global radiation (EstGlobRad) using RelRad function
  RadWeather$EstSg_S0 <- RelRad(RadWeather$RelSun, as.integer(as.character(RadWeather$Monat)))
  RadWeather$EstGlobRad <- RadWeather$EstSg_S0 * RadWeather$Angot

  # Convert Date column to Excel time representation
  RadWeather$ExcelTime <- as.integer(difftime(RadWeather$Date, as.Date("1899-12-30")))

  # Calculate VP (Vapour Pressure) and Sat_def (Saturation Deficit)
  RadWeather$VP <- 6.1078 * exp(17.2694 * RadWeather$LUFTTEMPERATUR / (RadWeather$LUFTTEMPERATUR + 238.3)) * RadWeather$REL_FEUCHTE / 100
  RadWeather$Sat_def <- 6.1078 * exp(17.2694 * RadWeather$LUFTTEMPERATUR / (RadWeather$LUFTTEMPERATUR + 238.3)) * (1 - RadWeather$REL_FEUCHTE / 100)

  # Calculate Rad_Int (Integrated Radiation)
  RadWeather$Rad_Int <- RadWeather$EstGlobRad * 1000 * 1000 / (24 * 60 * 60)

  # Select desired columns
  selcols <- c("Stations_id",  "WINDGESCHWINDIGKEIT", "NIEDERSCHLAGSHOEHE",#"MESS_DATUM",
               "SONNENSCHEINDAUER", "LUFTTEMPERATUR", "REL_FEUCHTE",
               "LUFTTEMPERATUR_MAXIMUM", "LUFTTEMPERATUR_MINIMUM", "MHoeheWind",
               "Date", "Stationshoehe", "geoBreite", "geoLaenge",
               "Stationsname", "DOY", "Dayl", "Angot",
               "RelSun", "Monat", "EstSg_S0", "EstGlobRad", "ExcelTime",
               "VP", "Sat_def", "Rad_Int", "geometry")

  # Return the data frame with selected columns
  RadWeather <- as.data.frame(RadWeather)
#  RadWeather <- RadWeather[,selcols]# RadWeather[, selcols, drop = FALSE]

  return(RadWeather)
}

## Download functions for DWD weather data --------------------------------------------------


#' Copy_DWD_ZipFiles makes a local copy of DWD weather data as zip files
#'
#' @param DWD_ftp_ ftp address of DWD  weather data
#' @param LocalCopy_DWD_ftp_ directory for storing local copy of DWD data
#' @import httr
#' @import curl
#' @import stringr
#'
#' @return Nothing
#' @export
#'
Copy_DWD_ZipFiles <- function(DWD_ftp_, LocalCopy_DWD_ftp_) {

  # copy all DWD files, (in particular zip files from the ftp server to the local directory
  # DWD_ftp_ = "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/"
  # LocalCopy_DWD_ftp_ = "C:/Users/.../LocalCopyDWD/recent/"
  # DWD_ftp_ = "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/"
  # LocalCopy_DWD_ftp_ = "C:/Users/.../LocalCopyDWD/historical/"

  # check if the local directory exists, if not create it
  if (!dir.exists(LocalCopy_DWD_ftp_)) {
    dir.create(LocalCopy_DWD_ftp_, recursive = T)}

  # Validate input parameters
  if (!is.character(DWD_ftp_) || !is.character(LocalCopy_DWD_ftp_)) {
    stop("Invalid input. 'DWD_ftp_' and 'LocalCopy_DWD_ftp_' should be character strings.")
  }
  # Set user agent for the HTTP request
  httr::user_agent("R User Agent")

  # Create a new handle with anonymous user credentials
  h <- new_handle()
  handle_setopt(h, userpwd = "anonymous")

  # Check if the FTP repository URL ends with a slash, add if missing
  if (!str_ends(DWD_ftp_, "/")) {
    DWD_ftp_ <- paste0(DWD_ftp_, "/")
  }

  # Get the list of file names from the FTP directory
  tryCatch({
    response <- GET(DWD_ftp_, dirlistonly = TRUE)
    #  #  if (http_type(response) != "text/html") {
    #  #    stop("Failed to retrieve file list from FTP server.")
    #  #  }
    filelist <- sort(stri_split_lines1(getURL(DWD_ftp_, dirlistonly = TRUE)))
  }, error = function(e) {
    stop("Failed to retrieve file list from FTP server.")
  })

  # Download each file from the FTP server to the local directory
  for (i in seq_along(filelist)) {
    filename <- filelist[i]
    dest_file <- paste(LocalCopy_DWD_ftp_,filename, sep="/" )
    cat(i, "\n")
    if (nchar(dest_file) > 1) {
      tryCatch({
        curl_download(paste0(DWD_ftp_, filename), destfile = dest_file, handle = h, quiet = FALSE)
      }, error = function(e) {
        warning(paste("Failed to download file:", filename))
      })
    }
  }
}



#' UpdateDWDData_to_fst updates the DWD weather data to a local fst file
#'
#' @param dataperiod choose recent or historical weather data
#' @param startdate start date for the weather data
#' @param isloadnew option to load data from DWD ftp server
#' @param DWD_content list with stationlist, ziplist, zipID
#' @param MinDataset option to load only the minimum dataset
#' @import fst
#' @import lubridate
#'
#' @return Nothing, a fst file is written to the local directory
#' @export
#'
# @examples UpdateDWDData_to_fst("recent", "1990-01-01", T, DWD_content=NULL, MinDataset=FALSE)
#'
#'

UpdateDWDData_to_fst <- function(dataperiod="recent", startdate="1990-01-01", isloadnew=T,
                                 DWD_content=NULL, MinDataset=FALSE) {

  # check if dataperiod parameter is valid
  stopifnot(dataperiod %in% c("recent", "historical"))
  startyear <- year(as.Date(startdate))

  ### load station data if not given as parameter
  if (is.null(DWD_content)){
    DWD_content <- getDWDStationList(historical = DWD_ftp_historical,
                                     recent = DWD_ftp_recent)
  }

  # use recent or historical data
  if (dataperiod == "recent") {
    stationlist <- DWD_content$recent$stationlist
    stationlist <- stationlist %>% distinct(Stations_id, .keep_all = T)
    Stationids  <- substr(DWD_content$recent$ziplist, 15, 19)
    ziplist <- DWD_content$recent$ziplist
  }

  if (dataperiod == "historical") {
    stationlist <- DWD_content$historical$stationlist
    stationlist <- stationlist %>% distinct(Stations_id, .keep_all = T)
    Stationids  <- substr(DWD_content$historical$ziplist, 15, 19)
    ziplist <- DWD_content$historical$ziplist
  }

  # length of the station list
  n_df <- length(Stationids)

  # prepare list of data frames for weather data import
  df_list <- vector(mode = "list", n_df)

  ## loop over the stations and get the weather data
  if (dataperiod == "recent") {
    for (i in 1:length(Stationids)){
      #  i <- 18
      station <- Stationids[i]
      if (isloadnew) {
        df_list[[i]] <- try(getsingleDWDWeather(station = station, ziplist = ziplist, repository = DWD_ftp_recent), silent = F)
      } else {
        df_list[[i]] <- try(getsingleDWDWeather(station = station, ziplist = ziplist, repository = LocalCopy_DWD_ftp_recent), silent = F)
      }
    }
  }

  # same for historical data
  if (dataperiod == "historical") {
    for (i in 1:length(Stationids)){
      #  i <- 18
      station <- Stationids[i]
      #  df_list[[i]] <- try(getsingleDWDWeather(station = station, ziplist = ziplist, repository = DWD_ftp_historical, local = isloadnew), silent = F)
      if (isloadnew) {
        df_list[[i]] <- try(getsingleDWDWeather(station = station, ziplist = ziplist, repository = DWD_ftp_historical), silent = F)
      } else {
        df_list[[i]] <- try(getsingleDWDWeather(station = station, ziplist = ziplist, repository = LocalCopy_DWD_ftp_historical), silent = F)
      }


#      df_list[[i]] <- getsingleDWDWeather(station = station, ziplist = ziplist, repository = DWD_ftp_historical, local = F)
    }
  }

  # remove empty data frames (no valid names etc.)
  df_list <-   df_list[lapply(df_list, length) > 1]

  # convert list of data frames in one
  df <- try(data.table::rbindlist(df_list, use.names = T, fill = TRUE), silent = T)
  rm(df_list)

  #  df$Date <- NULL
  df$Date <- ymd(df$MESS_DATUM)
  df <- df %>% dplyr::filter(Date >= as.Date(startdate))

  # rename columns
  df <- RenameDWDWeather(df)

  # retrieve stations_id from filename
  df$Stations_id <- sprintf("%05i", type.convert(df$Stations_id, dec = ".", as.is="T"))
  df <- df %>% left_join( stationlist, by = "Stations_id")
  class(df) <- class(as.data.frame(df))

  # remove rows with missing geoBreite which is a necessary information. This is a dirty patch and the basic reasons should be tackled.
  df <- df %>%
    filter(!is.na(geoBreite))

  if (MinDataset==FALSE) {
    # add estimates for radiation
    df <- getRadWeather(df)
  } else {
    df <- df %>% select( "Stations_id","Date", longNames_DWD_core)
  }

  #  df <- df %>% select( -FX, -RSKF, -SHK_TAG, -NM, -VPM,-eor, -TGK)

  # save data to fst file
  if (dataperiod == "recent") {
    fn <- paste0(Local_Rdata_path,"/recent_weather_dat_",as.character(startyear),".fst")
    write.fst(x = df, path = fn)}
  if (dataperiod == "historical") {
    fn <- paste0(Local_Rdata_path,"/weather_dat_",as.character(startyear),".fst")
    write.fst(x = df, path = fn)}
}


#' Title getsingleDWDWeather loads data for a single weather station
#'
#' @details This is a function which loads data for a single weather station either for
#' recent or historical weather data. The data are retrieved in the DWD format.
#' The measurement height for wind speed is taken from the meta data of the station and
#' added to the data frame and wind speed is corrected to a standard height of 10 m.
#'
#' @param station 5 digit ID of the weather station as character
#' @param ziplist Character array with the ZIP-Filenames
#' @param repository ftp Repository address or local directory
#' @param quiet echo on/off
#' @import dplyr
#' @import utils
#' @return data frame with weather data from DWD for either historical or recent zip files
#' @export
#'
# @examples getsingleDWDWeather("00044", ziplist, DWD_ftp_recent, local=F, quiet=T)
getsingleDWDWeather <- function(station, ziplist, repository, quiet=T){

  # check if the repository is local
  local <- FALSE

  if (repository == LocalCopy_DWD_ftp_historical){
    local <- TRUE
    dest_file <- paste0(repository, "/")
  }
  if (repository == LocalCopy_DWD_ftp_recent){
    local <- TRUE
    dest_file <- paste0(repository, "/")
  }

  # if no local unzip directory is existing, create it
  if (!dir.exists(paste0(path_unzip))){
    # if unzip directory is not existing, create it
    dir.create(path_unzip, recursive = T)
  } else {

  # delete all files in the unzip directory just in case there are some left
  # get all files in the unzip directory, recursively
    f <- list.files(path_unzip, include.dirs = F, full.names = T, recursive = T)
  # remove the files
    file.remove(f)
  }

  # name of file with measurement height of wind data
  df_mHeight <- NULL

  # select the file name for to be zipfile to be downloaded/unzipped from ziplist
  ZipFilename <- grep(paste0("tageswerte_KL_", station), ziplist, value = TRUE)
  if (nchar(ZipFilename)<2) {
    stop()
    return <- NULL
  }


  # if not local  download the zip file from the ftp server
  if (!local) {
    # name of the destination file were the data are stored
    dest_file <- paste0(path_unzip, "/", ZipFilename)
    download.file(url=paste0(repository,"/", ZipFilename),
                destfile=dest_file, mode = "wb", quiet = quiet, method = "auto")} else {
    dest_file <- paste0(repository, "/", ZipFilename)
                }

  # unzip the file to the unzip directory
  unzip(zipfile = dest_file,
            exdir = path_unzip)

  # construct the name of the data file
  DataFilename <- grep("produkt_klima_tag_", list.files(path_unzip), value = TRUE)

  # read weather data from temporary file
  DWDWeather <- read.table(file = paste(path_unzip, DataFilename, sep="/"),
                           header=TRUE, sep=";", quote="", dec=".", na.strings=c("-999", "\032"),
                           stringsAsFactors=FALSE, strip.white=TRUE, fill=TRUE, fileEncoding="latin1")
  DWDWeather$STATIONS_ID <- sprintf("%05i", type.convert(DWDWeather$STATIONS_ID, dec = ".", as.is="T"))

  # add standard measurement height for wind speed
  DWDWeather$MHoeheWind <- 10 # default value [m]
  DWDWeather$Date <- as.Date(as.character(DWDWeather$MESS_DATUM), "%Y%m%d")

  # read meta data for wind speed measurement height
  MetaWindfn <- paste0("Metadaten_Geraete_Windgeschwindigkeit_", station,".txt")

  if(file.exists(paste0(path_unzip, "/",MetaWindfn))){
  MetaWinddata <- read.table(file = paste(path_unzip, MetaWindfn, sep="/"),
                             header=TRUE, sep=";", quote="", dec=".", na.strings=c("-999", "\032"),
                             stringsAsFactors=FALSE, strip.white=TRUE, fill=TRUE, fileEncoding="latin1")
  # letzte Zeile raus
  MetaWinddata <- MetaWinddata[-nrow(MetaWinddata),]
  MetaWinddata <- MetaWinddata %>% mutate(Von_Datum = as.Date(as.character(Von_Datum), format="%Y%m%d"),
                                         Bis_Datum=as.Date(as.character(Bis_Datum), format="%Y%m%d"))

  # extract the measurement height for wind speed
  dateseq <- NULL#Date()
  Heightseq <- NULL
  for (i in 1:nrow(MetaWinddata)){
    dateseqi <- seq(MetaWinddata[i, "Von_Datum"], MetaWinddata[i, "Bis_Datum"], "1 day" )
    dateseq <- c(dateseq, dateseqi)
    Heightseqi <- rep(MetaWinddata[i, "Geberhoehe.ueber.Grund..m."], length(dateseqi))
    Heightseq <- c(Heightseq, Heightseqi)
  }

  df_mHeight <- data.frame(Date=as.Date(x = dateseq, origin="1970-01-01"), MHoeheWind=Heightseq)
  df_mHeight <- df_mHeight %>% distinct(Date, .keep_all = T)

  # add measurement height for wind speed to the data frame
  if (!is.null(df_mHeight)){
    DWDWeather$MHoeheWind <- NULL
    DWDWeather <- DWDWeather %>% left_join(x = DWDWeather, y = df_mHeight, by="Date")

    # correct for measurement heigts different to 10m
    DWDWeather$FM <-  windheight( ui = DWDWeather$FM, zi =  DWDWeather$MHoeheWind, zo = 10)
  }

  }
  DWDWeather <- DWDWeather %>% dplyr::rename(Stations_id = STATIONS_ID)
  return(DWDWeather)
}



#' getsingleDWDRain loads rainfall data for a single weather station
#'
#' @param station 5 digit string ID of the weather station as character
#' @param ziplist Character array with the ZIP-Filenames
#' @param repository ftp Repository address or local directory
#' @param local Option to use local copy or ftp data
#' @param quiet echo on/off
#'
#' @return data frame with rain data from DWD for either historical or recent zip files
#' @export
#'
# @examples getsingleDWDRain("00044", ziplist, DWD_ftp_recent, local=F, quiet=T)
getsingleDWDRain <- function(station, ziplist, repository, quiet=T){


  # check if the repository is local
  local <- FALSE

  if (repository == LocalCopy_DWD_Rain_ftp_historical){
    local <- TRUE
    dest_file <- paste0(repository, "/")
  }
  if (repository == LocalCopy_DWD_Rain_ftp_recent){
    local <- TRUE
    dest_file <- paste0(repository, "/")
  }

  # if no local unzip directory is existing, create it
  if (!dir.exists(paste0(path_unzip))){
    # if unzip directory is not existing, create it
    dir.create(path_unzip, recursive = T)
  } else {

    # delete all files in the unzip directory just in case there are some left
    # get all files in the unzip directory, recursively
    f <- list.files(path_unzip, include.dirs = F, full.names = T, recursive = T)
    # remove the files
    file.remove(f)
  }

  ZipFilename <- grep(paste0("tageswerte_RR_", station), ziplist, value = TRUE)

  if (nchar(ZipFilename)<2) {
    stop()
    return <- NULL
  }


  # if not local  download the zip file from the ftp server
  if (!local) {
    # name of the destination file were the data are stored
    dest_file <- paste0(path_unzip, "/", ZipFilename)
    download.file(url=paste0(repository,"/", ZipFilename),
                  destfile=dest_file, mode = "wb", quiet = quiet, method = "auto")} else {
                    dest_file <- paste0(repository, "/", ZipFilename)
                  }

  # unzip the file to the unzip directory
  unzip(zipfile = dest_file,
        exdir = path_unzip)

  DataFileName <- grep("produkt_nieder_tag_", list.files(path_unzip), value = TRUE)

  DWDWeather <- read.table(file = paste(path_unzip, DataFileName, sep="/"),
                           header=TRUE, sep=";", quote="", dec=".", na.strings=c("-999", "\032"),
                           stringsAsFactors=FALSE, strip.white=TRUE, fill=TRUE, fileEncoding="latin1")
  DWDWeather$Date <- as.Date(as.character(DWDWeather$MESS_DATUM), "%Y%m%d")
  DWDWeather$STATIONS_ID <- sprintf("%05i", type.convert(DWDWeather$STATIONS_ID, dec = ".", as.is="T"))

  DWDWeather <- DWDWeather %>% dplyr::rename(Stations_id = STATIONS_ID)

  return(DWDWeather)
}



#' getcombinedDWDWeather gets the combined (historic & recent) weather data for a DWD station
#'
#' @param DWD_content A list structure of the DWD stations
#' @param station The ID of the station in 5 digits but as character
#' @param local Option to use local stored zip-files
#' @param recent_repository The ftp address of the recent zip files or the local directory
#' @param historical_repository The ftp address of the historical zip files or the local directory
#'
#' @return data frame with DWD observations historical + recent
#' @export
#'
# @examples getcombinedDWDWeather(DWD_content, "00044", local=F, DWD_ftp_recent, DWD_ftp_historical)
getcombinedDWDWeather <- function(DWD_content, station, local=F,
                                  recent_repository, historical_repository){
  # stattion ID to character
  station <- as.character(station)
  if(station %in% DWD_content$historical$zipID){
    # if the station is in the historical zipID list, get the historical data
    data.hist <- getsingleDWDWeather(station, DWD_content$historical$ziplist, repository = historical_repository)
  }
  if(station %in% DWD_content$recent$zipID){
    # if the station is in the recent zipID list, get the recent data
    data.rct <- getsingleDWDWeather(station, DWD_content$recent$ziplist, repository = recent_repository)
  }

  if(exists("data.hist") & exists("data.rct")){
    # detect the last date of the historical data
    t1 <- data.hist$Date[length(data.hist$Date)]
    # combine the historical and recent data filtered first by the last date of the historical data
    data.sum <- rbind(data.hist, data.rct[data.rct$Date > t1,])
  }else{
    if(exists("data.hist")){
      # if only historical data is available
      data.sum <- data.hist
    }else{
      if(exists("data.rct")){
      # if only recent data is available
      data.sum <- data.rct
      }
    }
  }
  if (exists("data.sum")) {
    # add the meta data of the station to the data frame
    data.sum <- cbind(data.sum,
                    DWD_content$stations[DWD_content$stations$Stations_id==station, c("Stationshoehe", "geoBreite", "geoLaenge", "Stationsname")])
    # remove empty last row
    data.sum <- subset(data.sum, !is.na(Stations_id))
  }else{
    data.sum <- NA
    }
  return(data.sum)
}


#' getcombinedDWDRain gets the combined (historic & recent) rain data for a DWD station
#'
#' @param DWD_content A list of the DWD station
#' @param station The ID of the station in 5 digits but as character
#' @param local Option to use local stored zip-files
#' @param recent_repository The ftp address of the recent zip files or the local directory
#' @param historical_repository The ftp address of the historical zip files or the local directory
#'
#' @return data frame with DWD observations historical + recent
# @examples getcombinedDWDRain(DWDRain_content, "00044", local=F, DWDRain_ftp_recent, DWDRain_ftp_historical)
getcombinedDWDRain <- function(DWDRain_content, station, local=F,
                               recent_repository, historical_repository){
  station <- as.character(station)
  if(station %in% DWDRain_content$historical$zipID){
    # if the station is in the historical zipID list, get the historical data
    data.hist <- getsingleDWDRain(station, DWDRain_content$historical$ziplist, DWDRain_ftp_historical)
  }
  if(station %in% DWDRain_content$recent$zipID){
    # if the station is in the recent zipID list, get the recent data
    data.rct <- getsingleDWDRain(station, DWDRain_content$recent$ziplist, DWDRain_ftp_recent)
  }

  if(exists("data.hist") & exists("data.rct")){
    # detect the last date of the historical data
    t1 <- data.hist$Date[length(data.hist$Date)]
    # combine the historical and recent data filtered first by the last date of the historical data
    data.sum <- rbind(data.hist, data.rct[data.rct$Date > t1,])
  }else{
    # if only historical data is available
    if(exists("data.hist")){
      data.sum <- data.hist
    }else{
      # if only recent data is available
      if(exists("data.rct")){
        data.sum <- data.rct
      }
    }
  }
  if (exists("data.sum")) {
    DWDRain_content$stations$Stations_id <- as.integer(DWDRain_content$stations$Stations_id)
#    data.sum <- cbind(data.sum,
#                      DWDRain_content$stations[DWDRain_content$stations$Stations_id==station, c("Stationshoehe", "geoBreite", "geoLaenge", "Stationsname")])
    SStations <- DWDRain_content$stations
    #    data.sum <- merge(x = data.sum, y=as.data.frame(DWDRain_content$stations[,c("Stations_id","Stationshoehe", "geoBreite", "geoLaenge", "Stationsname")]),
    #                  by.x="Stations_id",by.y="Stations_id")

    # rename the column name of the station id
    if ("STATIONS_ID" %in% names(data.sum)) {
      names(data.sum)[names(data.sum)=="STATIONS_ID"] <- "Stations_id"}

    # add the meta data of the station to the data frame
    data.sum <- merge(x = data.sum, y=SStations[,c("Stations_id","Stationshoehe", "geoBreite", "geoLaenge", "Stationsname")],
                      by="Stations_id")
    # remove empty last row
    data.sum <- subset(data.sum, !is.na(Stations_id))
  }else{
    data.sum <- NA
  }
  return(data.sum)
}



#' Title DWDisMeasured
#'
#' @param weather_historic historic weather data
#' @import dplyr
#'
#' @return data frame with DWD station info for selected stations
#' @export
#'
DWDisMeasured <- function  (df, Maxpct_miss = 0.3){

  selvars <- longNames_DWD[!longNames_DWD %in% c("QUALITAETS_NIVEAU","LUFTDRUCK_STATIONSHOEHE")]
  selvars <- c("Stations_id", selvars)

  df <- df %>% select(all_of(selvars))
  dfl <- df %>% pivot_longer(cols = -Stations_id, names_to = "variable", values_to = "value")
  df_missing <- dfl %>% group_by(Stations_id, variable) %>%
    summarise(n_miss = sum(is.na(value)), n=n(), pct_miss = n_miss/n*100) %>%
     mutate(IsMeasured = ifelse(pct_miss < Maxpct_miss, TRUE, FALSE))
}

#' SelectStationsByDataAvailability selects the  weather stations from a list of stations
#' by data availability, more stations are selected until a minimum number
#' of stations is reached which contain the required data
#'
#' @param lat Latitude of location
#' @param long Longitude of location
#' @param height_loc Height of the location
#' @param stationlist List of stations from which the stations should be selected
#' @param station_pars List of stations and their measured parameters
#' @param minstations minimum number of stations to be selected
#' @param MinRedundancy Minimum redundancy which has to be reached for all parameters
#' @param startdate date from which weather data should be available
#' @param max.Height.Distance_m maximum height
#' @param radius Maximum radius where the station should be selected from
#' @importFrom sf st_as_sf st_set_crs st_distance st_geometry
#' @import dplyr
#' @importFrom purrr map
#'
#' @return data frame with DWD station info for selected stations
#' @export
#'
SelectStationsByDataAvailability <- function(lat, long,
                                          height_loc=100,
                                          stationlist,
                                          station_pars,
                                          minstations=3,
                                          MinRedundancy=3,
                           max_stations=20,
                           radius=70000,
                           startdate,
                           max.Height.Distance_m=100) {

  # create a data frame from location coordinates
  Location <- data.frame(Latitude=lat, Longitude=long)
  Location <- st_as_sf(Location, coords = c("Longitude", "Latitude")) %>%
    st_set_crs(value = "+proj=longlat +datum=WGS84")

  # make an sf object from stationlist data frame
  stationlist <- st_as_sf(stationlist, coords = c("geoLaenge", "geoBreite")) %>%
    st_set_crs(value = "+proj=longlat +datum=WGS84")

  # calculate distances between stations and location
  stationlist$Distance_m <- pmax(1,as.numeric(st_distance(stationlist, Location))) # minimum distance is 1 m because
  stationlist$Distance_km <- format(stationlist$Distance_m /1000, digits = 3)

  # filter
  stationlist <- stationlist %>%
    mutate("Height.Distance_m" = Stationshoehe - height_loc) %>%
    filter(Distance_m < radius,
           bis_datum > startdate,
           Height.Distance_m < abs(max.Height.Distance_m)) %>%
    arrange(Distance_m) %>% mutate(dist_ndx = row_number())


  Redundancy <- 0
  OldRedundancy <--1
  nStations <- 0
  success <- F
  selvars <- longNames_DWD[!longNames_DWD %in% c("QUALITAETS_NIVEAU","LUFTDRUCK_STATIONSHOEHE")]
  df_redundancy <- data.frame( variable = selvars, nMeasured = numeric(length(selvars)))
  SelIDs <- NULL
  repeat {
    for (ndx in stationlist$dist_ndx)  {
      act_station <- stationlist %>% filter(dist_ndx == ndx)
      act_stationpar <- station_pars %>%
        filter(Stations_id == act_station$Stations_id)
      if (nrow(act_stationpar) != 0) {
      for (ActVar in selvars) {
        IsMeasured <- act_stationpar %>% filter(variable == ActVar)
        IsMeasured <- IsMeasured$IsMeasured
        if  (IsMeasured==TRUE){
          df_redundancy[df_redundancy$variable==ActVar, "nMeasured"] <- df_redundancy[df_redundancy$variable==ActVar, "nMeasured"] + 1
        }
      }
        nStations <- nStations + 1
        OldRedundancy <- Redundancy
        Redundancy <- min(df_redundancy$nMeasured)
        if (nStations < minstations) {
          SelIDs <- c(SelIDs, act_station$Stations_id)
        } else
        if (Redundancy > OldRedundancy) {
          SelIDs <- c(SelIDs, act_station$Stations_id)
        }
      if (Redundancy >= MinRedundancy) {
        success <- TRUE
        break}
      }
      if(success == TRUE) {break}
    }
    if(success == TRUE) {break}
  }

#  SelIDs <- strsplit(SelIDs, " ")
  SelIDs <- unique(SelIDs)
  stations_selected <- stationlist %>% filter(Stations_id %in% SelIDs)

    # add geoLaenge and geoBreite as columns
  stations_selected <- stations_selected %>%
    mutate(geoLaenge = unlist(map(stations_selected$geometry,1)),
           geoBreite = unlist(map(stations_selected$geometry,2)))
  #  st_geometry(stations_selected) <- NULL
  return (stations_selected)
}



#' SelectStations selects the nearest weather stations from a list of stations
#'
#' @param lat Latitude of location
#' @param long Longitude of location
#' @param height_loc Height of the location
#' @param stationlist List of stations from which the stations should be selected
#' @param minstations minimum number of stations to be selected
#' @param radius Maximum radius where the station should be selected from
#' @param startdate date from which weather data should be available
#' @param max.Height.Distance_m maximum height
#' @importFrom sf st_as_sf st_set_crs st_distance st_geometry
#' @import dplyr
#'
#' @return data frame with DWD station info for selected stations
#' @export
#'
# @examples SelectStations(50.0, 8.0, 100, stationlist, 3, 20, 70000, "1990-01-01", 100)
SelectStations <- function(lat, long, height_loc=100, stationlist, minstations=7,
                           max_stations=20,
                           radius=70000,
                           startdate,
                           max.Height.Distance_m=100) {

# create a data frame from location coordinates
  stationlist$bis_datum <- as.Date(as.character(stationlist$bis_datum), "%Y%m%d")
  stationlist$von_datum <- as.Date(as.character(stationlist$von_datum), "%Y%m%d")
  startdate <- as.Date(startdate, "%Y-%m-%d")
  Location <- data.frame(Latitude=lat, Longitude=long)
  Location <- st_as_sf(Location, coords = c("Longitude", "Latitude")) %>%
    st_set_crs(value = "+proj=longlat +datum=WGS84")

# make an sf object from stationlist data frame
  stationlist <- st_as_sf(stationlist, coords = c("geoLaenge", "geoBreite")) %>%
    st_set_crs(value = "+proj=longlat +datum=WGS84")

# calculate distances between stations and location
  stationlist$Distance_m <- pmax(1,as.numeric(st_distance(stationlist, Location))) # minimum distance is 1 m because
  stationlist$Distance_km <- as.numeric(format(stationlist$Distance_m /1000, digits = 3))

# filter
  stationlist <- stationlist %>%
    mutate("Height.Distance_m" = Stationshoehe - height_loc) %>%
    filter(Distance_m < radius,
           bis_datum > startdate,
           Height.Distance_m < abs(max.Height.Distance_m)) %>%
    arrange(Distance_m)

  # filter version with nearest minstations stations
  Minstation_list <- stationlist %>% arrange(Distance_m) %>% slice_head(n=minstations)

  # filter version with nearest according to radius
  Nearstation_list <- stationlist %>% filter(Distance_m<=radius)

  # select the list with the minimum number of stations if its larger
  # than the list with the nearest stations
  if(nrow(Minstation_list)>nrow(Nearstation_list))
    {stations_selected <- Minstation_list} else
  {stations_selected <- Nearstation_list}

  # if the number of stations is larger than max_stations, select the nearest max_stations
  if(nrow(stations_selected)>max_stations){
    stations_selected <- Nearstation_list %>% slice_head(n=max_stations)
  }
  rm(Nearstation_list, Minstation_list)
  # add geoLaenge and geoBreite as columns
  stations_selected <- stations_selected %>%
    mutate(geoLaenge = unlist(map(stations_selected$geometry,1)),
           geoBreite = unlist(map(stations_selected$geometry,2)))
#  st_geometry(stations_selected) <- NULL
  return (stations_selected)
}



#' GetWeatherData_selection gets the weather data for the selected stations
#' by downloading the data from the DWD ftp server
#' @param stations_selected selected stations
#' @param DWD_content list structure of available DWD stations,
#' must be either DWD_content$recent or DWD_content$historical
#' @param startdate start date for selection of data
#' @import dplyr
#' @importFrom data.table rbindlist
#'
#' @return data frame with weather data for selected stations
#' @export
#'
# @examples GetWeatherData_selection(stations_selected, DWD_content, local=F, startdate="1990-01-01")
GetWeatherData_selection <- function(stations_selected,
                                     DWD_content,
                                     repository=DWD_ftp_historical,
                                     startdate="1990-01-01") {

  # Use local data?
  local <- ifelse(repository %in% c(DWD_ftp_recent, DWD_ftp_historical), F, T)

  n_df <- length(stations_selected$Stations_id)
  ## prepare list of data frames for weather data import
  df_list <- vector(mode = "list", n_df)

  weatherdata <- NULL
  # loop over selected stations
  for (station in stations_selected$Stations_id) {
    # for debugging
    #    ID <- station_selected$Stations_id[1]
    # is station in the list of available zip files?
    if (station %in% DWD_content$zipID){

      # get the weather data for the station
      weather <- getsingleDWDWeather(station = station,
                                     ziplist = DWD_content$ziplist,
                                     repository = repository,
                                     quiet = T)
      weather$Stations_id <- station
      # add to list of data frames
      df_list[[station]] <- weather}
  }
  ## remove empty data frames (no valid names etc.)
  df_list <-   df_list[lapply(df_list, length) > 1]
  ## convert list of data frames in one
  # convert list of data frames in one
  weatherdata <- try(data.table::rbindlist(df_list, use.names = T), silent = T)
  rm(df_list)
  weatherdata <- as.data.frame(weatherdata)
  # remove data older than start date
  weatherdata <- weatherdata %>% filter(Date >= as.Date(startdate))
  # rename columns
  weatherdata <- RenameDWDWeather(weatherdata)
  # add station info
  weatherdata <- weatherdata %>% left_join( stations_selected, by = "Stations_id")
  # add radiation data
  weatherdata <- getRadWeather(weatherdata)
  stations_selected$Distance_km <- stations_selected$Distance_m / 1000
  # correction of windspeed now done in GetsingleDWDWeather
  # calculate wind speed at 10 m
#  weatherdata$WINDGESCHWINDIGKEIT <-  windheight( ui = weatherdata$WINDGESCHWINDIGKEIT,
#                                                     zi =  weatherdata$MHoeheWind, zo = 10)
  weatherdata <- merge(x = weatherdata, y = stations_selected[,c("Stations_id", "Distance_m", "Distance_km", "Height.Distance_m")], by="Stations_id")
  ## add additional time indicators, remove Values older than start date
  weatherdata <- weatherdata %>% mutate(Jahr=year(Date), Monat=month(Date),
                                              ExcelTime = as.integer(difftime(Date, as.Date("1899-12-30")))
  ) %>%
    dplyr::select(c(all_of(idvars), "Date", "Jahr","Monat","ExcelTime", "MHoeheWind", all_of(measvars)))


  return(weatherdata)
}




#' GetWeatherData_selection_fst
#'
#' @param stations_selected selected stations
#' @param DWD_content list structure of available DWD stations
#' @param startdate start date for selection of data
#' @import dplyr
#'
#' @return data frame with weather data for selected stations in fst format
#' @export
#'
# @examples GetWeatherData_selection_fst(stations_selected, DWD_content, local=F, startdate="1990-01-01")
GetWeatherData_selection_fst <- function(stations_selected,  DWD_content, repository=DWD_ftp_recent,
                                         startdate="1990-01-01") {


  local <- ifelse(repository == DWD_ftp_recent, F, T)
  n_df <- length(stations_selected$Stations_id)
  ## prepare list of data frames for weather data import
  df_list <- vector(mode = "list", n_df)

  weather_recent <- NULL
  #file.name = file.names[3]
  #  station_selected$Stations_id <-  sprintf("%05i", type.convert(station_selected$Stations_id, dec = "."))

  #  EndDate <- Sys.Date()
  #  Length <- EndDate-as.Date(startdate)+1

  for (station in stations_selected$Stations_id) {
    #    ID <- station_selected$Stations_id[1]
    if (station %in% DWD_content_recent$zipID){
    weather <- getsingleDWDWeather(station = station, ziplist = DWD_content$recent$ziplist,
                                     repository = repository, quiet = T)
    weather$Stations_id <- station
    df_list[[station]] <- weather}
  }
  ## remove empty data frames (no valid names etc.)
  df_list <-   df_list[lapply(df_list, length) > 1]
  ## convert list of data frames in one

  weather_recent <- try(data.table::rbindlist(df_list, use.names = T), silent = T)
  rm(df_list)
  weather_recent <- as.data.frame(weather_recent)
  weather_recent <- weather_recent %>% filter(Date >= as.Date(startdate))
  weather_recent <- RenameDWDWeather(weather_recent)
  weather_recent <- weather_recent %>% left_join( stations_selected, by = "Stations_id")
  weather_recent <- getRadWeather(weather_recent)

# corrections of wind speed now done in getsingleDWDWeather

#  weather_recent$WINDGESCHWINDIGKEIT <-  windheight( ui = weather_recent$WINDGESCHWINDIGKEIT,
#                                                  zi =  weather_recent$MHoeheWind, zo = 10)
  weather_recent <- merge(x = weather_recent, y = stations_selected[,c("Stations_id", "Distance_m", "Distance_km", "Height.Distance_m")], by="Stations_id")
  ## add additional time indicators, remove Values older than start date
  weather_recent <- weather_recent %>% mutate(Jahr=year(Date), Monat=month(Date),
                                        ExcelTime = as.integer(difftime(Date, as.Date("1899-12-30")))
  ) %>%
    dplyr::select(c(all_of(idvars), "Date", "Jahr","Monat","ExcelTime", all_of(measvars)))

  fn_historical <- paste0("./LocalCopyDWD/Rdata/weather_dat_", as.character(startdate), ".fst")
  weather_historical <- read.fst(fn_historical)
  weather_historical$Jahr <- year(weather_historical$Date)
  selcols <- names(weather_recent)
  weather_historical <- weather_historical[, selcols]
  weather_all <- rbind(weather_historical, weather_recent)

  return(weather_all)
}





#' GetRainData_selection gets the rain data for the selected stations
#' by downloading the data from the DWD ftp server
#' @param stations_selected selected stations
#' @param DWDRain_content list structure of available DWD additional rain data stations,
#' must be either DWDRain_content$recent or DWDRain_content$historical
#' @param startdate start date for selection of data
#' @import dplyr
#' @importFrom data.table rbindlist
#'
#' @return data frame with weather data for selected stations
#' @export
#'
# @examples GetWeatherData_selection(stations_selected, DWD_content, local=F, startdate="1990-01-01")
GetRainData_selection <- function(stations_selected,
                                     DWDRain_content,
                                     repository=DWDRain_ftp_historical,
                                     startdate="1990-01-01") {

  # Use local data?
  local <- ifelse(repository %in% c(DWDRain_ftp_recent, DWDRain_ftp_historical), F, T)

  n_df <- length(stations_selected$Stations_id)
  ## prepare list of data frames for weather data import
  df_list <- vector(mode = "list", n_df)

  if (repository == DWDRain_ftp_recent) {
    ziplist <- DWDRain_content$recent$ziplist
    zipID <- DWDRain_content$recent$zipID
  } else {
    ziplist <- DWDRain_content$historical$ziplist
    zipID <- DWDRain_content$historical$zipID
  }

  raindata <- NULL
  # loop over selected stations
  for (station in stations_selected$Stations_id) {
    # for debugging
    #    ID <- station_selected$Stations_id[1]
    # is station in the list of available zip files?
    if (station %in% zipID){

      # get the weather data for the station
      raindata <- getsingleDWDRain(station = station,
                                     ziplist = ziplist,
                                     repository = repository,
                                    # local = local,
                                     quiet = T)
      raindata$Stations_id <- station
      # add to list of data frames
      df_list[[station]] <- raindata}
  }
  ## remove empty data frames (no valid names etc.)
  df_list <-   df_list[lapply(df_list, length) > 1]
  ## convert list of data frames in one
  # convert list of data frames in one
  raindata <- try(data.table::rbindlist(df_list, use.names = T), silent = T)
  rm(df_list)
  raindata <- as.data.frame(raindata)
  # remove data older than start date
  raindata <- raindata %>% filter(Date >= as.Date(startdate))
  # rename columns
  raindata <- raindata %>% dplyr::rename(NIEDERSCHLAGSHOEHE = RS)
  raindata$Date <- as.Date(as.character(raindata$MESS_DATUM), "%Y%m%d")
  raindata$MESS_DATUM <- NULL
  raindata$Jahr <- year(raindata$Date)
  raindata$Monat <- month(raindata$Date)
  raindata$ExcelTime <- as.integer(difftime(raindata$Date, as.Date("1899-12-30")))
  # add station info
  raindata <- raindata %>% left_join( stations_selected, by = "Stations_id")
  # add radiation data
  raindata <- merge(x = raindata, y = stations_selected[,c("Stations_id", "Distance_m", "Distance_km", "Height.Distance_m")], by="Stations_id")
  ## add additional time indicators, remove Values older than start date
  raindata <- raindata %>% mutate(Jahr=year(Date), Monat=month(Date),
                                        ExcelTime = as.integer(difftime(Date, as.Date("1899-12-30")))
  ) %>%
    dplyr::select(c(all_of(idvars), "Date", "Jahr","Monat","ExcelTime", "NIEDERSCHLAGSHOEHE"))


  return(raindata)
}





#' getHUMEWeather converts the DWD weather data into the HUME format
#'
#' @param RadWeather Data frame with augmented DWD data
#'
#' @return Data frame in HUME weather format
#' @export
#'
# @examples getHUMEWeather(RadWeather)
getHUMEWeather <- function(RadWeather){
  HUMEWeather <- RadWeather
  HUMEWeather <- subset(HUMEWeather, select = c(ExcelTime, LUFTTEMPERATUR, NIEDERSCHLAGSHOEHE, REL_FEUCHTE,
                                                VP, Sat_def, Rad_Int, EstGlobRad, WINDGESCHWINDIGKEIT, LUFTTEMPERATUR_MINIMUM, LUFTTEMPERATUR_MAXIMUM))
  HUMEWeather$LUFTTEMPERATUR <- round(HUMEWeather$LUFTTEMPERATUR, 1)
  HUMEWeather$NIEDERSCHLAGSHOEHE <- round(HUMEWeather$NIEDERSCHLAGSHOEHE, 1)
  HUMEWeather$REL_FEUCHTE <- round(HUMEWeather$REL_FEUCHTE, 1)
  HUMEWeather$VP <- round(HUMEWeather$VP, 1)
  HUMEWeather$Sat_def <- round(HUMEWeather$Sat_def, 1)
  HUMEWeather$Rad_Int <- round(HUMEWeather$Rad_Int, 1)
  HUMEWeather$EstGlobRad <- round(HUMEWeather$EstGlobRad, 2)
  HUMEWeather$WINDGESCHWINDIGKEIT <- round(HUMEWeather$WINDGESCHWINDIGKEIT, 1)
  HUMEWeather$LUFTTEMPERATUR_MINIMUM <- round(HUMEWeather$LUFTTEMPERATUR_MINIMUM, 1)
  HUMEWeather$LUFTTEMPERATUR_MAXIMUM <- round(HUMEWeather$LUFTTEMPERATUR_MAXIMUM, 1)
  names(HUMEWeather) <- c("Time", "TMPM", "Rain", "LF", "VP", "Sat_def", "Rad_Int", "GlobRad", "Wind", "TMPMN", "TMPMX")
  return(HUMEWeather)
}




#' LoadDWDDatafromStationlist
#'
#' @param station_selected # a data frame with selected stations
#' @param RainStation_selected # a data frame with selected rain stations
#' @param Max_Height_diff <- 100 # maximum height difference (m)
#' between location and station to be accepted
#' @param weather_historic # a data frame with historical weather data
#' @param weather_recent # a data frame with recent weather data
#' @param StartYear # start year for data selection
#' @param maxError # maximum deviation of monthly parameter value
#' from average of stations as multiples of standard deviation
#' @param Max_Height_diff # maximum difference from location to station to which stations are included
#' @import dplyr
#'
#' @return interpolated weather data in HUME format including radiation data
#' @export
#'
# @examples LoadDWDDatafromStationlist(station_selected, RainStation_selected, DWD_content, DWDRain_content, weather_historic, weather_recent, local = F, StartYear= 1990, maxError = 2, Max_Height_diff = 100)
#'
  LoadDWDDatafromStationlist <- function(station_selected,
                                       RainStation_selected=NULL,
                                       DWD_content=NULL,
                                       DWDRain_content=NULL,
                                       weather_historic=NULL,
                                       weather_recent=NULL,
                                       local = F,
                                       StartYear= 1990,
                                       maxError = 2,
                                       Max_Height_diff = 100)
{

  startdate <- paste0(as.character(StartYear), "-01-01")

  # if no meta data are given, load them from DWD
  if (is.null(DWD_content)){
    DWD_content <- getDWDStationList(historical = DWD_ftp_historical, recent = DWD_ftp_recent)
  }

  # if no meta data for additional rainfall stations are given, load them from DWD
  if (is.null(DWDRain_content)){
    DWDRain_content <- getDWDRainContent(repository = DWDRain_ftp_recent)
  }


  # AddRainstations

  if (!is.null(RainStation_selected)){
    if (is.null(DWDRain_content)){
      DWDRain_content <- getDWDRainStationList(recent = DWDRain_ftp_recent, historical = DWD_ftp_historical)
    }
    # select for stations which are not in selected general weather stations
#    RainStation_selected <- RainStation_selected[!(RainStation_selected$Stations_id %in% station_selected$Stations_id),]
  }


  # if no historical weather data are given, load them from DWD
  if (is.null(weather_historic)) {


    weather_historic <- GetWeatherData_selection(stations_selected = station_selected,
                                                 DWD_content = DWD_content$historical,
#                                                 local = local,
                                                 repository = DWD_ftp_historical,
                                                 startdate = startdate)
  } else
  {
    weather_historic <- weather_historic %>% filter(Date >= startdate)
    weather_historic <- weather_historic %>% filter(Stations_id %in% station_selected$Stations_id)
    #    weather_historic <- setDT(weather_historic)
  }
  # if no recent weather data are given, load them from DWD
  if (is.null(weather_recent))  {
    weather_recent <- GetWeatherData_selection(stations_selected =  station_selected,
                                          DWD_content$recent,
                                          repository=DWD_ftp_recent,
                                          startdate=startdate)
  } else {
    # convert station id to character
    weather_recent$Stations_id <- sprintf("%05i", type.convert(weather_recent$Stations_id, dec = ".", as.is="T"))

    # filter recent weather data for selected stations
    weather_recent <- weather_recent %>%  filter(Stations_id %in% station_selected$Stations_id)
  }


  # combine historical and recent data

  weather_historic$Jahr <- year(weather_historic$Date)
  weather_recent$Jahr <- year(weather_recent$Date)
#  setDT(weather_historic)
#  setDT(weather_recent)
  weather_historic <- as.data.frame(weather_historic)


  selcols <- names(weather_recent)
#  weather_all <- rbind(weather_historic[, ..selcols], weather_recent)
  weather_all <- weather_historic %>% dplyr::select(selcols)
  weather_all <- rbind(weather_all, weather_recent)
  rm(weather_recent, weather_historic)


  # download the additional rain data for the local stations near the point of interest
  AddRainData <- NULL
  for (ID in RainStation_selected$Stations_id) {
    rain <- getcombinedDWDRain(DWDRain_content =  DWDRain_content,
                               historical_repository = DWDRain_ftp_historical, recent_repository = DWDRain_ftp_recent,
                               station = ID)
    if (!is.null(rain)){if(class(rain)=="data.frame"){
      AddRainData <- dplyr::bind_rows(AddRainData, rain)
    }}
  }
  # filter for data newer than start date
  AddRainData <- AddRainData[AddRainData$Date >= startdate,]

  # rename to more readable column names
  AddRainData <- RenameDWDRain(AddRainData)
  AddRainData$Stations_id <- sprintf("%05i", type.convert(AddRainData$Stations_id, dec = ".", as.is="T"))

  # interpolated weather data in HUME format
  xportHUME <- InterpolateWeatherData(station_selected,
                                      RainStation_selected,
                                      weather_dat = weather_all,
                                      RainData = AddRainData)


  xportHUME <- xportHUME %>% arrange(Time)


  #rm(xportw)
  return(xportHUME)
}




# Interpolation functions for DWD weather data ########################################




#' InterpolateWeatherData interpolates the weather data from the selected station to the location
#'
#' @param weather_dat weather data from the selected station in DWD format including the distance and
#' height difference to the location for which to interpolate the data
#' @param maxError maximum deviation for a parameter from the average of the station
#'  to be included in the interpolation
#' @import data.table
#' @import dplyr
#'
#' @return interpolated data in the HUME formate
#' @export
#'
# @examples InterpolateWeatherData(station_selected, RainStation_selected, weather_dat, RainData, maxError=2)
InterpolateWeatherData <- function(station_selected,
                                   RainStation_selected=NULL,
                                   weather_dat,
                                   RainData=NULL,
                                   maxError=2,
                                   startdate="1990-01-01") {


  # Add the additional rain data to the data frame
  SelNames <- names(weather_dat)
  ## add RainData do weather data
  tmp <- bind_rows(weather_dat, RainData)
  weather_dat <- tmp[, SelNames]
  rm(tmp)


  # if additional stations are given, add to the selected stations
  if (!is.null(RainStation_selected)){
    stations_selected <- rbind(station_selected[,names(RainStation_selected)], RainStation_selected)
  }

  ## All data from selected weather station into long format
  # melt version
  wetter_long <- reshape2::melt(data = weather_dat, value.name = "Value", measure.vars=measvars, variable.name="variable" )
  # dplyr version
  #weather_dat <- as.data.frame(weather_dat)
  #wetter_long <- weather_dat %>% pivot_longer(cols = all_of(measvars), values_to = "Value", names_to = "variable")
  # set to data table format
  setDT(wetter_long)
  wetter_long$Jahr <- year(wetter_long$Date)
  # add month column
  wetter_long$Monat <- month(wetter_long$Date)

  # add column with mean values for each variable and day

  # data table version
  DailyMeanRegionalWeather_l <- wetter_long[ , .(meanRegionValue=mean(Value, na.rm=T), sdRegionalValue = sd(Value, na.rm=T)),
                                             by=.(Date, variable)]

  # dplyr version
  #  DailyMeanRegionalWeather_l <- wetter_long %>% group_by(Date, variable) %>% summarise(meanRegionValue=mean(Value, na.rm=T),
  #                                                                                       sdRegionalValue = sd(Value, na.rm=T))

  # data table version
  MonthlyMeanRegionalWeather_l <- wetter_long[, .(MonthmeanRegionValue=mean(wetter_long$Value, na.rm=T)), by=c("Jahr", "Monat", "variable")]

  # dplyr version
  #  MonthlyMeanRegionalWeather_l <- wetter_long %>% mutate(Jahr=year(Date), Monat=month(Date)) %>%
  #    group_by(Jahr, Monat, variable) %>%
  #    summarise(MonthmeanRegionValue=mean(Value, na.rm=T))



  # Hinzufügen von regionalem Mittelwert und Standardabweichung als neue Spalten in long-weather
  Weatherl <- merge(x = wetter_long, y = DailyMeanRegionalWeather_l, by=c("variable", "Date"))

  # calculate relative differences
  Weatherl <- Weatherl %>% mutate (RelDiff = (Value-meanRegionValue)/sdRegionalValue)

  # filter out values with too high differences
  Weatherl <- Weatherl %>% mutate(checkedValue = ifelse(abs(RelDiff)> maxError, NA, Value))

  # new code has been added at the end of the function
  # add monthly mean values to long weather data
#  Weatherl <- Weatherl %>% left_join(x=Weatherl, y=MonthlyMeanRegionalWeather_l, by=c("variable", "Jahr", "Monat"))

  # replace missing values with mean regional values
#  Weatherl$Value <- ifelse(is.na(Weatherl$checkedValue), Weatherl$MonthmeanRegionValue, Weatherl$Value)

  # rain data are not filtered ...
  Weatherl[Weatherl$variable!="NIEDERSCHLAGSHOEHE","Value"] <- Weatherl[Weatherl$variable!="NIEDERSCHLAGSHOEHE","checkedValue"]
  #summary(Weatherl$Value)

  # delete columns with mean values, etc.
  wetter_long <- Weatherl %>% dplyr::select(-RelDiff, -meanRegionValue, -sdRegionalValue, -checkedValue)

  # merge weather data with station data
#  wetter_long <-  merge(x = wetter_long, y = station_selected[,c("Stationsname", "Distance_m")], by="Stationsname", all.x=T)
   wetter_long <-  left_join(x = wetter_long, y = station_selected[,c("Stations_id", "Distance_m")], by="Stations_id")

  # select rain observations of nearest weather station
  Raindata <- wetter_long %>% filter(variable == "NIEDERSCHLAGSHOEHE", !is.na(Value)) %>% dplyr::select(Stationsname, Date, variable, Value, Distance_m) %>%
    group_by(Date, variable) %>% slice(which.min(Distance_m))

  ## retransform selected weather data to wide format
  #ww <- wl %>% select(-Distance_m) %>% pivot_wider(id_cols = Date, names_from = variable, values_from = Value )

  # add Exceltime for later use
  Raindata$ExcelTime <- as.integer(difftime(Raindata$Date, as.Date("1899-12-30")))

  # apply inverse distance weighted average to the obtain the local data except for rainfall
  xport <- wetter_long %>% dplyr::select(Stationsname, Date, Jahr, Monat, variable, Value, Distance_m) %>%
    filter (!is.na(Value)) %>%
    dplyr::group_by(Jahr, Monat, Date, variable) %>%
    dplyr::summarise(sumValue=sum(Value/Distance_m), sum_1_Dist=sum(1/Distance_m)) %>%
    mutate(Value=sumValue/sum_1_Dist) %>%
    ungroup %>% dplyr::select(-sum_1_Dist, -sumValue)

  # transform to wide format
  xportw <- xport %>% pivot_wider(names_from = "variable", values_from = "Value")

  # replace interpolated rain fall data with data from nearest station if not missing

  # merge rain data with weather data
  xportw <- merge(x = xportw, y = Raindata[, c("Date", "Value")])

# use rain data from nearest station if available
  xportw$NIEDERSCHLAGSHOEHE <- ifelse(!is.na(Raindata$Value), Raindata$Value, xportw$NIEDERSCHLAGSHOEHE)

  # remove additional Value column
  xportw$Value <- NULL

  # select colums to be checked for na values
  selcols <- names(xportw)[(!names(xportw) %in% c("Date", "Jahr", "Monat"))]

  # fill missing values with mean values of the month
  xportw <- xportw %>%
    group_by(Jahr, Monat) %>%
    mutate_at(selcols, na.aggregate)

  # add additional time indicator
  xportw$ExcelTime <- as.integer(difftime(xportw$Date, as.Date("1899-12-30")))
  xportHUME <- getHUMEWeather(xportw)

  return(xportHUME)
}





  #' Title InterpolateFromDWD
  #' Interpolation of weather data using the 3 nearest DWD stations
  #' in between a defined difference of altitude
  #'
  #' @param df_DWD_core data frame with DWD weather data
  #' @param stationlist data frame with station information
  #' @param geoBreite Latitude of location
  #' @param geoLaenge Longitude of location
  #' @import dplyr
  #' @import data.table
  #'
  #' @return interpolated weather data in HUME format including radiation data
  #' @export
  #'
  # @examples InterpolateFromDWD(df_DWD_core, stationlist, geoBreite, geoLaenge, max.Height.Distance_m=100, Hoehe_m = 100, df_Rain=NULL, startdate = "1990-01-01")
  InterpolateFromDWD <- function(df_DWD_core, stationlist, geoBreite, geoLaenge, max.Height.Distance_m=100, Hoehe_m = 100, df_Rain=NULL,
                                 startdate = "1990-01-01") {


    df_DWD_core <- df_DWD_core %>% filter(Date >= as.Date(startdate))
    setDT(df_DWD_core)

    # merge the stationlist with the weather data
    stationlist$Distance_km <- as.numeric(stationlist$Distance_km)
    if (!is.null(df_Rain)){
      df_Rain <- df_Rain %>% filter(Date >= as.Date(startdate))
      df_Rain$Distance_km <- as.numeric(df_Rain$Distance_km)
      if(nrow(df_Rain)==0){
        df_Rain <- NULL
      }
    }

   df_DWD_core <- merge(x = df_DWD_core, y = stationlist[,c("Stations_id", "Distance_km", "Stationshoehe")], by = "Stations_id")
#    df_DWD_core <- left_join(x = df_DWD_core,
#                         y = stationlist[,c("Stations_id", "Distance_km", "Stationshoehe")], by = "Stations_id")
    # set to data table format to increase speed

    # calculate the height difference between the location and the station
    df_DWD_core$HeightDifference <- abs(Hoehe_m - df_DWD_core$Stationshoehe)
    # filter for stations with height difference less than max.Height.Distance_m
    df_DWD_core <- df_DWD_core[df_DWD_core$HeightDifference < max.Height.Distance_m,]
    df_DWD_core$HeightDifference <- NULL

    # define the id variables for the melt function
    id_vars <- c("Stations_id", "Date",  "Distance_km")

    # melt the data to long format
    long_data <- data.table::melt(df_DWD_core, id.vars = id_vars, measure.vars = longNames_DWD_core,
                                  variable.name = "variable", value.name = "value")
 #   long_data <- long_data[(!is.na(long_data$value))&(!is.na(long_data$Distance_km)), ]

    if (!is.null(df_Rain) ){
      df_Rain$value <- df_Rain$NIEDERSCHLAGSHOEHE
      df_Rain$variable <- "NIEDERSCHLAGSHOEHE"
#      df_Rain$Date <- as.Date(as.character(df_Rain$MESS_DATUM), format = "%Y%m%d")
      df_Rain <- df_Rain %>% dplyr::select(Stations_id, Date, variable, value, Distance_km) %>% filter(!is.na(value))
      long_data <- rbind(long_data, df_Rain)
    }

    # merge the rain data with the weather data
    long_data$Distance_km <- as.numeric(long_data$Distance_km)
    long_data <- as.data.table(long_data)

    # sort the data by date and distance and variable
    setkey(long_data, Date, Distance_km, variable)
  #  na.omit(long_data, cols = c("value", "Distance_km"))

    # data table approach to select the first three values for each date and variable which are not NA
    result <- long_data[ !is.na(value)&!is.na(Distance_km), head(.SD, 3),by = .(Date, variable)]
    # select the nearest value for the rain data for each date
    rainresult <- long_data[variable == "NIEDERSCHLAGSHOEHE"& !is.na(value)&!is.na(Distance_km), head(.SD, 1), by = .(Date, variable)]
    # remove the rain data from the result
    result <- result[variable != "NIEDERSCHLAGSHOEHE",]
    # combine the rain data with the result
    result <- rbind(result, rainresult)
    if (sum(is.na(result$value)>0)) {
      print("NA values in result")
      print(as.character(geoBreite))
    }
#    result <- long_data %>% group_by(Date, variable) %>%
#      filter(!is.na(value) & !is.na(Distance_km)) %>%
#      slice_head(n = 3)

    # calculate the sum of the inverse distance for each variable
    df.SumInvDist <- result %>% mutate(InvDist = 1/Distance_km)  %>%  group_by(Date, variable) %>%
             dplyr::summarise(SumInvDist=sum(InvDist)) %>%
             ungroup()
    df.InvDist <- result %>% mutate(InvDist = 1/Distance_km) %>% dplyr::select(Date, variable, Stations_id, InvDist)

    df.wf <- merge(x = df.InvDist, y = df.SumInvDist, by = c("Date", "variable")) %>% mutate(wf=InvDist/SumInvDist)


#   %>% dplyr::summarise(Distance_km=mean(Distance_km, na.rm=T), n=n())

#    if(!is.null(df_Rain) ){
      # select the first value for the rain data for each date
#      Raindata <- result[result$variable == "NIEDERSCHLAGSHOEHE",]
#      setkey(Raindata, Date, Distance_km)
      # select the nearest station with rain data
#     Raindata <- Raindata[,head(.SD,1), by = .(Date)]
#    }

    # interpolate according to inverse distance weighting and transform into wide format
    xport <- result %>% dplyr::select(Date, variable, value, Distance_km) %>%
      filter (!is.na(value)) %>%
      dplyr::group_by(variable, Date) %>%
      dplyr::summarise(sumValue=sum(value/Distance_km), sum_1_Dist=sum(1/Distance_km)) %>%
      mutate(value=sumValue/sum_1_Dist) %>%
      ungroup %>% dplyr::select(-sum_1_Dist, -sumValue) %>%
      pivot_wider(names_from = variable, values_from = value) %>% arrange(Date)

    # use rain data from nearest station if available
#    if (!is.null(df_Rain)){
#      xport$NIEDERSCHLAGSHOEHE <- Raindata$value
#    }

    # add the coordinates to the data
    xport$geoBreite <- as.numeric(geoBreite)
    xport$geoLaenge <- as.numeric(geoLaenge)
    xport <- st_as_sf(xport, coords = c("geoLaenge", "geoBreite")) %>%
      st_set_crs(value = "+proj=longlat +datum=WGS84")
    # calculate the radiation data
    df <- getRadWeather(xport)
    # format and rename to HUME-formatted data
    DWDdata <- getHUMEWeather(df)
    output <- list(DWDdata=DWDdata, df.wf=df.wf)
    return(output)
  }



  #' Title InterpolateToFST Interpolates weather data for a list of locations
  #'
  #' @param core_weather data frame with the core DWD data
  #' @param df.locations data frame with the locations for which the weather data should be interpolated
  #' @param DWD_content Meta data for the DWD weather stations
  #' @param DWDRain_content Meta data for the DWD rain stations
  #' @param startdate start date for the selection of the weather data
  #' @param recent logical, if TRUE recent data are used, otherwise historical data
  #' @importFrom sf st_as_sf st_set_crs
  #' @import dplyr
  #'
  #' @return a data frame with the interpolated weather data
  #' @export
  #'
  InterpolateToFST <- function(core_weather, df.locations, DWD_content, DWDRain_content, startdate, recent=TRUE) {



    sites <- df.locations$ID
    df.locations$Stationsname <- df.locations$ID

    starttime <- Sys.time()
    i <- 1
    AllData <- data.frame()
    for (site in sites){
      #  site <- sites[1]
      #  site <- "Nomborn"

      print(paste0(site, "Site Nr.: ",as.character(i), "\n"))
      i <- i + 1
      starttime <- Sys.time()
      print(paste0(site, "\n"))

      geoBreite <- df.locations %>%  filter(ID==site) %>% dplyr::select(Latitude)  #[df$Stationsname=="site", "geoBreite"]
      geoLaenge <- df.locations %>%  filter(ID==site) %>% dplyr::select(Longitude)  #[df$Stationsname=="site", "geoBreite"]
      #Hoehe_m <- df[df$Stationsname==site, "Stationshoehe"]
      Hoehe_m <- as.numeric(df.locations[df.locations$ID==site, "Hoehe_m"])

      Location <- data.frame(Latitude=as.numeric(geoBreite), Longitude=as.numeric(geoLaenge))
      Location <- st_as_sf(Location, coords = c("Longitude", "Latitude")) %>%
        st_set_crs(value = "+proj=longlat +datum=WGS84")

      if (recent == TRUE) {
        stationlist <- DWD_content$recent$stationlist
        RainStationList <- DWDRain_content$recent$stationlist
        ziplist <- DWD_content$recent$ziplist
        RainZiplist <- DWDRain_content$recent$ziplist
        RainRepository <- DWDRain_ftp_recent
      }
      else {
        stationlist <- DWD_content$historical$stationlist
        RainStationList <- DWDRain_content$historical$stationlist
        ziplist <- DWD_content$historical$ziplist
        RainZiplist <- DWDRain_content$historical$ziplist
        RainRepository <- DWDRain_ftp_historical
      }

      # make an sf object from stationlist data frame
      stationlist <- st_as_sf(stationlist, coords = c("geoLaenge", "geoBreite")) %>%
        st_set_crs(value = "+proj=longlat +datum=WGS84")

      stationlist$Distance_m <- pmax(1,as.numeric(st_distance(stationlist, Location))) # minimum distance is 1 m because
      stationlist$Distance_km <- format(stationlist$Distance_m /1000, digits = 3)
      stationlist$geometry <- NULL
      stationlist <- as.data.frame(stationlist)

      # select the nearest station with rain data
      RainStation_selected <- SelectStations (lat=geoBreite,
                                              long=geoLaenge,
                                              height_loc=Hoehe_m,
                                              stationlist = RainStationList,
                                              #DWDRain_content$recent$stationlist,
                                              minstations=3,
                                              max_stations = 3,
                                              radius=50000,
                                              startdate=startdate,
                                              max.Height.Distance_m=200)

      # get the rain data for the selected station
#      df_Rain <- getsingleDWDRain(station=RainStation_selected$Stations_id,
#                                  RainZiplist,
#                                  repository=RainRepository,
#                                  local=F,
#                                  quiet=T)

      DWDRainStationList <- ifelse(recent, DWDRain_content$recent, DWDRain_content$historical)
      df_Rain <- GetRainData_selection (stations_selected=RainStation_selected,
                                        DWDRain_content=DWDRain_content,
                                        repository=RainRepository,
                                        startdate=startdate)
        # add the distance to the rain station to the data frame
      df_Rain <- RenameDWDRain(df_Rain)

      df_Rain <- left_join(x=df_Rain, y=RainStation_selected[,c("Stations_id", "Distance_km")], by="Stations_id")
#      df_Rain$Distance_km <- RainStation_selected$Distance_km

      #
      result <- InterpolateFromDWD(df_DWD_core = core_weather, stationlist = stationlist, geoBreite = geoBreite, geoLaenge = geoLaenge,
                                   max.Height.Distance_m=100, Hoehe_m = Hoehe_m, df_Rain = df_Rain,
                                   startdate = startdate)
      DWDdata <- result$DWDdata

      DWDdata$ID <- site
      DWDdata$Longitude <- geoLaenge$Longitude
      DWDdata$Latitude <- geoBreite$Latitude
      summary(DWDdata)

      AllData <- rbind(AllData, DWDdata)

      endtime <- Sys.time()
      cat("Time for ", site, " : ", endtime - starttime, "/n")

    }

    EndTime <- Sys.time()
    print(paste0("Time for interpolation of data: ", round(EndTime - starttime, digits=2), "\n"))
    return(AllData)

  }




# Analyze/add additional weather parameters ###########################################

#' Title
#'
#' @param df data frame with weather data
#' @import dplyr
#'
#' @return data frame with additional weather parameters
#' CumRain, CumRad, TempSum, PT, cPT, Rn, ra, rc, pETP, cumETP, climWbal, cumWbal
#' @export
#'
  AddWeatherParameters <- function(df, ShiftYears=FALSE, StartMonth=1){
    stopifnot("Time" %in% names(df))
    stopifnot("GlobRad" %in% names(df))
    stopifnot("TMPM" %in% names(df))
    stopifnot("Rain" %in% names(df))
    stopifnot("Wind" %in% names(df))
    stopifnot("VP" %in% names(df))
    stopifnot("Sat_def" %in% names(df))

    # add some derived date variables
    df <- df %>%
      mutate(Datum = as.Date(Time, origin = "1899-12-30"),
             Date = Datum,
             Year = year(Datum),
             Month = month(Datum),
             DOY = yday(Datum))

    # add parameter for calculations of year summation parameters other than the calendar year
    if (ShiftYears) {
      df$SumYear <- ifelse(df$Month >= StartMonth, df$Year+1, df$Year)
    }
    else {
      df$SumYear <- df$Year
    }

    # add derived variables
    df <- df %>%
      mutate(PT = 0,
             PT = ifelse(TMPM > 0 , GlobRad / TMPM, 0), # calculate photo-thermal quotient
             Rn = pmax(0, 0.6494 * (Rad_Int) - 18.417), # calculate net radiation
             ra = ra_f(wind_speed = Wind, crop_height = 0.15, measure_height = 10, f_ra_funct = "Thom-Oliver"), # calculate aerodynamic resistance using the Thom-Oliver method
             rc = rc_f_vectorized(rc0 = 50, LAI = 4), # calculate canopy resistance of a well watered closed crop canopy (rc0=50, LAI=4)
             pETP = Penman(Temp = TMPM, Sat_def = Sat_def, Net_beam = Rn, # calculate potential evapotranspiration
                           delta = delta_f(sat_vap_press = VP, Temp = TMPM),
                           gamma = 1013 * Psycro, l_h_v_water = l_h_v_water,
                           ra = ra, rc = rc),
             climWbal = Rain - pETP # calculate climatic water balance
      )

    # calculate cumulative parameters
    df <- df %>%
      group_by(SumYear) %>%
      mutate(CumRain = cumsum(Rain),
             CumRad = cumsum(GlobRad),
             TempSum = cumsum(pmax(0, TMPM)),
             cumETP = cumsum(pETP),
             cumWbal = cumsum(climWbal),
             cPT = cumsum(PT)) %>%
      ungroup()

    return(df)

  }



  #' @title CreateScenarioWeather
  #' @param df data frame with weather data
  #' @param scenYear A year for which a scenario weather data is created
  #' @param StartMonthSim A month for the start of the simulation scenario
  #' @param startYear A year for the start of the simulation scenario
  #' @param ScenarioStartDay A day for the start of the simulation scenario
  #' @param EndYear A year for the end of the simulation scenario
  #' @param site_key A key for the site
  #' @import dplyr
  #'
  #' @return A data frame with scenario weather data
  #' @export
  #'


  CreateScenarioWeather <- function(df=NULL, scenYear=2024, StartMonthSim=9, startYear=2004, ScenarioStartDay=NULL, EndYear=2024, site_key="L21" ) {

    stopifnot(!is.null(df))
    stopifnot(!is.null(scenYear))

    df$Year <- year(df$Date)
    df$month <- month(df$Date)

    # the start day is located back one year
    if (is.null(ScenarioStartDay)) {
      ScenarioStartDay <- make_date(year= startYear-1, month=StartMonthSim, day=1)
    }

    # the sequence of simulation years
    SimYears <- seq(startYear, EndYear)

    # the weather data frame is filtered for the site key
    df <- df %>% filter(key==site_key) %>% as.data.frame() %>% arrange(Time)

    # the weather data frame is filtered for the start day, i.e.
    # the day when simulation scenario starts
    df <- df %>% filter(Date >= ScenarioStartDay)

    df$SimYear <- ifelse(df$month < StartMonthSim, df$Year, df$Year+1)

    lastTime <- max(df$Time)
    lastDate <- as.Date(lastTime, origin = "1899-12-30")

    # the weather data from the actual simulation year are filtered
    KnownWeather <- df %>% filter(SimYear==scenYear)

    nyears <- length(SimYears)

    # a utility function to replicate the known weather data back to the previous simulation years
    nKnownWeather <- lapply(seq_len(nyears), function(i) {
      df <- KnownWeather
      df$SimYear <- SimYears[i]
      df$Date <- ifelse(df$month < StartMonthSim,
                        as.Date(paste0(as.character(SimYears[i]),"-",format(df$Date, "%m-%d"))),
                        as.Date(paste0(as.character(SimYears[i]-1),"-",format(df$Date, "%m-%d"))))
      return(df)
    })

    # combine the list of data frames
    nKnownWeather <- as.data.frame(data.table::rbindlist(nKnownWeather))
    nKnownWeather <- nKnownWeather %>% filter(!is.na(Date))%>% arrange(Time)
    # transform date column to date format
    nKnownWeather$Date <- as.Date(nKnownWeather$Date)
    # generate a sequence of valid dates without non existing leap dates i.e. 29th of February
    Dateseq <- seq(min(nKnownWeather$Date), max(nKnownWeather$Date), by="days")
    # filter the data frame for the valid dates
    nKnownWeather <- nKnownWeather %>% filter(Date %in% Dateseq)
    # extract vector with known, valid dates
    AllKnownDates <- nKnownWeather$Date
    # filter the data frame for the unknown dates
    df_hist <- df %>% filter(all_of(!(Date %in% AllKnownDates))) %>% arrange(Time)
    # shift the Date of df_hist one year forward as we are in the next simulation year
    # i.e. the data frrom the previous year are used to fill up the actual year
    # towards the end of the simulation
    df_hist$Date <- df_hist$Date + years(1)
    df_hist$SimYear <- df_hist$SimYear + 1
    # combine the known and unknown data
    dfscen <- rbind(df_hist, nKnownWeather)
    FirstYear <- SimYears[1]
    dfscen <- dfscen[dfscen$SimYear > FirstYear,]
    dfscen$Time <- as.numeric(dfscen$Date - as.Date("1899-12-30"))
    dfscen <- dfscen %>% arrange(Time) %>% filter(!is.na(Date))

    return(dfscen)
  }


#' Title
#'
#' @param fn file name of the stationlist file
#' @import dplyr
#'
#' @return a data frame with the forecast data for all station in the list
#' @export
#'
GetForecastDataForStationList <- function(fn) {


  # read the list of location for which weather data should be interpolated
  df.locations <- read_csv(fn)

  #sites <- df$Stationsname
  df.locations <- as.data.frame(df.locations)
  sites <- df.locations$ID
  df.locations$Stationsname <- df.locations$ID

  i <- 1
  AllForeCastData <- data.frame()
  for (site in sites){
    #      site <- sites[1]
    #  site <- "Nomborn"

    cat(site, "Site Nr.",as.character(i), "/n")
    i <- i + 1
    starttime <- Sys.time()
    cat(site, "\n")

    geoBreite <- df.locations %>%  filter(ID==site) %>% dplyr::select(Latitude)  #[df$Stationsname=="site", "geoBreite"]
    geoLaenge <- df.locations %>%  filter(ID==site) %>% dplyr::select(Longitude)  #[df$Stationsname=="site", "geoBreite"]
    #Hoehe_m <- df[df$Stationsname==site, "Stationshoehe"]
    Hoehe_m <- as.numeric(df.locations[df.locations$ID==site, "Hoehe_m"])

    Location <- data.frame(Latitude=as.numeric(geoBreite), Longitude=as.numeric(geoLaenge))
    Location <- st_as_sf(Location, coords = c("Longitude", "Latitude")) %>%
      st_set_crs(value = "+proj=longlat +datum=WGS84")

    # create a location object as numeric vector of Latitude and Longitude
    location <- c(as.numeric(geoBreite), as.numeric(geoLaenge))

    forecast_start <- LatestRecentDate+1
    forecast_end <- as.Date(Sys.Date())+7
    # get the weather forecast data
    forecast <- weather_forecast(
      location = location,
      start = forecast_start,
      end = forecast_end,
      hourly = c("temperature_2m", "shortwave_radiation", "wind_speed_10m", "relative_humidity_2m"),
      daily = c("temperature_2m_max", "temperature_2m_min","precipitation_sum", "shortwave_radiation_sum", "sunshine_duration" ),
      response_units = list(
        temperature_unit = "celsius",
        precipitation_unit = "mm",
        #        wind_unit = "m/s"
        wind_speed_unit = "ms"
      ),
      #      daily = NULL,
      model = NULL,#"icon_d2",#NULL, "icon_eu",#
      timezone = "Europe/Berlin"
    )
    daily_forecast <- forecast[is.na(forecast$time),c("date","daily_temperature_2m_max","daily_temperature_2m_min","daily_precipitation_sum","daily_shortwave_radiation_sum","daily_sunshine_duration")]
    tmp <- forecast %>% filter(!is.na(time)) %>% dplyr::select("date","time","hourly_temperature_2m","hourly_shortwave_radiation","hourly_wind_speed_10m","hourly_relative_humidity_2m") %>%
      mutate(Sat_def = air_saturation_deficit(hourly_temperature_2m, hourly_relative_humidity_2m)) %>%
      mutate(date = as.Date(date)) %>% group_by(date) %>%
      summarise(TMPM = mean(hourly_temperature_2m, na.rm = T),
                Sat_def = mean(Sat_def, na.rm = T),
                LF = mean(hourly_relative_humidity_2m, na.rm = T),
                Wind = mean(hourly_wind_speed_10m, na.rm = T),
                LF = mean(hourly_relative_humidity_2m, na.rm = T))

    daily_forecast <- left_join(daily_forecast, tmp, by = "date")
    #  daily_forecast$Time <-  as.numeric(daily_forecast$date-as.Date("1899-12-30"))
    daily_forecast <- daily_forecast %>% mutate(Date = as.Date(date), Time=as.numeric(Date-as.Date("1899-12-30"))) %>% dplyr::select(-date)
    daily_forecast <- daily_forecast %>% dplyr::rename(TMPMX = daily_temperature_2m_max, TMPMN=daily_temperature_2m_min, Rain=daily_precipitation_sum, GlobRad = daily_shortwave_radiation_sum)
    daily_forecast <- daily_forecast %>%  mutate(VP = 6.1078 * exp(17.269 * TMPM / (TMPM + 237.3)), Sat_def = 6.1078 * exp(17.269 * TMPM / (TMPM + 237.3) - LF / 100 * 17.269 * TMPM / (TMPM + 237.3))) %>%
      dplyr::select(-daily_sunshine_duration, -Date)

    summary(daily_forecast)
    daily_forecast$ID <- site
    daily_forecast$Longitude <- geoLaenge$Longitude
    daily_forecast$Latitude <- geoBreite$Latitude
    daily_forecast$Rad_Int <- daily_forecast$GlobRad * 1e6/86400

    AllForeCastData <- rbind(AllForeCastData, daily_forecast)

    endtime <- Sys.time()
    cat(paste0("Time for ", site, " : ", round(endtime - starttime, digits=2)," seconds", "\n"))
  }
  EndTime <- Sys.time()
  cat(paste("Time for gathering of forecast data:", round(EndTime - starttime, digits=2), " seconds", "\n"))
  return(AllForeCastData)

}



#' WriteHumeWeatherFile
#'
#' @param df data frame with weather data
#' @param fn file name for output
#'
#' @return none, a file is written
#' @export
#'
#' @examples WriteHumeWeatherFile(df, fn)
WriteHumeWeatherFile <- function(df, fn) {
  # Validate file path
  #  if (!file.exists(fn)) {
  #    stop("The specified file path does not exist.")
  #  }
  if (!all(namesHUME %in% names(df))) {
    stop("Not all column names for HUME file in data frame.")
  }


  df <- df[,namesHUME]
  # Datei öffnen
  #    fileHUME <- file(fn, open="wt", encoding="latin1")
  fileHUME <- file(fn, open="wb", encoding="UTF8")
  # Namen schreiben
  write(namesHUME, file = fileHUME, ncolumns = length(unitsHUME), append = FALSE, sep=",")
  # Einheiten schreiben
  write(unitsHUME, file = fileHUME, ncolumns = length(unitsHUME), append = TRUE, sep=",")
  # Daten schreiben
  write.table(df, file=fileHUME, append=TRUE, quote=FALSE, sep=",",#sep="\t",
              eol="\n", dec=".", row.names=FALSE, col.names=FALSE)
  # Datei muss explizit geschlossen werden
  close(fileHUME)
}


