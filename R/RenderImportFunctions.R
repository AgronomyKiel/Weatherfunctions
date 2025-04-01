
# Import functions with rendering ######################################################


#' render_Wetter_Analysis
#'
#' @param Standort name of the site for which weather data should be estimated
#' @param geoBreite latitude of the site
#' @param geoLaenge longitude of the site
#' @param Hoehe_m height above sea level of the site
#' @param radius radius from which DWD weather stations should be included
#' @param minstations minimum number of stations to be included
#' @param startdate start date (string, format "1990-01-01")
#' @param maxError maximum deviation (%) of the values of a single weather station from the average of all stations
#' @param set_title title of the output file
#' @param out_file name of the output file
#' @import rmarkdown
#'
#' @return none, a html file is generated
#' @export
#'
#' @examples render_Wetter_Analysis(Standort, geoBreite, geoLaenge, Hoehe_m, radius, minstations, startdate, maxError, set_title, out_file)
render_Wetter_Analysis = function(Standort, geoBreite, geoLaenge, Hoehe_m, radius, minstations, startdate, maxError, set_title, out_file) {

  ScriptFN <- paste0(localpath, "sources/", "AnalyzeWeatherClimate.rmd")
  rmarkdown::render(input = ScriptFN,
                    params = list(
                      Standort = Standort,
                      set_title=set_title
                    ),
                    output_file = out_file, output_format = "html_document" #,  envir = new.env()
  )
}

#' render_Wetter_Import
#'
#' @param Standort name of the site for which weather data should be estimated (string)
#' @param geoBreite latitude of the site (numeric)
#' @param geoLaenge longitude of the site (numeric)
#' @param Hoehe_m height above sea level of the site (numeric)
#' @param radius radius from which DWD weather stations should be included (numeric)
#' @param minstations minimum number of stations to be included (numeric)
#' @param startdate start date (string, format "1990-01-01")
#' @param maxError maximum deviation (%) of the values of a single weather station from the average of all stations
#' @param set_title title of the output file
#' @param out_file name of the output file
#' @import rmarkdown
#'
#' @return none, a html file is generated
#' @export
#'
#' @examples render_Wetter_Import(Standort, geoBreite, geoLaenge, Hoehe_m, radius, minstations, startdate, maxError, set_title, out_file)
#render_Wetter_Import = function(Standort, geoBreite, geoLaenge, Hoehe_m, radius,
#                                minstations, startdate, maxError, set_title, out_file)

render_Wetter_Import = function(Standort, geoBreite, geoLaenge, Hoehe_m, radius,
                                minstations, startyear, endyear, maxError, set_title)


{

  ScriptFN <- paste0(localpath, "DWD_Wetter_Import_new2.rmd")
  rmarkdown::render(input = ScriptFN,
                    params = list(
                      Standort = Standort,
                      geoBreite = geoBreite,
                      geoLaenge = geoLaenge,
                      Hoehe_m = Hoehe_m,
                      radius = radius,
                      minstations = minstations,
                      startyear = startyear,
                      endyear = endyear,
                      maxError = maxError,
                      set_title=set_title
                    ),
                    output_file = out_file, output_format = "html_document" #,  envir = new.env()
  )
}


