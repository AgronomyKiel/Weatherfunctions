# some constants

l_h_v_water <- 2.477 * 1E6 # latente Verdunstungsenergie von Wasser bei bei 10 øC in [J/Kg] }
Psycro      <- 0.000662    # { Psychrometerkonstante [1/øK] }
Karman_const <- 0.41       # { von Karman-Konstante [-] }


#' Title roughness_f
#'
#' @param crop_height
#'
#' @return roughness_f
#' @export
#'
#' @examples roughness_f(0.1)
roughness_f <- function(crop_height)
  #{ ********************************************************************** }
  #{ Zweck : empirische Funktion zur Ermittlung des Rauigkeitsfaktors
  #  nach Monteith (1973) S.90
  #  Parameter :
  #    Name             Inhalt                          Einheit      Typ
  #
  #  crop_height      Pflanzenhöhe                    [m]         I
  #  roughness_f      Rauhigkeitsfaktor               [m]         O }
  #{ ********************************************************************** }
{
  crop_height <- pmax( 0.05 ,crop_height)       ## Mindesthöhe von 5 cm
  roughness_f <- 0.13 * crop_height
}


#' Function displacement_height (zero plane displacement height)
#'
#' @param crop_height
#'
#' @return displacement_height [m]
#' @export
#'
#' @examples displacement_height(0.1)
displacement_height <- function ( crop_height  )
{
  crop_height <- pmax(0.05, crop_height) # Mindesthöhe von 5 cm
  displacement_height <- 0.63 * crop_height
}


#' ra_f function for calculating aerodynamic resistance
#'
#' @param wind_speed [m]
#' @param crop_height [m]
#' @param measure_height [m]
#' @param f_ra_funct [character] "Monteith-Unsworth" or 'Thom-Oliver'
#'
#' @return aerodynamic resistance [s/m]
#' @export
#'
#' @examples ra_f(1, 0.1, 2, "Monteith-Unsworth")
ra_f <- function(wind_speed, crop_height, measure_height, f_ra_funct = "Monteith-Unsworth")

  #{ ********************************************************************** }
  #{ Zweck : Berechnung des aerodynamischen Widerstandes
  #  Parameter :
  #    Name             Inhalt                          Einheit      Typ
  #
  #  wind_speed       Mittlere Windgeschwindigkeit    [m/s]        I
  #  crop_height      Pflanzenhöhe                    [m]          I
  #
  #  ra_f             aerodynamischer Widerstand      [s/m] }
  #{ ********************************************************************** }

{
  # avoid negative wind speeds
  wind_speed <- pmax(0.0001, wind_speed)

  # roughness length
  z0 <- roughness_f(crop_height)

  # displacement height
  d  <- displacement_height(crop_height)

  # check for Penman-Monteith
  isMonteith <- (f_ra_funct == "Monteith-Unsworth")

  # adjust length of isMonteith to length of wind_speed for vectorized calculation
  isMonteith <- rep(isMonteith, length(wind_speed))

  # aerodynamic resistance
  ra <- ifelse (isMonteith,
                #Original-Penman-Monteith für "near-neutral conditions"
                (log(measure_height/z0)*log(measure_height/(0.2*z0)))/((Karman_const^2)*wind_speed),
                #Formulierung zur Einbeziehung von Konvektion nach Thom and Oliver (1977)  zitiert in Jackson et al. 1988}
                (4.72*(log((measure_height-d)/z0))^2)/(1+0.54*wind_speed))
  return(ra)
}



#' dens_air function for calculating air density [Kg/m3]
#'
#' @param Temp [°C]
#'
#' @return air density [Kg/m3]
#' @export
#'
#' @examples dens_air(20)
dens_air <- function (Temp)

  # ********************************************************************** }
  # Zweck : empirische Funktion zur Ermittlung der Dichte trockener Luft
  #   aus Monteith (1973)

  #  Parameter :
  #    Name             Inhalt                          Einheit      Typ

  #  Temp             Mittlere Tagestemperatur        [øC]         I
  #  dens_air         Dichte der Luft                 [Kg/m3]      O }
  # ********************************************************************** }

{
  dens_air <- 1.2917 - 0.00434 * Temp
}

# ----------------------------------------------------------------------- }
#' Penman function for calculating potential evapotranspiration
#'
#' @param Temp [°C]
#' @param Sat_def [mbar] or [hPa]
#' @param Net_beam [J/m2*s]
#' @param delta [mbar/K]
#' @param gamma [mbar/K]
#' @param l_h_v_water [J/Kg]
#' @param ra aerodynamic resistance [s/m]
#' @param rc bulk-canopy resistance [s/m]
#'
#' @return potential evapotranspiration [mm/d]
#' @export
#'
#' @examples Penman(20, 10, 100, 0.1, 0.1, 2.477 * 1E6, 10, 10)
Penman <- function (Temp, Sat_def, Net_beam, delta, gamma,
                    l_h_v_water, ra, rc)

  # **********************************************************************
  #    **********************        Penman           ***********************
  #    **********************************************************************

  #  Parameter :
  #  Name             Inhalt                          Einheit      Typ

  #  Temp             Lufttemperatur                  [øC]         I
  #  Sat_def          Sättigungsdefizit der Luft      [mbar]       I
  #  Net_beam         Nettostrahlung                  [J/m2*s]     I
#  delta            Steigung der S„ttigungs-
#  dampfdruckkurve                 [mbar/K]     I
#  gamma            Psychrometerkonstante           [mbar/K]     I
#  l_h_v_water      latente Verdunstungswärme von Wasser bei 10øC             [J/Kg]       I
#  ra               Grenzflächenwiderstand          [s/m]        I
#  rc               bulk-Stomatawiderstand          [s/m]        I

#  Penman           potentielle Evapotranspiration  [kg/(m2*s)]  O

#  **********************************************************************
#    **********************************************************************
#    ********************************************************************** }


{ # Penman
  # spezifische Wärme der Luft [J/(Kg*K)]
  cp <- 1005.0
  rho <- dens_air(Temp)
  # potential evapotranspiration
  pETP <- (delta * Net_beam + rho * cp * Sat_def / ra) / (delta + gamma * (1 + rc / ra))
  pETP <- pETP / l_h_v_water * 86400.0
  Penman <- pmax(0.0, pETP)
  return(Penman)
} # Penman }
# ----------------------------------------------------------------------- }




# ----------------------------------------------------------------------- }

#' Title sat_vap_press_f
#'
#' @param Temp
#'
#' @return sat_vap_press_f [mbar]
#' @export
#'
#' @examples sat_vap_press_f(20)
sat_vap_press_f <- function (Temp)

  # ********************************************************************** }
  # Zweck : empirische Funktion zur Ermittlung des gesättigten Wasserdampf-
  #    druckes
  #  nach Groot (1983) bzw. Goudriaan (1977)

  #  Parameter :

  #  Name             Inhalt                          Einheit      Typ

  #  Temp             Temperatur                      [øC]         I
#  sat_vap_press_f  gesättigter Wasserdampfdruck    [mbar]       O }
# ********************************************************************** }

{
  sat_vap_press_f <- 6.11 * exp(17.4 * Temp / (Temp + 239.0))
}
# ----------------------------------------------------------------------- }



#' Title
#'
#' @param temperature air temperature [°C]
#' @param relative_humidity relative humidity [%]
#'
#'#' @export
#'
air_saturation_deficit <- function(temperature, relative_humidity){

  # saturation vapour pressure [mbar]
  svp <- sat_vap_press_f(temperature)

  # actual vapour pressure [mbar]
  avp <- svp * relative_humidity / 100.0

  # air saturation deficit [mbar]
  air_saturation_deficit <- svp - avp
  return(air_saturation_deficit)

}



# ----------------------------------------------------------------------- }


#' Title delta_f
#'
#' @param sat_vap_press saturated vapour pressure [mbar]
#' @param Temp Temperature [°C]
#'
#' @return delta_f [mbar/K]
#' @export
#'
#' @examples delta_f(20, 10)
delta_f <- function (sat_vap_press, Temp)

  # ********************************************************************** }
  # Zweck : empirische Funktion zur Ermittlung Steigung der Wasserdampf-
  #    druckkurve in Abh„ngigkeit von ges„ttigtem Wasserdampfdruck
  #  und Temperatur
  #  nach Groot (1983)

  #  Parameter :

  #    Name             Inhalt                          Einheit      Typ

#  Temp             Temperatur                      [øC]         I
#  sat_vap_press    ges„ttigter Wasserdampfdruck    [mbar]       I

#  delta_f          Steigung der Wasserdampdruck-
#    kurve                            [mbar/øK]   O }
# ********************************************************************** }

{
  delta_f <- 239.0 * 17.4 * sat_vap_press / ((Temp + 239.0)^2)
}
# ----------------------------------------------------------------------- }
# ----------------------------------------------------------------------- }


#' Title rc_f
#'
#' @param rc0 canopy resistance under well watered condition [s/m]
#' @param LAI leaf area index [-]
#'
#' @return canopy resistance rc_f [s/m]
#' @export
#'
#' @examples rc_f(50, 4)
rc_f <- function (rc0, LAI){

  rc <- NA
  if (LAI < 1.0) { (rc <- rc0)}

  if ((LAI >= 1.0) && (LAI < 2))
    {(rc <- rc0 / LAI)}

  if ((LAI >= 2.0) && (LAI < 6))
    {rc <- rc0 / 2 - (rc0/2-rc0/3)*((LAI - 2) / 4)}
  #         rc0 / 2 - (rc0/2-rc0/3)*((LAI - 2) / 4)
  if (is.na(rc))
  {(rc <- (rc0/3))}

  if (rc < 0.1)  {(rc <- 0.1)}

  rc_f <- rc
}



rc_f_vectorized <- function (rc0, LAI){

  # lengthen rc0 to length of LAI
  rc <- rep(NA, length(LAI))

  rc[LAI < 1.0] <- rc0

  rc[(LAI >= 1.0) & (LAI < 2)] <- rc0 / LAI[(LAI >= 1.0) & (LAI < 2)]

  rc[(LAI >= 2.0) & (LAI < 6)] <- rc0 / 2 - (rc0/2-rc0/3)*(LAI[(LAI >= 2.0) & (LAI < 6)] - 2) / 4

  rc[(LAI >= 6)] <- rc0 / 3

  # set minimum value
  rc <- pmax(rc, 0.1)

  return(rc)
}



TCanopyDiff <- function (Temp, Sat_def, Net_beam, ra, rc)

{
  # spezifische Wärme der Luft [J/(Kg*K)]
  cp <- 1005.0
  Psycro      <- 0.000662    #  Psychrometerkonstante [1/°K]
  rho <- dens_air(Temp)
  Icl <- 0.9
  gamma <- cp * Psycro
  svp <- sat_vap_press_f(Temp)
  delta <- delta_f(svp, Temp)
  x <- gamma*(1+rc/ra)
  TCanopyDiff <- (ra*Icl*Net_beam)/(rho*cp)*(x/(delta+x))-(Sat_def/(delta+x))
}



#' Title TmaxCanopy
#'
#' @param Tair Air temperature [°C]
#' @param ra aerodynamic resistance [s/m]
#' @param Rn Net radiation [W/m2]
#'
#' @return TmaxCanopy [°C]
#' @export
#'
#' @examples TmaxCanopy(20, 100, 600)
TmaxCanopy <- function (Tair, ra, Rn)
{
  cp <- 1005.0 # spezifische Wärme der Luft [J/(Kg*K)]
  Icl <- 0.9 #radiation interception coefficients for the upper  limits, reducing RNet by the #soil heat flux,
  #set to  0.9 according to Jackson et al. (1988)
  rho <- dens_air(Tair)
  TmaxCanopy <- Tair+ (ra*Rn*Icl)/(rho*cp)
}



#' Title rs_Ta_f
#' function for the contribution of air temperature to stomatal resistance
#' according to Neukam et al. (2018)
#'
#' @param Ta air temperature  [°C]
#' @param rs_min minimum stomatal conductance [s/m]
#'
#' @return rs_Ta_f [s/m]
#' @export
#'
#' @examples rs_Ta_f(20, 10)
rs_Ta_f <-  function  (Ta, rs_min)

{
  Topt   <- 25    #°C
  h      <- 0.4275 #s·m-1·K-2
  r_s_Ta_f <- rs_min+h*(Topt-Ta)^2
}




#' Title rs_Rnet_f
#' function for the contribution of net radiation to stomatal resistance
#' according to Neukam et al. (2018)
#'
#' @param Rnet net radiation [W/m2]
#' @param rs_min minimum stomatal conductance [s/m]
#'
#' @return rs_Rnet_f [s/m]
#' @export
#'
#' @examples rs_Rnet_f(100, 10)
rs_Rnet_f <- function (Rnet, rs_min)

{
  f     <- 0.0005 # s·m3·W-2
  Rcrit <- 533    #W∙m-2
  ifelse(Rnet<Rcrit, rs_min+f*(Rcrit-Rnet)^2,rs_min)
}

#' Title rs_VPD_psi_r_f
#' function for the contribution of VPD and soil water potential to stomatal resistance
#' according to Neukam et al. (2018)
#'
#' @param VPD vapour pressure deficit [hPa]
#' @param psi_r weighted average soil water potential in rooted soil [MPa]
#' @param rs_min minimum stomatal conductance [s/m]
#'
#' @return rs_VPD_psi_r_f [s/m]
#' @export
#'
#' @examples rs_VPD_psi_r_f(10, 0.1, 10)
rs_VPD_psi_r_f <- function (VPD, psi_r, rs_min)

{
  psi_FC  <- -6.3*10^-3 # MPa
  a       <- 124        #s·m-1·MPa-1
  b       <- 9.4        #s·m-1·hPa-1∙MPa-1
  rs_VPD_psi_r_f <- rs_min-(a+b*VPD)*(psi_FC-psi_r)
}


#' Title rs_f
#' function for the stomatal resistance as influenced by
#' air temperature, net radiation, vapour pressure deficit and soil water potential
#' according to Neukam et al. (2018)
#'
#' @param Ta air temperature  [°C]
#' @param Rnet net radiation [W/m2]
#' @param VPD vapour pressure deficit [hPa]
#' @param psi_r weighted average soil water potential in rooted soil [MPa]
#' @param rs_min minimum stomatal conductance [s/m]
#'
#' @return actual stomata resistance rs_f [s/m]
#' @export
#'
#' @examples rs_f(20, 100, 10, 0.1, 10)
rs_f <- function (Ta, Rnet, VPD, psi_r, rs_min)

  # Ta  air temperature [°C]
  # Rnet net Radiation [W.m-2]
  # VPD vapour pressure deficit [hPa]
  # psi_r weighted average soil water potential in rooted soil [MPa]

{
  rs_Ta <- rs_Ta_f(Ta, rs_min)
  rs_Rnet <- rs_Rnet_f(Rnet, rs_min)
  rs_VPD_psi_r <- rs_VPD_psi_r_f(VPD,psi_r,rs_min)
  rs_f <- max(rs_Ta, rs_Rnet, rs_VPD_psi_r)
}

#######################
