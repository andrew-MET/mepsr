#' List of Weather Stations
#'
#' Grib code table 3 giving descriptions of the vertical level type
#' indicatorOfLevelType codes.
#'
#' @format A data frame with 255 rows and 4 variables: \describe{ \item{id}{grib
#'   indicatorOfTypeOfLevel code} \item{description}{a description of the
#'   vertical level type} \item{units1}{The units of the vertical level type.
#'   For layers between two surfaces this gives the top layer} \item{units2}{for
#'   layers between two surfaces this gives the bottom layer}}
#'
#' @source \url{http://apps.ecmwf.int/codes/grib/format/grib1/level/3/}
"grib_level_types"
