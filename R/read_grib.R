#' Read a field from a grib file
#'
#' @importFrom Rgrib2 Gopen Gdec
#' @param filename The grib file name.
#' @param parameter The parameter to read. Standard HARP names are used.
#' @param ... Arguments for \code{get_grib_param_info} for level and levtype.
#'
#' @return A geofield object with 2d array and projection information
#' @export
#'
#' @examples
#' file_name <- "/lustre/storeB/users/andrewts/mepsr_data/grib/fc2017052600+001grib_fp"
#' model_geofield <- read_grib(file_name, "t2m")
#' model_geofield <- read_grib(file_name, "t", level = 500)
#' model_geofield <- read_grib(file_name, "topo")
read_grib <- function(filename, parameter, ...) {

  param_info    <- get_grib_param_info(parameter, ...)
  if (is.na(param_info$short_name)) {
    stop("Unknown parameter: ", parameter)
  }

  grib_info     <- Rgrib2::Gopen(filename)

  grib_position <- grib_info %>%
    dplyr::filter(
      shortName               ==  param_info$short_name,
      indicatorOfParameter    ==  param_info$param_number,
      indicatorOfTypeOfLevel  ==  param_info$level_type,
      level                  %in% param_info$level_number
    )
  if (length(unique(grib_position$shortName)) > 1) stop("More than one shortName found")
  if (length(unique(grib_position$indicatorOfParameter)) > 1) stop("More than one indicatorOfParameter found")
  if (length(unique(grib_position$indicatorOfTypeOfLevel)) > 1) stop("More than one indicatorOfTypeOfLevel found")

  grib_position <- dplyr::pull(grib_position, position)

  if (length(grib_position) == 1) {
    Rgrib2::Gdec(filename, grib_position)
  } else {
    drop(simplify2array(lapply(grib_position, function(x) Rgrib2::Gdec(filename, x))))
    # Note: attributes are dropped - see new meteogrid for multidimension meteogrid objects.
  }
}
