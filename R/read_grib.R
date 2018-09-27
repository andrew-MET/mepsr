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

  if (!requireNamespace("Rgrib2", quietly = TRUE)) {
    stop("Rgrib2 must be installed to read grib files")
  }

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
    all_levels <- lapply(grib_position, function(x) {cat("."); Rgrib2::Gdec(filename, x)})
    cat("\n")
    # Note: attributes are dropped - wait for new meteogrid for multidimension meteogrid objects.
    # In the meantime return a list with domain information - read_members will only ever ask for one level.
    domain_data     <- meteogrid::DomainExtent(all_levels[[1]])
	  x               <- seq(domain_data$x0, domain_data$x1, domain_data$dx)
	  y               <- seq(domain_data$y0, domain_data$y1, domain_data$dy)
	  proj4_string    <- paste0(
	    "+", paste(
	      meteogrid::proj4.list2str(attr(all_levels[[1]], "domain")$projection), collapse = " +"
      )
	  )
	  list(
	    model_data   = simplify2array(all_levels),
	    x            = x,
	    y            = y,
	    z            = param_info$level_number,
	    member       = NA,
	    proj4_string = proj4_string,
	    parameter    = parameter,
	    filename     = filename,
	    dimensions   = c("x", "y", grib_level_types$description[grib_level_types$id == param_info$level_type])
	  )
  }
}
