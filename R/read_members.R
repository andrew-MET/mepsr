#' Read ensemble members from a forecast file
#'
#' \code{read_members} reads all esemble members from a forecast file for a
#' single lead time. It is possible to read from grib or netcdf files via the
#' helper functions. It is assumed that grib files only contain one member and
#' ntcdf files contain all members.
#'
#' @param filepath The path to the forecast file.
#' @param filename The forecast file name.
#' @param num_perturbed_members The number of perturbed members in the forecast.
#'   Note that this does not include the control member.
#' @param parameter The parameter to read.
#' @param file_type The forecast file format. The function can attempt to
#'   ascertain the format from the file name, but if it can't \code{file_type}
#'   must be passed as an argument.
#' @param lead_time The lead time to read.
#' @param ... Arguments to be passed to \code{read_netcf} or \code{read_grib}
#'
#' @return A list containing: \cr
#' \code{\strong{model_data}}: The 3d field (the 3rd dimension is ensemble member). \cr
#' \code{\strong{x}}: The x coordinates in the projection of the forecast file. \cr
#' \code{\strong{y}}: The y coordinates in the projection of the forecast file. \cr
#' \code{\strong{proj4_string}}: The proj4 projection string for the forecast file. \cr
#' \code{\strong{parameter}}: The parameter that the 3d field represents. \cr
#' \code{\strong{filename}}: The full path to the forecast file.
#' @export
#'
#' @examples
#' file_path <- "/lustre/storeB/immutable/archive/projects/metproduction/MEPS/2018/01/01"
#' file_name <- "meps_extracted_2_5km_20180101T00Z.nc"
#' model_field <- read_members(file_path, file_name, 9, "precipitation_amount_acc", lead_time = 24)
#' model_field <- read_members(file_path, file_name, 9, "Pcp", lead_time = 24)

read_members <- function(filepath,
                         filename,
                         num_perturbed_members,
                         parameter,
                         file_type = NULL,
                         lead_time = NULL,
                         ...)  {
#
# read control and get domain info
#
	if (is.null(file_type)) {
		file_type <- tolower(tools::file_ext(filename))
		if (! file_type %in% c("grib", "grb", "nc", "nc4", "netcdf")) {
			stop("Unable to ascertain file type. Call the function with file_type = '<file_type>'",
				call. = FALSE
			)
		} else {
			file_type <- switch(
				file_type,
				"grb" = "grib",
				"nc"  = "netcdf",
				"nc4" = "netcdf",
				file_type
			)
		}
	}
#
	if (tolower(file_type) == "grib") {
#
    if (!requireNamespace("Rgrib2", quietly = TRUE)) {
  		stop("Package Rgrib2 required for read_members() - you can get it from HARP",
  			call. = FALSE
  		)
  	}
#
	  model_file      <- file.path(filepath, "mbr000", filename)
	  geofield_data   <- read_grib(model_file, parameter)
	  domain_data     <- geogrid::DomainExtent(geofield_data)
	  x               <- seq(domain_data$x0, domain_data$x1, domain_data$dx)
	  y               <- seq(domain_data$y0, domain_data$y1, domain_data$dy)
	  proj4_string    <- paste0(
	    "+", paste(
	      geogrid::proj4.list2str(attr(geofield_data, "domain")$projection), collapse = " +"
      )
	  )
	  data_all        <- array(NA, c(dim(geofield_data), num_perturbed_members + 1))
	  data_all[, , 1] <- geofield_data
#
	} else if (tolower(file_type) == "netcdf") {
#
		if (!requireNamespace("ncdf4", quietly = TRUE)) {
			stop("Package ncdf4 required for read_members() - Please install from CRAN",
				call. = FALSE
			)
		}
#
		if (is.null(lead_time)) stop("lead_time must be supplied for NetCDF data")
		model_file      <- file.path(filepath, filename)
		ncID            <- ncdf4::nc_open(model_file)
		x               <- ncdf4::ncvar_get(ncID, "x")
		y               <- ncdf4::ncvar_get(ncID, "y")
		proj4_string    <- ncdf4::ncatt_get(ncID, "projection_lambert", "proj4")$value
		num_members     <- length(ncdf4::ncvar_get(ncID, "ensemble_member"))
		ncdf4::nc_close(ncID)
		if (num_members < (num_perturbed_members + 1)) {
			cat("\nWARNING: Number of perturbed members in file   =", num_members - 1)
			cat("\n         Number of perturbed members requested =", num_perturbed_members)
			cat("\n         Setting number of perturbed members to ", num_members - 1)
			cat("\n")
			num_perturbed_members <- num_members - 1
		}
		data_all        <- array(NA, c(length(x), length(y), num_perturbed_members + 1))
		data_all[, , 1] <- read_netcdf(model_file, parameter, 0, lead_time, ...)
#
	} else {
		stop("Unknown file type ", file_type, ". Can only deal with netcdf or grib",
			call. = FALSE
		)
	}
#
# Get perturbed members
#
	cat("Reading members ")
	for (member in 1:num_perturbed_members) {
		cat(".")
  	member_name <- paste0("mbr", formatC(member, width = 3, flag = "0"))
  	if (file_type == "grib") {
			model_file               <- file.path(filepath, member_name, filename)
  		data_all[, , member + 1] <- read_grib(model_file, parameter)
  	} else if (file_type == "netcdf") {
  		data_all[, , member + 1] <- read_netcdf(model_file, parameter, member, lead_time, ...)
  	}
	}
	cat("\n")
#
# Convert units - it is assumed that when geopotential is requested, geopential
# height is what is wanted

	is_temperature <- function(x) {
	  tolower(x) %in% c("t", "t2m", "sst") | stringr::str_detect(x, "temperature")
	}
	is_pressure <- function(x) {
	  tolower(x) == "pmsl" | stringr::str_detect(x, "pressure")
	}
	is_geopotential <- function(x) {
	  tolower(x) %in% c("z0m", "z") | stringr::str_detect(x, "geopotential")
	}

	if (is_temperature(parameter) & min(data_all, na.rm = TRUE) > 200) {
		data_all <- data_all - 273.15
	}
	if (is_pressure(parameter) & min(data_all, na.rm = TRUE) > 2000) {
		data_all <- data_all/100
	}
	if (is_geopotential(parameter)) {
		data_all <- data_all/9.80665
	}

#
	list(
		model_data   = data_all,
		x            = x,
		y            = y,
		member       = seq(0, num_perturbed_members),
		proj4_string = proj4_string,
		parameter    = parameter,
		filename     = model_file
	)
}
