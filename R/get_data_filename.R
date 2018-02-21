#' Gets the filename of a forecast file
#'
#' Works for Operational MEPS. For experments, the experiment directory is
#' currently hard coded. This needs fixing!
#'
#' @importFrom magrittr %>%
#' @param data_date The required forecast date. Maybe YYYYMMDD or YYYY-MM-DD or
#'   any date object.
#' @param data_cycle The forecast cycle. Can be numeric or a string.
#' @param data_source If "operational meps" (the default) will find MEPS data.
#'   Otherwise an experiment is assumed and the path must be given. Not yet
#'   implemented.
#' @param experiment_name The experiment name.
#' @param parameter If the parameter is in a surfex file, for example, it must
#'   be given so that the correct file is found.
#' @param lead_time The lead time must be given in hours for experiments.
#'
#' @return A list containting \code{\strong{fullpath}}, the full path to the
#'   file, \code{\strong{filepath}}, the path to the data file and
#'   \code{\strong{filename}}, the name of the file.
#' @export
#'
#' @examples
#' get_data_filename(20180101, 0)
#' get_data_filename(20180101, 0, parameter = "SST")
#' get_data_filename("2018-01-01", "06")
#' get_data_filename(20180101, 0, data_source = "experiment", experiment_name = "my_exp", lead_time = 24)

get_data_filename <- function(data_date,
															data_cycle,
															data_source     = "operational meps",
															experiment_name = NULL,
															parameter       = NULL,
															lead_time       = NULL) {
#
	data_year  <- lubridate::ymd(data_date) %>% lubridate::year()
	data_month <- lubridate::ymd(data_date) %>% lubridate::month() %>% formatC(width = 2, flag = "0")
	data_day   <- lubridate::ymd(data_date) %>% lubridate::day() %>% formatC(width = 2, flag = "0")
	data_cycle <- formatC(data_cycle, width = 2, flag = "0")
#
	if (tolower(data_source) == "operational meps") {
#
		filepath <- file.path(
			"/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
			data_year,
			data_month,
			data_day
		)
		if (is.null(parameter)) {
		  data_type <- "extracted"
		} else {
		  if (tolower(parameter) %in% c("sst", "tg1", "tg2", "tg3", "wg1", "wg2", "wg3")) {
			  data_type <- "sfx"
		  } else {
			  data_type <- "extracted"
		  }
		}
		filename <- paste0(
			"meps_", data_type, "_2_5km_",
			data_year, data_month, data_day,
			"T", data_cycle, "Z.nc"
		)
#
	} else {
#
		filepath <- file.path(
			"/lustre/storeB/users/andrewts/surfacePerturbations/grib",
			paste0(data_year, data_month, data_day, data_cycle),
			experiment_name
		)
		filename <- paste0(
			"fc",
			data_year, data_month, data_day, data_cycle,
			"+", as.numeric(lead_time) %>% formatC(width = 3, flag = "0"),
			".grib"
		)
#
	}
	list(fullpath = file.path(filepath, filename), filepath = filepath, filename = filename)
}
