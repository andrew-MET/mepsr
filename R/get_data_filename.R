#' Gets the filename of a forecast file
#'
#' Works for Operational MEPS. For experments, the experiment directory is
#' currently hard coded. This needs fixing!
#'
#' @importFrom magrittr %>%
#'
#' @param data_date The required forecast date. Maybe YYYYMMDD or YYYY-MM-DD or
#'   any date object.
#' @param data_cycle The forecast cycle. Can be numeric or a string.
#' @param data_source If "operational meps" (the default) will find MEPS data.
#'   Otherwise "expt" must be used and \code{template} should be specified.
#' @param experiment_name The experiment name - can be used in \code{template}.
#' @param parameter If the parameter is in a surfex file, for example, it must
#'   be given so that the correct file is found. Otherwise an extracted file is
#'   assumed.
#' @param lead_time The lead time must be given in hours for NetCDF. If
#'   \code{template} contains ${LDTx} lead_time must be specified.
#' @param members The members to get filenames for if \code{template} contains
#'   ${MBRx}.
#' @param static_path The static path for filenames. The full path is assumed to
#'   be \code{static_path/template}.
#' @param template A template for filenames. Standard templates are
#'   "harmoneps_grib", "harmoneps_grib_fp", "haramoneps_grib_sfx", and the same
#'   with "harmonie" in place of "harmoneps" for deterministic files. \cr
#'   Otherwise, a template can be given with substitutions expressed as
#'   ${substitution}. Available substitutions are ${YYYY}, ${MM}, ${DD}, ${HH},
#'   ${LDTx}, ${MBRx}, where x is the number of charcters in the string with
#'   leading zeros added. YYYY = year (4 digits), MM = month (2 digits with
#'   leading zero if < 10), DD = day (2 digits with leading zero if < 10), HH =
#'   hour (2 digits with leading zero if < 10), LDT = lead time, MBR = ensemble
#'   member. ${static_path} and ${experiment_name} can also be used if
#'   specified.
#'
#' @return The full path(s) to the forecast file(s).
#' @export
#'
#' @examples
#' get_data_filename(20180101, 0)
#' get_data_filename(20180101, 0, parameter = "SST")
#' get_data_filename("2018-01-01", "06")
#'
#' my_static_path <- "/lustre/storeB/users/andrewts/surfacePerturbations/grib"
#' my_expt <- "MEPS_summer2017_sfcPertRef"
#' my_template <- file.path(
#'   "${YYYY}${MM}${DD}${HH}",
#'   "${experiment_name}",
#'   "mbr${MBR3}/fc${YYYY}${MM}${DD}${HH}+${LDT3}.grib"
#' )
#' get_data_filename(
#'   20170527, 0, data_source = "expt",
#'   static_path = my_static_path,
#'   experiment_name = my_expt,
#'   template = my_template,
#'   lead_time = 3, members = seq(0, 10)
#' )


get_data_filename <- function(
  data_date,
	data_cycle,
	data_source     = "operational meps",
	experiment_name = NULL,
	parameter       = NULL,
	lead_time       = NULL,
  members         = seq(0, 9),
  static_path     = "",
  template        = "harmoneps_grib"
) {
#
	YYYY <- lubridate::ymd(data_date) %>% lubridate::year()
  MM   <- lubridate::ymd(data_date) %>% lubridate::month() %>% formatC(width = 2, flag = "0")
	DD   <- lubridate::ymd(data_date) %>% lubridate::day() %>% formatC(width = 2, flag = "0")
	HH   <- formatC(data_cycle, width = 2, flag = "0")
#
	if (tolower(data_source) == "operational meps") {
#
		filepath <- file.path(
			"/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
			YYYY,
			MM,
			DD
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
			YYYY, MM, DD,
			"T", HH, "Z.nc"
		)
		fullpath <- file.path(filepath, filename)
#
	} else if (tolower(data_source) %in% c("experiment", "expt", "exp")) {
#
		fullpath <- list()
	  template <- get_template(template)

		if (stringr::str_detect(template, "LDT")) {
		  if (is.null(lead_time)) {
		    stop("Template includes LDT but lead_time not supplied")
		  }
		  LDT  <- as.character(lead_time)
		  LDT1 <- LDT
		  LDT2 <- formatC(lead_time, width = 2, flag = "0")
		  LDT3 <- formatC(lead_time, width = 3, flag = "0")
		  LDT4 <- formatC(lead_time, width = 4, flag = "0")
		}

		if (stringr::str_detect(template, "MBR")) {

		  count <- 0
		  for (member in members) {
        count <- count + 1
		    MBR   <- as.character(member)
        MBR1  <- MBR
        MBR2  <- formatC(member, width = 2, flag = "0")
        MBR3  <- formatC(member, width = 3, flag = "0")
        MBR4  <- formatC(member, width = 4, flag = "0")

        fullpath[[count]] <- stringr::str_interp(string = template)
		  }

		} else {
		  fullpath <- stringr::str_interp(string = template)
		}

	} else {
	  stop("Unknown data_source: ", data_source, ".\nAcceptable values are 'operational meps' or 'expt'")
	}

	unlist(fullpath)

}
