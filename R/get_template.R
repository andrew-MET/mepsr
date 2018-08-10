#' Get File Name Template
#'
#' @param template The file type to generate the template for. Can be
#'   "harmoneps_grib", "harmeoneps_grib_fp", "harmoneps_grib_sfx",
#'   "harmonie_grib", "harmonie_grib_fp", or "harmone_grib_sfx". If anything
#'   else is passed, it is returned unmodified.
#'
#' @return A template that can be interpreted by \code{str_interp} from the
#'   \code{stringr} package.
#' @export
#'
#' @examples
#' get_template("harmoneps_grib")
#' get_template("harmonie_grib_fp")
#'
get_template <- function(template) {
  template <- switch(template,
    "harmoneps_grib" = file.path(
		  "${static_path}",
      "${YYYY}","${MM}","${DD}","${HH}",
      "mbr${MBR3}",
      "fc${YYYY}${MM}${DD}_${HH}+${LDT3}_grib"
		),
		"harmoneps_grib_fp" = file.path(
		  "${static_path}",
      "${YYYY}","${MM}","${DD}","${HH}",
      "mbr${MBR3}",
      "fc${YYYY}${MM}${DD}_${HH}+${LDT3}_grib_fp"
		),
		"harmoneps_grib_sfx" = file.path(
		  "${static_path}",
      "${YYYY}","${MM}","${DD}","${HH}",
      "mbr${MBR3}",
      "fc${YYYY}${MM}${DD}_${HH}+${LDT3}_grib_sfx"
		),
		"harmonie_grib" = file.path(
		  "${static_path}",
      "${YYYY}","${MM}","${DD}","${HH}",
      "fc${YYYY}${MM}${DD}_${HH}+${LDT3}_grib"
		),
		"harmonie_grib_fp" =file.path(
		  "${static_path}",
      "${YYYY}","${MM}","${DD}","${HH}",
      "fc${YYYY}${MM}${DD}_${HH}+${LDT3}_grib_fp"
		),
		"harmonie_grib_sfx" = file.path(
		  "${static_path}",
      "${YYYY}","${MM}","${DD}","${HH}",
      "fc${YYYY}${MM}${DD}_${HH}+${LDT3}_grib_sfx"
		),
    "ifsens_old" = file.path(
      "${static_path}",
      "${YYYY}",
      "eps25_${YYYY}${MM}${DD}${HH}Z.nc"
    ),
    "ifsens" = file.path(
      "${static_path}",
      "${YYYY}",
      "ec_ens_${YYYY}${MM}${DD}T${HH}Z.nc"
    ),
    "ifsens_temp" = file.path(
      "${static_path}",
      "eps25_${YYYY}${MM}${DD}${HH}Z.nc"
    ),
    file.path("${static_path}", template)
  )
  template
}
