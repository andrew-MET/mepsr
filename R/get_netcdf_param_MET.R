#' Get MET Norway NetCDF parameter name from a HARP parameter name
#'
#' @param param HARP parameter name.
#'
#' @return The parameter name in a MET Norway NetCDF file
#' @export
#'
#' @examples
#' get_netcdf_param_MET("T2m")
#' get_netcdf_param_MET("PMSL")
#' get_netcdf_param_MET("S10m")
#' get_netcdf_param_MET("Pcp")

get_netcdf_param_MET <- function (param) {
#
  ncParam <- switch (tolower(param),
                     "t2m"    = "air_temperature_2m",
                     "rh2m"   = "relative_humidity_2m",
                     "cctot"  = "cloud_area_fraction",
                     "cchigh" = "high_type_cloud_area_fraction",
                     "ccmed"  = "medium_type_cloud_area_fraction",
                     "cclow"  = "low_type_cloud_area_fraction",
                     "pmsl"   = "air_pressure_at_sea_level",
                     "s10m"   = "wind_speed",
                     "d10m"   = "wind_direction",
                     "pcp"    = "precipitation_amount_acc",
                     "g10m"   = "wind_speed_of_gust",
                     "tg1"    = "TG1",
                     "tg2"    = "TG2",
                     "tg3"    = "TG3",
                     "wg1"    = "WG1",
                     "wg2"    = "WG2",
                     "wg3"    = "WG3",
                     "fog"    = "fog_area_fraction",
                     "sst"    = "SST",
                     "snow"   = "snowfall_amount_acc",
                     NA)
#
  ncParam
#
}
