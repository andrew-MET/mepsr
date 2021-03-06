# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Compute the ensemble mean and standard deviation
#'
#' @param ens A 3d array with the third dimension as the ensemnble member
#'
#' @return A list with \code{ens_sd}, the ensemble standard deviation and
#'   \code{ens_mean}, the ensemble mean
#'
#' @examples
#' my_file <- get_data_filename(20180101, 0)
#' t2m <- read_members(my_file, "T2m", lead_time = 0)
#' my_mean_and_sd <- ens_mean_and_sd(t2m$model_data)
#' @export
ens_mean_and_sd <- function(ens) {
    .Call('_mepsr_ens_mean_and_sd', PACKAGE = 'mepsr', ens)
}

