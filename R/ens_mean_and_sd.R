#' Compute the ensemble mean of a 2d field
#'
#' @param .ens ensemble data as read in by \code{read_members}.
#'
#' @return a list with the same format as the input but the \code{model_data}
#'   element is the ensemble mean.
#' @export
#'
#' @examples
ensemble_mean <- function(.ens) {
  .ens$model_data <- ens_mean_and_sd(.ens$model_data)$ens_mean
  .ens$member     <- "ensemble_mean"
  .ens
}

#' Compute the ensemble standard deviation of a 2d field
#'
#' @param .ens Ensemble data as read in by \code{read_members}.
#'
#' @return A list with the same format as the input but the \code{model_data}
#'   element is the ensemble mean.
#' @export
#'
#' @examples
ensemble_sd <- function(.ens) {
  .ens$model_data <- ens_mean_and_sd(.ens$model_data)$ens_sd
  .ens$member     <- "ensemble_sd"
  .ens
}
