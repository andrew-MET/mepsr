#' @useDynLib mepsr
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"
.onUnload <- function (libpath) {
  library.dynam.unload("mepsr", libpath)
}

