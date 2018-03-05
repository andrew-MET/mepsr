#' Plot a field from an eps model
#'
#' Uses the output of \code{read_members} to quickly plot a field with a
#' coastline map
#'
#' @importFrom raster crop extent
#' @importFrom sp spTransform plot
#' @importFrom graphics lines
#'
#' @param eps_field Output of \code{read_members}.
#' @param member Member to plot. Can be numeric or one of "mean", "sd".
#' @param legend Whether to plot a legend - TRUE or FALSE. Defaults to TRUE.
#' @param hires_coast Plot a high resolution coastline - TRUE or FALSE. Defaults
#'   to FALSE.
#' @param ... Arguments for \code{image.plot}, e.g. for breaks and colour
#'   palette.
#'
#' @return A plot
#' @export
#'
#' @examples
#' my_file <- get_data_filename(20180101, 0)
#' t2m <- read_members(my_file$filepath, my_file$filename, 9, "T2m", lead_time = 0)
#' quick_plot_field(t2m, 0)
#'
#' breaks <- c(-50, -40, - 30, seq(-20, 15, 5))
#' colour_palette <- c("blue", "skyblue", "lightgreen", "green", "yellow", "gold")
#' colours <- colorRampPalette(colour_palette)(length(breaks) - 1)
#' quick_plot_field(t2m, 0, col = colours, breaks = breaks, lab.breaks = breaks)
#'
#' library(viridis)
#' quick_plot_field(t2m, 0, hires = TRUE, col = viridis(256))

quick_plot_field <- function(
  eps_field,
  member,
  legend = TRUE,
  hires_coast = FALSE,
  ...
) {

  if (is.numeric(member)) {
    member_index <- which(eps_field$member == member)
    if (length(member_index) == 0) stop("Member ", member, " not found")
    plot_field <- eps_field$model_data[ , , member_index]
  } else if (tolower(member) %in% c("mean", "sd")) {
    plot_field <- ens_mean_and_sd(eps_field$model_data)[[paste0("ens_", member)]]
  } else {
    stop("Unknown option: '", member, "' for member")
  }

  x_range <- range(eps_field$x)
  y_range <- range(eps_field$y)

  if (hires_coast) {
    countries_polygon <- sp::spTransform(countriesHigh, eps_field$proj4_string)
  } else {
    countries_polygon <- sp::spTransform(countriesLow, eps_field$proj4_string)
  }
  countries_polygon <- raster::crop(countries_polygon, raster::extent(c(x_range, y_range)))

  plot_fun <- ifelse (legend, fields::image.plot, image)
  plot_fun(
    eps_field$x,
    eps_field$y,
    plot_field,
    asp  = 1,
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    bty  = "n",
    ...
  )
  sp::plot(countries_polygon, add = TRUE)
  graphics::lines(
    c(x_range[1], x_range[2], x_range[2], x_range[1], x_range[1]),
    c(y_range[1], y_range[1], y_range[2], y_range[2], y_range[1])
  )
}
