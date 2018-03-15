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
#' @param member Member to plot. Can be numeric or one of "mean", "sd", or "all".
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
#' t2m <- read_members(my_file, "T2m", lead_time = 0)
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
  col = fields::tim.colors(),
  ...
) {

  if (is.numeric(member)) {
    member_index <- which(eps_field$member == member)
    if (length(member_index) == 0) {
      stop("Member ", member, " not found")
    } else {
      plot_field <- eps_field$model_data[ , , member_index]
    }
  } else if (tolower(member) %in% c("mean", "sd")) {
    plot_field <- ens_mean_and_sd(
      eps_field$model_data
    )[[paste0("ens_", tolower(member))]]
  } else if (tolower(member) == "all") {
    plot_field <- eps_field$model_data
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

  plot_cols  <- 1
  plot_rows  <- 1
  num_panels <- 1
  if (length(dim(plot_field)) == 3) {
    num_panels <- dim(plot_field)[3]
    plot_cols <- num_panels %>%
      sqrt() %>%
      ceiling()
    plot_rows <- plot_cols - 1
    if (plot_cols * plot_rows < num_panels) plot_rows <- plot_cols
  }
  panel_layout <- t(matrix(seq(1, plot_cols * plot_rows), ncol = plot_cols, nrow = plot_rows))
  layout(panel_layout)
  par(mar = c(1, 1, 1, 1))
  plot_fun <- ifelse (legend, fields::image.plot, image)
  for (plot_panel in seq(1, num_panels)) {
    if (num_panels == 1) {
      panel_plot <- plot_field
    } else {
      panel_plot <- plot_field[, , plot_panel]
    }
    plot_fun(
      eps_field$x,
      eps_field$y,
      panel_plot,
      asp  = 1,
      xlab = "",
      ylab = "",
      xaxt = "n",
      yaxt = "n",
      bty  = "n",
      col  = col,
      ...
    )
    sp::plot(countries_polygon, add = TRUE)
    graphics::lines(
      c(x_range[1], x_range[2], x_range[2], x_range[1], x_range[1]),
      c(y_range[1], y_range[1], y_range[2], y_range[2], y_range[1])
    )
  }
}
