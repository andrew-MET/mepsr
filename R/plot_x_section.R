#' Plot a vertical cross section of a three dimensional field
#'
#' @param .fcst_3d A three dimseninal mepsr type object. Must have z as a named
#'   list element containng the level numbers.
#' @param point1 A vector of length 2 with the (x, y) location of the first
#'   point in the cross section. x and y will normally be longitude and latitude
#'   repsectively in decimal degrees. If x and y are in projected co-ordinates
#'   the proj4 string must be supplied in the \code{.proj4_string} argument.
#' @param point2 A vector of length 2 with the (x, y) location of the second
#'   point in the cross section. x and y must be in the same projection as
#'   \code{point1}.
#' @param surface_pressure A two dimensional mepsr type object with the surface
#'   pressure in Pa on the same domain as \code{.fcst_3d}.
#' @param .proj4_string The proj4 string for the cross section end points.
#' @param topo_color The colour to shade the topogrpahy.
#' @param topo_highlight The colour to highlight the top of the topography.
#' @param ... Arguments to \link[graphics]{image} and \link[fields]{image.plot}.
#'
#' @export
#'
#' @examples
plot_x_section <- function(
  .fcst_3d,
  point1,
  point2,
  surface_pressure,
  .proj4_string  = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
  topo_color     = "grey30",
  topo_highlight = "grey70",
  legend         = TRUE,
  ...
) {

  projected_points <- rbind(point1, point2) %>%
    sp::SpatialPoints(sp::CRS(.proj4_string)) %>%
    sp::spTransform(sp::CRS(.fcst_3d$proj4_string)) %>%
    as.data.frame()

  colnames(projected_points) <- c("x", "y")

  find_nearest_grid_index <- function(vec, point) {
    diffs <- abs(vec - point)
    which(diffs == min(diffs))
  }

  x1 <- find_nearest_grid_index(.fcst_3d$x, projected_points$x[1])
  x2 <- find_nearest_grid_index(.fcst_3d$x, projected_points$x[2])
  y1 <- find_nearest_grid_index(.fcst_3d$y, projected_points$y[1])
  y2 <- find_nearest_grid_index(.fcst_3d$y, projected_points$y[2])

# Ensure the longest grid direction in relation to the cross section line
# is used to find points along the cross section line.

  x_section_line <- data.frame(x = numeric(), y = numeric(), distance = numeric())

  if (diff(projected_points$x >= diff(projected_points$y))) {

    for (x in .fcst_3d$x[x1:x2]) {

      y <- .fcst_3d$y[y1]                 +
        (.fcst_3d$y[y2] - .fcst_3d$y[y1]) *
        (x - .fcst_3d$x[x1])              /
        (.fcst_3d$x[x2] - .fcst_3d$x[x1])

      distance_along_line <- sqrt(
        (x - .fcst_3d$x[x1]) ^ 2 +
        (y - .fcst_3d$y[y1]) ^ 2
      )

      x_section_line <- rbind(
        x_section_line,
        data.frame(x = x, y = y, distance = distance_along_line)
      )

    }

  } else {

    for (y in .fcst_3d$y[y1:y2]) {

      x <- .fcst_3d$x[x1]                 +
        (.fcst_3d$x[x2] - .fcst_3d$x[x1]) *
        (y - .fcst_3d$y[y1])              /
        (.fcst_3d$y[y2] - .fcst_3d$y[y1])

      distance_along_line <- sqrt(
        (x - .fcst_3d$x[x1]) ^ 2 +
        (y - .fcst_3d$y[y1]) ^ 2
      )

      x_section_line <- rbind(
        x_section_line,
        data.frame(x = x, y = y, distance = distance_along_line)
      )

    }

  }

# Interpolate the data onto the cross section for all vertical levels

  interp_to_x_section <- function(x_coords, y_coords, .fcst_2d, .psfc, x_section_data, level) {

    x_section_data$data <- fields::interp.surface(
      list(x = x_coords, y = y_coords, z = .fcst_2d),
      as.matrix(cbind(x_section_data$x, x_section_data$y))
    )

    x_section_data$psfc <- fields::interp.surface(
      list(x = x_coords, y = y_coords, z = .psfc),
      as.matrix(cbind(x_section_data$x, x_section_data$y))
    )

    x_section_data$level <- level

    constants <- dplyr::filter(hybrid_level_definitions_65, `level number` == level)

    x_section_data <- x_section_data %>%
      dplyr::mutate(pressure = constants$ahalf + constants$bhalf * .data$psfc)

    x_section_data

  }

  all_levels <- purrr::map2(
    seq_along(.fcst_3d$z),
    .fcst_3d$z,
    ~interp_to_x_section(
      .fcst_3d$x,
      .fcst_3d$y,
      .fcst_3d$model_data[ , , .x],
      drop(surface_pressure$model_data),
      x_section_line,
      .y
    )
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::transmute(
      level,
      distance = distance / 1000,
      pressure = pressure / 100,
      psfc = psfc / 100,
      data
    ) %>%
    tibble::as_tibble()

# Interpolate the cross section onto a regular grid for plotting

  interped_x_section <- akima::interp(
    all_levels$distance,
    all_levels$pressure,
    all_levels$data,
    xo = seq(min(all_levels$distance), max(all_levels$distance), 2.5),
    yo = seq(min(all_levels$pressure), max(all_levels$pressure), 1)
  )

# Construct the polygon for topography, adding a flat line at the bottom to join the ends

  topo_polygon <- all_levels %>%
    dplyr::filter(pressure == min(pressure)) %>%
    dplyr::select(distance, psfc)
  psfc_max <- max(topo_polygon$psfc)

  topo_polygon <- dplyr::bind_rows(
    data.frame(
      distance = min(min(c(topo_polygon$distance, interped_x_section$distance))),
      psfc = psfc_max + 25
    ),
    topo_polygon
  ) %>%
    dplyr::bind_rows(
      data.frame(
        distance = max(max(c(topo_polygon$distance, interped_x_section$distance))),
        psfc = psfc_max + 25
      )
    )

# Do the plot

  plot_fun <- ifelse (legend, fields::image.plot, image)

  plot_fun(
    interped_x_section$x,
    interped_x_section$y,
    interped_x_section$z,
    ylim      = rev(range(interped_x_section$y)),
    xaxt      = "n",
    yaxt      = "n",
    ann       = FALSE,
    axes      = FALSE,
    useRaster = TRUE,
    ...
  )

  polygon(topo_polygon$distance, topo_polygon$psfc, col = topo_color, border = topo_highlight)
}
