#' Fan plot visualising intervals of a distribution
#'
#' Fan Plots allow the distribution of a variable to be visualised by
#' representing sets of central probability intervals through colour.
#' For every value of \code{x}, \code{geom_fan} computes quantiles of \code{y} and uses these to plot
#' intervals containing increasing proportions of the total density of \code{y}.
#' Intervals are mapped to a continuous colour scale, so that changes in colour
#' represent intervals covering an increasing proportion of total density.
#' Quantiles can also be precomputed and mapped to the aesthetic \code{quantile}.
#' This function is designed with the need to summarise MCMC posterior
#' distributions in mind, and implements the functionality of the \code{fanplot}
#' package in \code{ggplot2}. Note that there should be enough observations of 
#' \code{y} at each \code{x} to allow estimation of the specified quantiles.
#'
#' @inheritParams ggplot2::layer
#' @param stat Use to override the default use of \code{\link{stat_interval}}
#' @param intervals specify the collection of intervals to be represented in the
#' fan.
#' @param ... other arguments passed on to \code{layer}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#'
#'
#' @section Aesthetics:
#'
#' \code{geom_fan} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \code{alpha}
#' \item \code{group}
#' \item \code{quantile}
#' }
#'
#' @export
#'
#'
#' @seealso
#' \code{stat_summary} Summarises y at each value of x
#'
#' \code{stat_quantile} Uses quantile regression to predict quantiles
#'
#' \code{geom_interval} Plot intervals boundaries as lines
#'
#'
#' @examples
#'
#' # Basic use. The data frame must have multiple y values for each
#' # x
#' library(ggplot2)
#'
#' ggplot(fake_df, aes(x=x,y=y)) +geom_fan()
#'
#'
#' # use precomputed quantiles - reducing storage requirements.
#' intervals = 1:19/20
#' fake_q <- calc_quantiles(fake_df, intervals=intervals)
#' # intervals in geom_fan must be the same as used to compute quantiles.
#' ggplot(fake_q, aes(x=x,y=y, quantile=quantile)) +
#'  geom_fan(intervals=intervals)
#'
#'
#' # change the colour scale
#' ggplot(fake_df, aes(x=x,y=y)) + geom_fan() + scale_fill_gradient(low="red", high="pink")
#'
#'
geom_fan <- function (mapping = NULL, data = NULL, stat = "interval", position = "identity",
         show.legend = NA, inherit.aes = TRUE, intervals=(2:98)/100, ...)
{

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomIntervalPoly, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(intervals=intervals, ...))
}


#' See \code{\link[ggplot2]{ggplot2-ggproto}}
#' @format NULL
#' @usage NULL
#' @export
GeomIntervalPoly <- ggplot2::ggproto("GeomIntervalPoly", ggplot2::Geom,
    required_aes = c("x", "y", "fill", "interval","hilo"),
    default_aes=ggplot2::aes(alpha=1),
    draw_group = function(data, panel_scales, coord) {
      n <- nrow(data)
      if (n <= 2) return(grid::nullGrob())
      # exclude median, avoiding floating point comparison problems.
      tol <- 1e-6
      data <- dplyr::filter(data, abs(interval - 0.0) > tol)
      data <- dplyr::group_by(data, x)

      data <- dplyr::mutate(data, ordering = hilo * x)

      data <- dplyr::arrange(data, hilo, ordering, -interval)

      data <- dplyr::ungroup(data)
      coords <- coord$transform(data, panel_scales)
      grid::polygonGrob(
        coords$x, coords$y,
        id= - data$interval * 100,
        #id=-1,
        default.units = "native",
        gp = grid::gpar(col = 0,
                        fill = scales::alpha(data$fill, data$alpha),
                        lwd = 0,
                        lty = 0
                )
       )
    },
    draw_key=ggplot2::draw_key_rect
)

#' Line plot visualising intervals of a distribution
#'
#' For every value of \code{x}, computes quantiles of \code{y} and uses these to plot
#' intervals containing increasing proportions of the total density of \code{y}.
#' Boundaries of intervals are mapped to \code{linetype}. Quantiles can also be
#' precomputed and mapped to the aesthetic \code{quantile}.
#' This function is designed with the need to summarise MCMC posterior
#' distributions in mind.
#'
#' @section Aesthetics:
#'
#' \code{geom_interval} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \code{quantile}
#' \item \code{group}
#' \item \code{colour}
#' \item \code{size}
#' }
#'
#' @export
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @param stat Use to override the default use of \code{\link{stat_interval}}
#' @param intervals specify the collection of intervals to be represented in the
#' fan.
#'
#' @seealso
#' \code{stat_summary} Summarises y at each value of x
#' \code{stat_quantile} Uses quantile regression to predict quantiles
#' \code{geom_fan} Plot intervals on a continuous colour scale
#'
#'
#' @examples
#'
#' library(ggplot2)
#' # Basic use. The data frame must have multiple y values for each
#' # x
#' ggplot(fake_df, aes(x=x,y=y)) +geom_interval()
#'
#'
#' # use precomputed quantiles - reducing storage requirements.
#' intervals = c(0,50,90)/100
#' fake_q <- calc_quantiles(fake_df, intervals=intervals)
#' # intervals in geom_fan must be the same as used to compute quantiles.
#' ggplot(fake_q, aes(x=x,y=y, quantile=quantile)) +
#'  geom_interval(intervals=intervals)
#'
#'
geom_interval <- function(mapping = NULL, data = NULL,
                      stat = "interval_fctr", position = "identity",
                      intervals=c(0,50,90)/100,
                      lineend = "butt",
                      linejoin = "round",
                      linemitre = 1,
                      arrow = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIntervalPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      intervals=intervals,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' See \code{\link[ggplot2]{ggplot2-ggproto}}
#' @format NULL
#' @usage NULL
#' @export
GeomIntervalPath <- ggplot2::ggproto("GeomIntervalPath", ggplot2::Geom,
  required_aes = c("x", "y", "interval","hilo","linetype"),

  default_aes = ggplot2::aes(colour = "black", size = 0.5, 
                             alpha = NA,
                             linetype="solid"),
  draw_group = function(data, panel_scales, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1,
                        na.rm = FALSE) {

    data$line_id <- interaction(data$interval,data$hilo)

    data <- data[order(data$line_id), , drop = FALSE]
    coords <- coord$transform(data, panel_scales)

    grid::polylineGrob(
      coords$x, coords$y, id = data$line_id,
      default.units = "native", arrow = arrow,
      gp = grid::gpar(col = alpha(data$colour, data$alpha),
                lwd = data$size * .pt,
                lty = unique(data$linetype),
                lineend = lineend,
                linejoin = linejoin,
                linemitre = linemitre
        )
     )
  },
  draw_key = ggplot2::draw_key_path
)
