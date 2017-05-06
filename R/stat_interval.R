
#' See \code{\link[ggplot2]{ggplot2-ggproto}}
#' @format NULL
#' @usage NULL
#' @export
StatInterval <- ggplot2::ggproto("StatInterval", ggplot2::Stat,
  ## TODO error handling / checking
  # what if the data doesn't have enough points at each x?
  # do it test driven!
  setup_params = function(data, params) {
    # testing like below
    #if (!is.null(data$y) || !is.null(params$y)) {
    #  stop("stat_bin() must not be used with a y aesthetic.", call. = FALSE)
    #}
    #if (is.integer(data$x)) {
    #  stop('StatBin requires a continuous x variable the x variable is discrete.
    # Perhaps you want stat="count"?',
    #       call. = FALSE)
    #}

    #if (!is.null(params$drop)) {
    #  warning("`drop` is deprecated. Please use `pad` instead.", call. = FALSE)
    #  params$drop <- NULL
    #}
    params
  },

  compute_group = function(data, scales, params, intervals) {
#    probs <- c(rev(0.5 - intervals / 2), 0.5 + intervals / 2)
#    data_q <- dplyr::do(dplyr::group_by(data, x),
#                        data.frame(quantiles=probs,
#                                   y=quantile(.$y, probs=probs)))
    # calculate quantiles if not already present.
    if (!("quantile" %in% names(data))){
      data <- calc_quantiles(data, intervals)
    }
    data_interval <- dplyr::mutate(data,
                                   Interval = abs(quantile - 0.5) * 2)
    data_interval <- dplyr::mutate(data_interval, Interval=round(Interval, 3))
    data_interval <- dplyr::group_by(data_interval, x, Interval)
    data_interval <- dplyr::mutate(data_interval, hilo=ifelse(y==max(y), 1, -1))
    data_interval <- dplyr::ungroup(data_interval)
    return(data_interval)
  },
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(fill=..Interval..,
                    interval=..Interval..,

                    hilo=..hilo..,
                    alpha=1)
)

#' Calculate quantiles of a tidy dataframe
#'
#' @param data  A data frame with containing x and y columns, with several y
#' values for every x
#' @param intervals a list of intervals for which corresponding quantiles are
#' desired.
#'
#' @return A data frame containing x, y, and quantile columns.
#'
#'
#' @examples
#'
#' head(fake_df)
#'
#' fake_q <- calc_quantiles(fake_df, intervals=c(0,0.5,0.8))
#' head(fake_q)
#'
#' @export
calc_quantiles <- function(data, intervals){
  # TODO fix this and make more flexible
  if (!("x" %in% names(data) && "y" %in% names(data))){
    stop("data frame must have x and y columns")
  }
  probs <- c(rev(0.5 - intervals / 2), 0.5 + intervals / 2)
  data_q <- dplyr::do(dplyr::group_by(data, x),
                      data.frame(quantile=probs,
                                 y=stats::quantile(.$y, probs=probs)))
  return(data_q)
}


#' See \code{\link[ggplot2]{ggplot2-ggproto}}
#' @format NULL
#' @usage NULL
#' @export
StatIntervalFctr <- ggplot2::ggproto("StatIntervalFctr", StatInterval,
  compute_group= function(data, scales, params, intervals){
    data <- StatInterval$compute_group(data,scales,params,intervals)
    data <- dplyr::mutate(data,
                          Interval_cont=Interval,
                          Interval=as.factor(Interval))
    int_levs <-levels(data$Interval)
    int_levs[int_levs=="0"] <- "Median"
    levels(data$Interval) <- int_levs
    return(data)
  },
  default_aes = ggplot2::aes(interval=..Interval..,
                    group=-..Interval_cont.. * ..hilo..,
                    linetype=..Interval..,
                    hilo=..hilo..)
)

#' Very similar to \code{\link{geom_interval}}, except uses
#' \code{\link[ggplot2]{geom_line}} to handle the plotting. This makes handling
#' plotting of intervals for several groups difficult to achieve, so
#' \code{\link{geom_interval}} is prefered.
#'
#' @inheritParams geom_interval
#' @export
stat_interval <- function (mapping = NULL, data = NULL, stat = "interval_fctr",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE,
                           intervals=c(0,50,90)/100, ...)
{
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = ggplot2::GeomLine,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, intervals=intervals, ...)
        )
}

#' See \code{\link[ggplot2]{ggplot2-ggproto}}
#' @format NULL
#' @usage NULL
#' @export
StatSample <- ggplot2::ggproto("StatIntervalFctr", ggplot2::Stat,
  required_aes = c("x", "y", "group"),

  compute_panel= function(data, scales, params, n_samples=5){
    chosen_groups <- sample(data$group,n_samples)
    data <- data[data$group %in% chosen_groups,]
    return(data)
  }
  #default_aes=aes(alpha=0.5, size=0.2)
)


#' Plots a randomly chosen sample of the specified groups using
#' \code{\link[ggplot2]{geom_line}}
#'
#'
#' @param n_samples number of samples to plot
#' @param size The width of the line in mm
#' @param alpha The transparency of lines to be drawn. Must lie between 0 and 1.
#' @inheritParams  ggplot2::geom_line
#'
#'
#' @export
stat_sample <- function (mapping = NULL, data = NULL, stat = "sample",
                         position = "identity", na.rm = FALSE, show.legend = F,
                         inherit.aes = TRUE, n_samples=5, size=0.2, alpha=1.0,
                         ...)
{
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = ggplot2::GeomLine,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,
                      n_samples=n_samples,
                      alpha=alpha,
                      size=size, ...)
                )
}

