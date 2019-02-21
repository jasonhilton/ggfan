#' See \code{\link[ggplot2]{ggplot2-ggproto}}
#' @format NULL
#' @usage NULL
#' @importFrom dplyr n
#' @export
StatInterval <- ggplot2::ggproto("StatInterval", ggplot2::Stat,
  setup_params = function(data, params) {
    params
  },

  compute_group = function(data, scales, params, intervals) {
    precomputed_quantiles <- "quantile" %in% names(data)
    if(length(data$x)<2){
      stop("Too few rows in plotting data. This may be because invalid data has
           been provided")
    }

    # calculate quantiles if not already present.
    if (!precomputed_quantiles){
      data <- calc_quantiles(data, intervals)
    } else {
      if (class(data$quantile)!= "numeric"){
        stop("Please ensure quantiles are inputed as a numeric vector with
             values bounded by 0 and 1")
      }
      if (max(data$quantile) > 1 | min(data$quantile) < 0){
        stop("Please ensure quantiles are inputed as a numeric vector with
             values bounded by 0 and 1")
      }
    }

    tol <- 1e-6
    data_interval <- dplyr::mutate(data,
                                   Interval = abs(quantile - 0.5) * 2)
    # round to avoid problems grouping by floating point.
    data_interval <- dplyr::mutate(data_interval, Interval=round(Interval, 3))

    if(precomputed_quantiles){
      # if the quantiles were precomputed they do not necessarily correspond
      # to intervals in the argument.
      # Filter to only contain those intervals requested
      data_interval <- dplyr::filter(data_interval,
                              .in_numeric(Interval, intervals, tol))
      if (dim(data_interval)[1] ==0){
        stop("No rows in plotting data frame. This may be because the intervals
              requested do not correspond to the precomputed quantiles you
              provided in the data. \n
              e.g. To plot interval 0.5, you need quantiles 0.25 and 0.75 \n
              Default intervals are 0, 0.5, and 0.9, so quantiles needed are
              c(0.05,0.25,0.5,0.75,0.95)")
      }
    }

    data_interval <- dplyr::group_by(data_interval, x, Interval)
    # exclude median, as is the empty interval (0.5,0.5).
    # Use tol. to avoid floating point wierdness.
    df_n <- dplyr::filter(data_interval, abs(quantile - 0.5) > tol)
    df_n <- dplyr::summarise(dplyr::group_by(df_n,Interval, x), n=n())
    if(!all(df_n$n==2)){
      stop(paste("Not all intervals have exactly two values at each x (high and low boundaries)",
                 "Note quantiles supplied must be in symmetric pairs about the median.",
                 "For example c(0.2,0.5,0.8) is admissable, c(0.1,0.5,0.51) is not"
      ))
    }
    data_interval <- dplyr::mutate(data_interval,
                                   hilo=ifelse(rank(y,ties.method="first")==n(),
                                               1, -1))
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
#' @param intervals A list of intervals for which corresponding quantiles are
#' desired.
#' @param x_var A character string giving the name of the x variable
#' @param y_var A character string giving the name of the y variable
#' @param rename Logical. Indicates whether to retain existing variable name
#' or use \code{x} and \code{y}.
#' @return A data frame containing x, y, and quantile columns (possibly renamed)
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
calc_quantiles <- function(data, intervals, x_var="x",y_var="y",rename=T){
  if (!(is.data.frame(data))){
    stop("data must be a data frame")
  }
  if (!(x_var %in% names(data) && y_var %in% names(data))){
     stop("specified x and y variables are not in dataframe")
  }

  probs <- c(rev(0.5 - intervals / 2), 0.5 + intervals / 2)
  # TODO recode with simpler way of doing this using rename_ or similar do_
  data$x <- data[[x_var]]
  data$y <- data[[y_var]]
  grouped_df <- dplyr::group_by(data, x)
  n_df <- dplyr::summarise(grouped_df, n=n())
  if (min(n_df$n) < length(probs)){
    stop("too few observations per x value to compute specifed intervals")
  }
  data_q <- dplyr::do(grouped_df,
                      data.frame(quantile=probs,
                                 y=stats::quantile(.$y, probs=probs)))
  if(!rename){
    data_q[[x_var]] <- data_q$x
    data_q[[y_var]] <- data_q$y
    data_q$x <- NULL
    data_q$y <- NULL
  }
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
                          Interval=as.factor(Interval),
                          Int_group=-Interval_cont * hilo)
    int_levs <-levels(data$Interval)
    int_levs[int_levs=="0"] <- "Median"
    levels(data$Interval) <- int_levs
    return(data)
  },
  default_aes = ggplot2::aes(interval=..Interval..,
                    group=..Int_group..,
                    linetype=..Interval..,
                    hilo=..hilo..)
)

#' Line plot visualising intervals of a distribution
#'
#'  Very similar to \code{\link{geom_interval}}, except uses
#' \code{\link[ggplot2:geom_path]{geom_line}} to handle the plotting. This makes handling
#' plotting of intervals for several groups difficult to achieve, so
#' \code{\link{geom_interval}} is preferred.
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
StatSample <- ggplot2::ggproto("StatSample", ggplot2::Stat,
  required_aes = c("x", "y", "group"),

  compute_panel= function(data, scales, params, n_samples=5){
    chosen_groups <- sample(data$group,n_samples)
    data <- data[data$group %in% chosen_groups,]
    return(data)
  },
  default_aes=ggplot2::aes(size=0.2)
)


#' Plots a randomly chosen sample of the specified groups using
#' \code{\link[ggplot2:geom_path]{geom_line}}
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


#' Find elements of one numeric vector in another.
#'
#' Find which elements of `a` are equal to at least one element in `b`, to
#' within some tolerance tol.
#'
#' @param a The vector for which comparison are to be made
#' @param b The vector to match against.
#' @param tol The tolerance within which values are assumed to be the same.
#'
#' @return A vector of logical values the same length as `a` describing whether
#' this vector element is close to at least one element in `b`
#'
.in_numeric <- function(a,b, tol=1e-6){
  if (length(b)==1){
    return(abs(a-b) < tol)
  } else{
    logical_mat <- vapply(a, function(x,y) abs(x-y) < tol,
                    FUN.VALUE=logical(length = length(b)),b)
    logical_vec <- apply(logical_mat, 2, any)
  }
  return(logical_vec)
}
