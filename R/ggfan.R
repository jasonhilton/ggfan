#'
#' Implements the functionality of the fanplot package as ggplot
#' geoms. Designed for summarising MCMC samples from a posterior distribution,
#' where a visualisation is desired for several values of a continuous
#' covariate.
#' Increasing posterior intervals derived from the quantiles of the sampled
#' quantity are mapped to a continuous colour scale.
#'
#'
#' @section Functions:
#' The package contains three plotting functions.
#'
#'
#' \code{\link{geom_fan}} produces fan plots
#'
#' \code{\link{geom_interval}} produces line plots, where pairs of lines represent
#' intervals
#'
#' \code{\link{stat_sample}} randomly plots a specified number of samples from the
#' data
#'
#' An additional function \code{\link{calc_quantiles}} computes relevant
#' quantiles for a specified set of intervals.
#'
#'
#' @docType package
#'
#' @name ggfan
#'
#'
NULL
