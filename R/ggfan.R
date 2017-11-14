#' Fanplots for ggplot2
#'
#' Implements the functionality of the \code{fanplot} package as \code{ggplot}
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

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "n" ,"x"))
# see https://github.com/tidyverse/magrittr/issues/29 and
# https://github.com/jennybc/googlesheets/commit/cf829306284601888035b2bad5988d6387187d6e
