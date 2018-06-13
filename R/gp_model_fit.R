#' A \code{stan_fit} object used in the \code{ggfan_stan} vignette, containing posterior samples
#' from a latent gaussian process model. This is provided as data to avoid 
#' having to conduct computationally expensive sampling when producing the vignettes.
#' 
#'
#' The code needed to recreate the object is included in the examples, as well 
#' as in the vignette code chunks. 
#'
#'
#' @format A `stan_fit` object containing samples of the following parameters.
#' \describe{
#'   \item{eta_sq}{Gaussian process variance parameter}
#'   \item{rho_sq}{Gaussian process roughness parameter}
#'   \item{z}{Latent poisson rate}
#'   \item{y_gen}{Posterior predictive sample of counts `y`} 
#' }
#' See the help page for \code{\link[rstan]{stanfit-class}} for more details.
#' 
#' 
#'
#' @examples
#' \dontrun{
#' # generate mean and variance for sequence of samples over time
#' library(rstan)
#' library(dplyr)
#' library(magrittr)
#' library(tidyr)
#' library(tibble)
#' 
#' library(ggfan)
#' seed <- 34526
#' set.seed(seed)
#' 
#' # data 
#' x <- seq(-5,5,0.1)
#' N <- length(x)
#' y <- cbind(rpois(N, exp(sin(x)+2)),rpois(N, exp(sin(x)+2)))
#' 
#' stan_data <- list(N=N, x=x, y=y)
#' 
#' 
#' compiled_model <- stan_model(file=file.path(path.package("ggfan"), 
#'                                             "stan","latent_gp_pois.stan"))
#' gp_model_fit <- sampling(compiled_model, data=stan_data, iter=3000,thin=6)
#' #devtools::use_data(gp_model_fit, internal=FALSE)
#' }
"gp_model_fit"