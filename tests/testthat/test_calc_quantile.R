context("calc_quantile")


test_that("calc_quantile returns df with x and y",
  {
  intervals <- c(0.5,0.8)
  df <- data.frame(x=rep(1:10,5), y=rnorm(50))
  df_q <- calc_quantiles(df, intervals)
  expect_true(all(c("x","y") %in% names(df_q)))
  }
)

test_that("calc_quantile allows retention of original df names",
  {
  intervals <- c(0.5,0.8)
  df <- data.frame(a=rep(1:10,5), b=rnorm(50))
  df_q <- calc_quantiles(df, intervals, x_var="a", y_var="b", rename=F)
  expect_true(all(c("a","b") %in% names(df_q)))
  }
)

test_that("calc_quantile returns error when too few observations are present",
  {
  intervals <- c(0.5,0.8)
  df <- data.frame(x=rep(1:10,2), y=rnorm(20))
  expect_error(df_q <- calc_quantiles(df, intervals))
  }
)

