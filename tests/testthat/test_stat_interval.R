context("stat_interval")


test_that("stat_interval throws an error when there are fewer observations than
           the quantiles to be plotted",{
             df <- data.frame(x=rep(1:10,2), y=c(rnorm(20)))
             expect_error(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
                            stat_interval(intervals = c(0.5,0.7,0.9)))

             df <- data.frame(x=rep(1:10,2), y=c(rnorm(20)))
             expect_error(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
                            stat_interval(intervals = c(0.0)))
           })

test_that("mapping quantile aesthetic which are assymetric about median gives error",{
             df <- data.frame(x=rep(1:10,2), quantile=rep(c(0.2,0.6),10),y=c(rnorm(20)))
             expect_error(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y,quantile=quantile)) +
                            stat_interval())

})


test_that("stat_interval warns when there are less than double the number of
          observations vs intervals",
          {
            df <- data.frame(x=rep(1:10,5),y=rnorm(50))
            expect_warning(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
                           stat_interval(intervals = c(0.5,0.8,0.9)))

           })

test_that("stat_interval throws a warning when x argument are not numeric",{
          df <- data.frame(x=rep(letters[1:10],2),y=rnorm(20))
          expect_warning(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
                           stat_interval())

          })


test_that("stat_interval throws an error when y argument are not numeric",{
  df <- data.frame(x=rep(letters[1:10],2),y=sample(letters[1:10],20,replace=T))
  expect_error(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
                 stat_interval())

})


test_that("stat_interval produces error when there are NAs.",
          {df <- data.frame(x=rep(1:10,5),
                            y=c(rnorm(40), NA, rnorm(3), NA,rnorm(5)))
          expect_error(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
                           stat_interval(intervals=c(0.0,0.2)))
          })

