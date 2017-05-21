context("stat_interval")

test_that("stat_interval warns when there are fewer observations than
           the quantiles to be plotted",{
             df <- data.frame(x=rep(1:10,2), y=c(rnorm(20)))
             expect_warning(print(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
                            stat_interval(intervals = c(0.5,0.7,0.9))))

           })

test_that("mapping quantile aesthetic which are assymetric about median gives warning", {
          df <- data.frame(x=rep(1:10,2), quantile=c(rep(0.2,10), rep(0.6,10)),y=c(rnorm(20)))
          expect_warning(print(ggplot2::ggplot(df, ggplot2::aes(x=x,y=y,quantile=quantile)) +
                      stat_interval()))

})

