context("stat_interval")

test_that("stat_interval warns when there are fewer observations than
           the quantiles to be plotted",{
             df <- data.frame(x=rep(1:10,2), y=c(rnorm(20)))
             expect_warning(ggplot2::ggplot_build(
                              ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) +
                            stat_interval(intervals = c(0.5,0.7,0.9))))

           })

test_that("mapping quantile aesthetic which are assymetric about median gives warning", {
          df <- data.frame(x=rep(1:10,2), quantile=c(rep(0.2,10), rep(0.6,10)),
                           y=c(rnorm(20)))
          expect_warning(ggplot2::ggplot_build(
            ggplot2::ggplot(df, ggplot2::aes(x=x,y=y,quantile=quantile)) +
                      stat_interval(intervals=c(0.2,0.6))))

})

test_that("stat_interval filters precomputed quantiles to match interval args",{ 
          df <-data.frame(x=rep(1:3,rep(4,3)), 
                          quantile=rep(seq(2,8,2)/10, 3),
                          y= rep(1:4,3))
          p_b <- ggplot2::ggplot_build(
            ggplot2::ggplot(df, ggplot2::aes(x=x,
                                             y=y,
                                             quantile=quantile)) +
                                       stat_interval(intervals=0.2))
          expect_equal(length(unique(p_b$data[[1]]$interval)), 1)
          expect_equal(unique(p_b$data[[1]]$interval), as.factor(0.2))
})

test_that("stat_interval give error with strings of quantiles",{ 
  df <-data.frame(x=rep(1:3,rep(4,3)), 
                  quantile=vapply(rep(seq(20,80,20), 3),
                                  function(x) paste0(x,"%"),
                                  FUN.VALUE ="4%"),
                  y= rep(1:4,3))
  expect_warning(ggplot2::ggplot_build(
    ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, quantile=quantile)) +
                                 stat_interval(intervals=0.2)))
  
})

test_that("stat_interval throws error when given nonsensical quantiles",{ 
  df <-data.frame(x=rep(1:3,rep(4,3)), 
                  quantile=rep(seq(2,8,2)*100, 3),
                  y= rep(1:4,3))
  
  expect_warning(ggplot2::ggplot_build(
                   ggplot2::ggplot(df, 
                                   ggplot2::aes(x=x, y=y, quantile=quantile)) +
                 stat_interval(intervals=0.2)))
})

test_that("stat_interval gives error when precomputed quantiles and requested
           intervals don't match", {
             df <- data.frame(x=rep(1:10,2), quantile=c(rep(0.1,10), rep(0.9,10)),
                              y=c(rnorm(10,-3) ,rnorm(10,3)))
             expect_warning(ggplot2::ggplot_build(
               ggplot2::ggplot(df, ggplot2::aes(x=x,y=y,quantile=quantile)) +
                                    stat_interval(intervals=c(0.6))))
           })
