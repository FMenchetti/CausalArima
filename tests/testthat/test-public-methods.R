public_method_fixture <- function() {
  set.seed(1702)
  n_pre <- 120L
  h <- 8L
  time <- seq_len(n_pre + h)
  dates <- as.Date("2020-01-01") + time - 1L
  post <- n_pre + seq_len(h)
  xreg <- cbind(price = rnorm(length(time)), promotion = rnorm(length(time)))
  y <- 20 + 2 * xreg[, "price"] - xreg[, "promotion"] +
    as.numeric(arima.sim(list(ar = 0.4), n = length(time)))
  y[post] <- y[post] + 3
  result <- CausalArima(ts(y), dates, dates[post[1L]], order = c(1, 0, 0),
                        xreg = xreg, nboot = 100L)
  horizons <- c(3L, h)
  list(result = result, post_dates = dates[post], horizons = horizons,
       horizon_dates = dates[post[horizons]])
}

test_that("print and summary select the requested inference rows", {
  f <- public_method_fixture()
  result <- f$result
  h <- length(f$post_dates)
  normal_print <- print(result)
  bootstrap_print <- print(result, type = "boot", horizon = f$horizon_dates)
  bootstrap_summary <- summary(result, type = "boot", horizon = f$horizon_dates)
  effect_rows <- c("Point causal effect", "Cumulative causal effect",
                    "Temporal average causal effect")
  expected_summary <- t(result$boot$inf[f$horizons, c("tau", "sum", "avg")])

  expect_identical(normal_print, result$norm$inf[h, ])
  expect_identical(bootstrap_print[["Time horizon"]], f$horizon_dates)
  expect_equal(unname(as.matrix(bootstrap_print[, -1L])),
               unname(result$boot$inf[f$horizons, ]))
  expect_identical(colnames(bootstrap_summary), as.character(f$horizon_dates))
  expect_equal(as.numeric(bootstrap_summary[effect_rows, ]),
               as.numeric(round(expected_summary, digits = 3)))
})

test_that("plots show the point and cumulative effects on the post-intervention dates", {
  f <- public_method_fixture()
  plots <- withCallingHandlers(
    plot(f$result, horizon = f$horizon_dates),
    lifecycle_warning_deprecated = function(warning) {
      # The existing plot method still uses ggplot2's deprecated line size.
      if (grepl("Using `size` aesthetic for lines was deprecated",
                conditionMessage(warning), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_s3_class(plots$forecast, "ggplot")
  expect_s3_class(plots$impact$point_effect, "ggplot")
  expect_s3_class(plots$impact$cumulative_effect, "ggplot")
  expect_s3_class(plots$residuals$ACF, "ggplot")
  expect_s3_class(plots$residuals$PACF, "ggplot")
  expect_s3_class(plots$residuals$QQ_plot, "ggplot")
  expect_identical(plots$impact$point_effect$data$x, f$post_dates)
  expect_equal(as.numeric(plots$impact$point_effect$data$y), f$result$causal.effect)
  expect_equal(as.numeric(plots$impact$cumulative_effect$data$y),
               cumsum(f$result$causal.effect))
})

test_that("numeric impact tables agree with the final inference estimates", {
  f <- public_method_fixture()
  h <- length(f$post_dates)
  normal <- f$result$norm$inf[h, ]
  bootstrap <- f$result$boot$inf[h, ]
  tables <- impact(f$result)

  expect_equal(tables$impact_norm$point_effect$estimate, unname(normal["tau"]))
  expect_equal(tables$impact_norm$sum$estimate, unname(normal["sum"]))
  expect_equal(tables$impact_norm$average$estimate, unname(normal["avg"]))
  expect_equal(unname(tables$impact_boot$effect_cum["absolute_effect", "sd"]),
               unname(bootstrap["sd.sum"]))
  expect_equal(unname(tables$impact_boot$average["absolute_effect", "sd"]),
               unname(bootstrap["sd.avg"]))
})
