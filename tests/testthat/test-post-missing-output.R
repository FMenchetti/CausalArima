post_output_arguments <- function(integrated = TRUE, use_xreg = TRUE) {
  set.seed(1911)
  dates <- as.Date("2020-01-01") + seq_len(128L) - 1L
  y <- rnorm(128L)
  if (integrated) y <- cumsum(y)
  xreg <- if (use_xreg) cbind(price = rnorm(128L), promotion = rnorm(128L)) else NULL
  if (use_xreg) y <- y + as.numeric(xreg %*% c(2, -1))
  y[121:128] <- y[121:128] + 3
  list(y = ts(y), dates = dates, int.date = dates[121L], auto = FALSE,
       order = c(0L, as.integer(integrated), 0L), xreg = xreg,
       arima.args = list(include.constant = FALSE))
}

post_missing_output_fixture <- function(only_one_observed = FALSE) {
  arguments <- post_output_arguments()
  post <- which(arguments$dates >= arguments$int.date)
  missing <- if (only_one_observed) c(1:5, 7:8) else c(1L, 4L, 5L, 8L)
  arguments$y[post[missing]] <- NA_real_
  result <- do.call(CausalArima, c(arguments, list(nboot = 100L)))
  list(result = result, post_dates = arguments$dates[post],
       observed = which(!is.na(result$causal.effect)))
}

test_that("missing horizons return every statistic at the previous observed date", {
  for (only_one_observed in c(FALSE, TRUE)) {
    f <- post_missing_output_fixture(only_one_observed)
    requested <- f$post_dates[c(8L, 6L, 8L)]
    actual <- if (only_one_observed) rep(f$post_dates[6L], 3L) else f$post_dates[c(7L, 6L, 7L)]
    indices <- match(actual, f$post_dates[f$observed])

    for (type in c("norm", "boot")) {
      expect_warning(printed <- print(f$result, type = type, horizon = requested),
                     "using previous observed post-intervention date")
      expect_identical(printed[["Time horizon"]], actual)
      expect_equal(unname(as.matrix(printed[, -1L])),
                   unname(f$result[[type]]$inf[indices, , drop = FALSE]))
      expect_warning(summarized <- summary(f$result, type = type, horizon = requested),
                     "using previous observed post-intervention date")
      expect_identical(colnames(summarized), as.character(actual))
      expect_equal(as.numeric(summarized[c(1L, 7L, 13L), ]),
                   as.numeric(round(t(f$result[[type]]$inf[indices, c("tau", "sum", "avg"), drop = FALSE]), 3L)))
    }

    messages <- character()
    tables <- withCallingHandlers(impact(f$result, horizon = requested), warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    expect_length(messages, 1L)
    expect_match(messages, "using previous observed post-intervention date")
    reference <- impact(f$result, horizon = actual)
    expect_equal(tables, reference)
    expect_identical(tables$impact_norm$point_effect$horizon, actual)
    expect_identical(names(tables$impact_boot), as.character(actual))
    expect_equal(print(f$result, horizon = actual)[["Time horizon"]], actual)
  }
})

test_that("horizons before the first observed post outcome return NA and a warning", {
  f <- post_missing_output_fixture()
  requested <- f$post_dates[1L]
  expect_warning(printed <- print(f$result, horizon = requested), "returning NA")
  expect_true(all(is.na(printed[, -1L])))
  expect_identical(printed[["Time horizon"]], requested)
  expect_warning(summarized <- summary(f$result, horizon = requested), "returning NA")
  expect_true(all(is.na(summarized[c(1:5, 7:11, 13:17), ])))
  expect_warning(tables <- impact(f$result, horizon = requested), "returning NA")
  expect_true(all(is.na(tables$impact_norm$point_effect[, -1L])))
  expect_true(all(is.na(tables$impact_boot[[1L]]$average)))
})

test_that("forecast retains missing outcomes and impact uses only observed dates", {
  f <- post_missing_output_fixture()
  plots <- suppressWarnings(plot(f$result, type = c("forecast", "impact")))
  forecast_data <- plots$forecast$data
  matches <- match(f$post_dates, forecast_data$x)
  expect_false(anyNA(matches))
  expect_identical(forecast_data$observed.cut[matches], tail(f$result$y, 8L))
  expect_equal(forecast_data$forecasted.cut[matches], f$result$forecast)
  expect_true(all(is.finite(forecast_data$forecasted.cut[matches])))
  expect_identical(plots$impact$point_effect$data$x, f$post_dates[f$observed])
  expect_equal(as.numeric(plots$impact$cumulative_effect$data$y),
               unname(f$result$norm$inf[, "sum"]))

  built <- suppressWarnings(ggplot2::ggplot_build(plots$forecast))
  line_layer <- which(vapply(plots$forecast$layers, function(layer) {
    inherits(layer$geom, "GeomLine")
  }, logical(1L)))
  expect_length(line_layer, 1L)
  observed_line <- built$data[[line_layer]]
  expect_identical(is.na(observed_line$y), is.na(forecast_data$observed.cut))
  expect_s3_class(suppressWarnings(ggplot2::ggplotGrob(plots$forecast)), "gtable")
})

test_that("forecast window does not require intervention date to be on the sampling grid", {
  arguments <- post_output_arguments(integrated = FALSE, use_xreg = FALSE)
  arguments$dates <- arguments$dates[1L] + 7L * seq_along(arguments$y)
  arguments$int.date <- arguments$dates[121L] - 1L
  arguments$y[c(124L, 128L)] <- NA_real_
  result <- do.call(CausalArima, arguments)
  plots <- suppressWarnings(plot(result, type = "forecast"))
  expect_equal(tail(plots$forecast$data$forecasted.cut, 8L), result$forecast)
  expect_identical(tail(plots$forecast$data$x, 8L), tail(arguments$dates, 8L))
})

test_that("POSIXct horizons are matched in seconds to POSIXct observed dates", {
  f <- post_missing_output_fixture()
  f$result$dates <- as.POSIXct(f$result$dates)
  f$result$int.date <- as.POSIXct(f$result$int.date)
  expect_warning(result <- print(f$result, horizon = as.POSIXct(f$post_dates[5L])),
                 "using previous observed post-intervention date")
  expect_identical(result[["Time horizon"]], as.POSIXct(f$post_dates[3L]))
})
