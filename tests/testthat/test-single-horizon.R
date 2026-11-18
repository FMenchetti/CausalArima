single_horizon_fixture <- function(h = 1L, integrated = FALSE,
                                   use_xreg = FALSE, include_constant = FALSE) {
  set.seed(1801)
  n_pre <- 120L
  time <- seq_len(n_pre + h)
  dates <- as.Date("2020-01-01") + time - 1L
  post <- n_pre + seq_len(h)
  innovations <- rnorm(length(time))
  y <- if (integrated) cumsum(innovations) else innovations
  if (include_constant) {
    y <- y + if (integrated) 0.2 * time else 20
  }
  xreg <- NULL
  if (use_xreg) {
    xreg <- cbind(price = rnorm(length(time)), promotion = rnorm(length(time)))
    y <- y + as.numeric(xreg %*% c(2, -1))
  }
  y[post] <- y[post] + 3
  arima_args <- if (integrated) {
    list(include.drift = include_constant)
  } else {
    list(include.mean = include_constant)
  }

  list(y = ts(y), dates = dates, int.date = dates[post[1L]],
       auto = FALSE, order = c(0L, as.integer(integrated), 0L),
       xreg = xreg, arima.args = arima_args)
}

test_that("a single post-intervention observation keeps inference dimensions", {
  cases <- expand.grid(integrated = c(FALSE, TRUE), use_xreg = c(FALSE, TRUE),
                       include_constant = c(FALSE, TRUE))
  n_boot <- 200L

  for (i in seq_len(nrow(cases))) {
    arguments <- do.call(single_horizon_fixture, as.list(cases[i, ]))
    normal_only <- do.call(CausalArima, arguments)
    result <- do.call(CausalArima, c(arguments, list(nboot = n_boot)))

    expect_identical(dim(result$norm$inf), c(1L, 15L))
    expect_identical(dim(result$boot$inf), c(1L, 15L))
    expect_identical(dim(result$boot$effect), c(1L, n_boot))
    expect_true(all(is.finite(result$norm$inf)))
    expect_true(all(is.finite(result$boot$inf)))
    expect_null(normal_only$boot)
    expect_equal(result$norm, normal_only$norm)

    # At one observation the point, cumulative and average statistics coincide.
    for (inference in list(result$norm$inf, result$boot$inf)) {
      expect_equal(unname(inference[, "tau"]), result$causal.effect)
      expect_equal(unname(inference[, "sum"]), result$causal.effect)
      expect_equal(unname(inference[, "avg"]), result$causal.effect)
      expect_equal(unname(inference[, "sd.sum"]), unname(inference[, "sd.tau"]))
      expect_equal(unname(inference[, "sd.avg"]), unname(inference[, "sd.tau"]))
    }
    expect_equal(unname(result$boot$inf[, "sd.tau"]), sd(result$boot$effect[1L, ]))

    if (!cases$use_xreg[i]) {
      pre_y <- as.numeric(head(arguments$y, -1L))
      pre_innovations <- if (cases$integrated[i]) diff(pre_y) else pre_y
      estimated_constant <- if (cases$include_constant[i]) mean(pre_innovations) else 0
      counterfactual <- estimated_constant
      if (cases$integrated[i]) {
        counterfactual <- tail(pre_y, 1L) + estimated_constant
      }
      variance <- result$model$sigma2 *
        (1 + as.integer(cases$include_constant[i]) / length(pre_innovations))
      expect_equal(result$causal.effect, tail(as.numeric(arguments$y), 1L) - counterfactual)
      expect_equal(unname(result$norm$inf[, "sd.tau"]^2), variance)
    }
  }
})

test_that("public methods accept the only post-intervention date", {
  for (use_xreg in c(FALSE, TRUE)) {
    arguments <- single_horizon_fixture(integrated = TRUE, use_xreg = use_xreg)
    for (n_boot in list(NULL, 200L)) {
      result <- do.call(CausalArima, c(arguments, list(nboot = n_boot)))
      for (type in c("norm", if (!is.null(n_boot)) "boot")) {
        printed <- print(result, type = type, horizon = result$int.date)
        summarized <- summary(result, type = type, horizon = result$int.date)
        expect_equal(as.numeric(printed[1L, -1L]), as.numeric(result[[type]]$inf))
        expect_identical(dim(summarized), c(17L, 1L))
        expect_identical(colnames(summarized), as.character(result$int.date))
        expect_equal(as.numeric(summarized[c(1L, 7L, 13L), 1L]),
                     rep(round(result$causal.effect, 3L), 3L))
      }

      tables <- impact(result)
      horizon_tables <- impact(result, horizon = result$int.date)
      expect_equal(impact(result, horizon = NULL), tables)
      expect_equal(tables$impact_norm$point_effect$estimate, result$causal.effect)
      expect_equal(horizon_tables$impact_norm$point_effect$estimate, result$causal.effect)
      if (is.null(n_boot)) {
        expect_null(horizon_tables$impact_boot)
      } else {
        expect_equal(horizon_tables$impact_boot[[1L]], tables$impact_boot)
        expect_equal(unname(tables$impact_boot$average["absolute_effect", "sd"]),
                     unname(result$boot$inf[, "sd.avg"]))
      }

      plots <- withCallingHandlers(
        {
          explicit_horizon <- plot(result, horizon = result$int.date)
          expect_equal(plot(result, horizon = NULL)$impact$point_effect$data,
                       explicit_horizon$impact$point_effect$data)
          explicit_horizon
        },
        lifecycle_warning_deprecated = function(warning) {
          if (grepl("Using `size` aesthetic for lines was deprecated",
                    conditionMessage(warning), fixed = TRUE)) {
            invokeRestart("muffleWarning")
          }
        }
      )
      for (effect_plot in plots$impact) {
        built <- ggplot2::ggplot_build(effect_plot)
        interval_layer <- which(vapply(effect_plot$layers, function(layer) {
          inherits(layer$geom, "GeomPointrange")
        }, logical(1L)))
        expect_length(interval_layer, 1L)
        interval <- built$data[[interval_layer]]
        expect_equal(interval$y, result$causal.effect)
        expect_equal(interval$ymax - interval$ymin,
                     2 * qnorm(0.975) * unname(result$norm$inf[, "sd.tau"]))
      }
      expect_s3_class(ggplot2::ggplot_build(plots$forecast), "ggplot_built")
    }
  }
})

test_that("seasonal integration preserves a single forecast step", {
  arguments <- single_horizon_fixture()
  arguments$y <- ts(as.numeric(diffinv(as.numeric(arguments$y), lag = 4L))[-seq_len(4L)],
                    frequency = 4L)
  arguments$seasonal <- c(0L, 1L, 0L)
  result <- do.call(CausalArima, c(arguments, list(nboot = 200L)))
  expected_effect <- as.numeric(tail(arguments$y, 1L) - arguments$y[length(arguments$y) - 4L])

  expect_identical(dim(result$boot$effect), c(1L, 200L))
  expect_equal(result$causal.effect, expected_effect)
  expect_equal(unname(result$norm$inf[, "sd.tau"]^2), result$model$sigma2)
  expect_equal(unname(result$norm$inf[, "sd.sum"]), unname(result$norm$inf[, "sd.tau"]))
  expect_equal(unname(result$boot$inf[, "sd.avg"]), unname(result$boot$inf[, "sd.tau"]))
})

test_that("a single requested horizon works with several post-intervention observations", {
  arguments <- single_horizon_fixture(h = 8L, use_xreg = TRUE)
  result <- do.call(CausalArima, c(arguments, list(nboot = 200L)))
  first_horizon <- result$int.date
  summarized <- summary(result, type = "boot", horizon = first_horizon)
  tables <- impact(result, horizon = first_horizon)

  expect_identical(dim(summarized), c(17L, 1L))
  expect_equal(tables$impact_norm$point_effect$estimate, result$causal.effect[1L])
  expect_equal(tables$impact_boot[[1L]]$effect_cum["absolute_effect", "estimates"],
               result$causal.effect[1L])
  expect_equal(unname(tables$impact_boot[[1L]]$effect_cum["absolute_effect", "sd"]),
               unname(result$boot$inf[1L, "sd.tau"]))
})
