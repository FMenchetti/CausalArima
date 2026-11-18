inference_fixture <- function() {

  # Settings
  set.seed(1701)
  n_pre <- 160L
  steps <- seq_len(8L)
  dates <- as.Date("2020-01-01") + seq_len(n_pre + length(steps)) - 1L
  post <- n_pre + steps

  # Simulating a random walk
  innovations <- rnorm(length(dates), sd = 2)
  walk <- cumsum(innovations)
  walk[post] <- walk[post] + 3

  # Returning results
  list(pre = seq_len(n_pre), post = post, steps = steps, dates = dates,
       int_date = dates[post[1L]], innovations = innovations, walk = walk)
}

expect_inference <- function(result, effect, covariance, n_boot) {
  h <- length(effect)
  steps <- seq_len(h)
  cumulative <- cumsum(effect)

  # Variance of cumulative effect: let 'covariance' be the variance of the
  # causal effect at each time horizon, then cumulate * covariance * t(cumulate)
  # gives the variance-covariance matrix of the cumulative effects
  cumulate <- lower.tri(matrix(0, h, h), diag = TRUE) * 1
  sum_variance <- diag(cumulate %*% covariance %*% t(cumulate))

  # Results from normal and bootstrap inference
  normal <- result$norm$inf
  bootstrap <- result$boot$inf
  draws <- result$boot$effect

  # What to check
  expect_s3_class(result, "cArima")
  expect_identical(dim(normal), c(h, 15L))
  expect_identical(dim(bootstrap), c(h, 15L))
  expect_identical(dim(draws), c(h, n_boot))
  expect_true(all(is.finite(normal)))
  expect_true(all(is.finite(bootstrap)))
  expect_equal(result$causal.effect, effect)
  expect_equal(unname(normal[, "tau"]), effect)
  expect_equal(unname(normal[, "sum"]), cumulative)
  expect_equal(unname(normal[, "avg"]), cumulative / steps)
  expect_equal(unname(normal[, "sd.tau"]^2), diag(covariance))
  expect_equal(unname(normal[, "sd.sum"]^2), sum_variance)
  expect_equal(unname(normal[, "sd.avg"]^2), sum_variance / steps^2)
  expect_equal(unname(normal[, "pvalue.tau.b"]),
               2 * pnorm(-abs(effect / sqrt(diag(covariance)))))
  expect_identical(bootstrap[, c("tau", "sum", "avg")],
                   normal[, c("tau", "sum", "avg")])

  # Recompute the bootstrap statistics from the saved level trajectories.
  sum_draws <- cumulate %*% draws
  expect_equal(unname(bootstrap[, "sd.tau"]), apply(draws, 1L, sd))
  expect_equal(unname(bootstrap[, "sd.sum"]), apply(sum_draws, 1L, sd))
  expect_equal(unname(bootstrap[, "sd.avg"]), apply(sum_draws / steps, 1L, sd))
  expect_equal(unname(bootstrap[, "pvalue.tau.b"]),
               rowMeans(abs(draws) > abs(effect)))
}

test_that("stationary regression agrees with least squares prediction", {
  f <- inference_fixture()
  h <- length(f$steps)
  n_boot <- 2000L

  # Generating regressors and their coefficients
  xreg <- cbind(price = rnorm(length(f$dates)), promotion = rnorm(length(f$dates)))
  design <- cbind(intercept = 1, xreg)

  # Generating the time series: Yt = 20 + 2*price - 1*promotion + innovations
  y <- as.numeric(design %*% c(20, 2, -1)) + f$innovations
  y[f$post] <- y[f$post] + 3

  # Applying CausalArima to Yt
  result <- CausalArima(ts(y), f$dates, f$int_date, auto = FALSE,
                        xreg = xreg, nboot = n_boot)

  # Applying lm to Yt pre-intervention
  pre_design <- design[f$pre, ]
  future_design <- design[f$post, ]
  coefficients <- lm.fit(pre_design, y[f$pre])$coefficients

  # Estimating the effect by comparing the observed outcome with the
  # predicted outcome using estimated coefficients
  effect <- y[f$post] - as.numeric(future_design %*% coefficients)
  covariance <- result$model$sigma2 *
    (diag(h) + future_design %*% solve(crossprod(pre_design), t(future_design)))
  expect_inference(result, effect, covariance, n_boot)

  setup <- .inference.setup(result$model, xreg[f$pre, ], xreg[f$post, ], h)
  expect_identical(setup$integration, diag(h))
})

test_that("random-walk inference accumulates innovations in levels", {
  f <- inference_fixture()
  n_boot <- 2000L

  # Applying CausalArima to Yt = Y_t-1 + eps_t (random walk)
  # Note: The counterfactual at any given time point post-intervention is the
  # last observation before the intervention: E[Y_t+k|F_t] = Y_t, so the effect
  # is Yt[post] - Yt
  result <- CausalArima(ts(f$walk), f$dates, f$int_date,
                        order = c(0, 1, 0), nboot = n_boot)
  # The variance of of the effects tau_t+1, tau_t+2, ..., tau_t+k are given by
  # sigma^2, 2*sigma^2, ..., k*sigma^2, covariances are computed accordingly
  # so the variance-covariance matrix of the effects is given below
  covariance <- result$model$sigma2 * outer(f$steps, f$steps, pmin)

  # Check whether the effects and variances computed by CausalArima are equal
  #
  expect_inference(result, f$walk[f$post] - tail(f$walk[f$pre], 1L),
                   covariance, n_boot)

  # Check Monte Carlo variances against the empirical innovation distribution.
  # Similarly as above, bootstrap variance should be k * innovation variance
  res <- as.numeric(residuals(result$model))[-1L]
  innovation_variance <- mean((res - mean(res))^2)
  variance_ratio <- result$boot$inf[, "sd.tau"]^2 / (innovation_variance * f$steps)
  expect_lt(max(abs(variance_ratio - 1)), 0.15)

  # Final check: same model as above without bootstrap
  normal_only <- CausalArima(ts(f$walk), f$dates, f$int_date, order = c(0, 1, 0))
  expect_null(normal_only$boot)
  expect_equal(normal_only$norm, result$norm)
})

test_that("the inference setup aligns pre-intervention differences and forecast steps", {
  f <- inference_fixture()
  h <- length(f$steps)
  n_diff <- length(f$pre) - 1L
  fit <- forecast::Arima(ts(f$walk[f$pre]), order = c(0, 1, 0))
  setup <- .inference.setup(fit, NULL, NULL, h)
  cumulate <- lower.tri(matrix(0, h, h), diag = TRUE) * 1

  expect_identical(setup$pre, seq_len(n_diff))
  expect_identical(setup$post, n_diff + f$steps)
  expect_identical(dim(setup$At), c(h, n_diff))
  expect_identical(setup$integration, cumulate)
})

test_that("estimating drift adds uncertainty to level forecasts", {
  f <- inference_fixture()
  n_boot <- 2000L

  # Applying CausalArima to Yt = Y_t-1 + delta + eps_t (random walk with drift)
  # Note: The counterfactual at any given time point post-intervention is
  # E[Y_t+k|F_t] = Y_t + k \hat(delta) where \hat(delta) is is estimated as
  # the mean of the pre-intervention differences
  result <- CausalArima(ts(f$walk), f$dates, f$int_date, order = c(0, 1, 0),
                        arima.args = list(include.drift = TRUE), nboot = n_boot)
  pre_walk <- f$walk[f$pre]
  effect <- f$walk[f$post] - (tail(pre_walk, 1L) + f$steps * mean(diff(pre_walk)))

  # The variance of tau_t+k is k * sigma^2( 1+ k/(n.pre -1)) and the covariance
  # between the effects is computed accordingly
  covariance <- result$model$sigma2 *
    (outer(f$steps, f$steps, pmin) + tcrossprod(f$steps) / (length(pre_walk) - 1L))
  expect_inference(result, effect, covariance, n_boot)
})

test_that("seasonal integration accumulates errors only within each season", {
  f <- inference_fixture()
  n_boot <- 2000L
  period <- 4L
  y <- ts(as.numeric(diffinv(f$innovations, lag = period))[-seq_len(period)],
          frequency = period)
  result <- CausalArima(y, f$dates, f$int_date, seasonal = c(0, 1, 0),
                        nboot = n_boot)
  same_season <- outer(f$steps, f$steps, "-") %% period == 0
  covariance <- result$model$sigma2 *
    ceiling(outer(f$steps, f$steps, pmin) / period) * same_season
  last_season <- length(f$pre) - period + 1L + (f$steps - 1L) %% period
  expect_inference(result, y[f$post] - y[last_season], covariance, n_boot)
})
