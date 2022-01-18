######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-08-09                                                ####
####                                                                              ####
####  Content:          Causal effect of an intervention                          ####
####                                                                              ####
####  Main function :   CausalArima                                               ####
####  Dependencies:     .boot.inf                                                 ####
####                    .norm.inf                                                 ####
####                    .long                                                     ####
####                    .sarma2larma                                              ####
######################################################################################
######################################################################################

#' Causal effect of an intervention
#'
#' @param y Vector of observations, univariate time series of class \code{ts}.
#' @param auto If set to TRUE, \code{auto.arima} returns the best fitting model in the pre-intervention period
#'             according to the selected \code{ic}. Defaults to TRUE.
#' @param order Order of the non-seasonal part of the ARIMA model, (p, d, q). Required when \code{auto} is set to FALSE.
#' @param seasonal Order of the seasonal part of the ARIMA model, (P, D, Q). Defaults to \code{c(0, 0, 0)}.
#' @param ic Information criterion to use for model selection. Possible values in c("aicc", "aic", "bic").
#'           Defaults to \code{"aic"}.
#' @param xreg  Optional vector, matrix or data.frame of regressors to be included in the model.
#'              It must have the same length of y.
#' @param dates Vector of dates of length t (with elements of class \code{Date}) corresponding to the
#'              observations in y.
#' @param int.date Date of the intervention (must be of class \code{Date}).
#' @param arima.args Additional arguments to be passed to Arima.
#' @param auto.args Additional arguments to be passed to auto.arima.
#' @param nboot Number of bootstrap iteration. Optional, if \code{nboot > 0} the function samples
#'              \code{nboot} times from the empirical distribution of model residuals and computes
#'              empirical critical values for the point, cumulative and temporal average effect statistics.
#' @param alpha Confidence level for the prediction intervals. Defaults to \code{0.05}.
#'
#' @return A list with the following components:
#' \item{norm}{The inferred point, cumulative and temporal average effects at each point in time after the
#'             intervention with standard deviations and p-values (left-sided, right-sided, bidirectional)
#'             estimated under the assumption of Normally distributed residuals.}
#' \item{boot}{The inferred point, cumulative and temporal average effects at each point in time after the
#'             intervention and empirical critical values (left-sided, right-sided, bidirectional) estimated
#'             by bootstrapping the errors from model residuals.}
#' \item{causal.effect}{Estimated causal effect at each point in time after the intervention
#'                    date (point effect).}
#' \item{model}{The model estimated in the pre-intervention period,
#'              result of a call to \code{Arima}.}
#' \item{dates}{The vector of dates considered in the analysis.}
#' \item{int.date}{The intervention date.}
#' \item{y}{The vector of observations considered in the analysis.}
#' \item{forecast}{Forecasted time series in the absence of intervention.}
#' \item{forecast_lower}{Lower confidence bound for the forecasted series.}
#' \item{forecast_upper}{Upper confidence bound for the forecasted series.}
#' \item{alpha}{Confidence level for the prediction intervals.}
#'
#' @export
#'
#' @examples
#' ## Example 1 (daily data, no predictors)
#' # Generating a time series of length 1000 and a vector of dates
#' y <- 0.5*seq(0.5, 250, by = 0.5) + rnorm(500, sd = 6)
#' dates <- seq.Date(from = as.Date("2014-01-05"), by = "days", length.out = 500)
#'
#' # Adding a fictional intervention
#' int.date <- as.Date("2015-04-01")
#' horizon <- c(as.Date("2015-04-10"), as.Date("2015-04-20"))
#' y.new <- y ; y.new[dates >= int.date] <- y.new[dates >= int.date]*1.25
#'
#' # Plot
#' oldpar <- par(no.readonly = TRUE)
#' plot(y = y.new, x = dates, type = "l", col = "cadetblue", xlim = c(as.Date("2014-10-01"), tail(dates, 1)))
#' lines(y = y, x = dates, col = "orange")
#'
#' # Causal effect estimation
#' start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
#' ce <- CausalArima(y = ts(y.new, start = start, frequency = 1), auto = TRUE, ic = "aic", dates = dates, int.date = int.date)
#'
CausalArima<-function(y, auto = TRUE, order = c(0, 0, 0), seasonal = c(0, 0, 0), ic = "aic", xreg = NULL, dates,
                      int.date, arima.args = list(), auto.args = list(), nboot = NULL, alpha = 0.05){

  ### param checks
  # if(class(y) != "ts") stop("y must be an object of class ts")
  if(!missing(xreg)) {
    if(!is.matrix(xreg) && !is.data.frame(xreg) && !is.numeric(xreg))
      stop("`xreg` must be a numeric vector, matrix or data.frame")
    xreg <- as.matrix(xreg)
    if(nrow(xreg) != length(y)) stop("nrow(xreg) != length(y)")
  }
  if(!any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`dates` must be a vector of class Date")
  if(length(dates) != length(y)) stop("length(dates) != length(y)")
  if(length(int.date) != 1 || !any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`int.date` must be a Date of length 1")
  if(!missing(nboot) && (!is.numeric(nboot) | nboot <= 0)) stop("`nboot` must be a positive numeric value")
  if(auto && sum(sum(order), sum(seasonal)) > 0){auto <- FALSE}

  ### STEP 1. Subsetting the data: before and after the intervention date
  ind<-dates>=int.date
  y.00<-y[!ind]
  y.01<-y[ind]

  if(!is.null(xreg)) {
    xreg0<-xreg[!ind,]
    xreg1<-xreg[ind,]

  } else {
    xreg0 <- NULL
    xreg1 <- NULL
  }

  ### STEP 2. Model estimation in the pre-intervention period
  if(auto){
     model <- do.call("auto.arima", c(list(y = ts(y.00,
                                                  frequency = frequency(y))),
                                     list(ic = ic), list(xreg = xreg0), auto.args))
   } else {
    model <- do.call("Arima", c(list(y = ts(y.00,
                                            frequency = frequency(y))),
                                list(order = order), list(seasonal = seasonal), list(xreg = xreg0),
                                arima.args))
  }

  ### STEP 3. Forecasting the counterfactual outcome in the absence of intervention
  h<-length(y.01)
  fcast<-forecast(model, xreg = xreg1, h = h, level = 1-alpha)
  mean.fcast.0<-as.numeric(fcast$mean)
  forecasted_low<-as.numeric(fcast$lower)
  forecasted_up<-as.numeric(fcast$upper)

  # Check
  if(sum(y<0, na.rm = T)==0 & sum(mean.fcast.0<0, na.rm = T)>0){print("warning: negative forecasts for a positive variable")
    print(mean.fcast.0[mean.fcast.0<0])}
  if(sum(y<0, na.rm = T)==0 & sum(model$fitted<0, na.rm = T)>0){"warning: negative fitted for a positive variable"}

  ### STEP 4. Causal effect computation: direct comparison between the observed outcome (y.01) and the
  #           predicted counterfactual (mean.fcast.0)
  causal.effect.0 <- y.01 - mean.fcast.0

  ### STEP 5. Test statistics

  ## Extracting
  d <- model$arma[6]; D <- model$arma[7]; S <- model$arma[5]
  coef <- model$coef
  sig2 <- model$sigma2
  ar  <- coef[ substr(names(coef), 1, 2) == "ar"  ]
  ma  <- coef[ substr(names(coef), 1, 2) == "ma"  ]
  sar <- coef[ substr(names(coef), 1, 3) == "sar" ]
  sma <- coef[ substr(names(coef), 1, 3) == "sma" ]

  ## MA(inf)
  par <- .sarma2larma(ar = ar, ma = ma, sar = sar, sma = sma, S = S)
  psi <- c(1, ARMAtoMA(ar = par$ar, ma = par$ma, lag.max = h-1))

  ## Stats (tau, sum.tau, avg.tau)
  # Removing NA's
  psi <- psi[!is.na(causal.effect.0)]
  tau  <- causal.effect.0[!is.na(causal.effect.0)]
  sum.tau <- cumsum(tau)
  avg.tau <- cumsum(tau) / seq(1, length(tau), 1)

  # Gaussian based inference
  norm <- .norm.inf(stat1 = tau, stat2 = sum.tau, stat3 = avg.tau, sig2 = sig2, psi = psi)

  # Bootstrap based inference
  boot <- if ( NROW(nboot) > 0 && is.finite(nboot) && nboot >= 1)
  {
    nboot <- round(nboot[1])
    .boot.inf.new(model = model, h = h, nboot = nboot, y.01 = y.01, xreg = xreg1)
    #.boot.inf(stat1 = tau, stat2 = sum.tau, stat3 = avg.tau, psi = psi, res = residuals(model),
    #          nrep = nboot)
  }
  else
  {
    NULL
  }

  ### STEP 6. Saving results
  my_list <- list(norm = norm, boot = boot, causal.effect = causal.effect.0, model = model,
                  dates = dates, int.date = int.date, y = y, xreg = xreg, forecast = mean.fcast.0, forecast_lower=forecasted_low,
                  forecast_upper = forecasted_up, alpha=alpha)
  class(my_list) <- "cArima"
  return(my_list)

}

# ------------------------------------------------------------------------------

##################################################################################
## Merge AR(p) and SAR(P); the same for MA(q) and SMA(Q)
##################################################################################

.sarma2larma <- function(ar = NULL, ma = NULL, sar = NULL, sma = NULL, S = 12)
{
  #### Adjust
  if ( NROW(ar) > 0 ) { ar <- -ar }
  if ( NROW(sar) > 0 ) { sar <- -sar }

  #### model
  list(
    ar = -.long(p = ar, ps = sar, s = S),
    ma =  .long(p = ma, ps = sma, s = S) )
}


##################################################################################
## Merge short and seasonal components
##################################################################################

.long <- function(p, ps, s)
{
  #### Settings
  np  <- NROW( p )
  nps <- NROW( ps )

  ####
  cp  <- if ( np > 0 ) { c(1, p) } else { 1 }
  cps <- if ( nps > 0 )
  {
    ind <- seq(from = s, by = s, length.out = nps)
    x1 <- numeric(s * nps)
    x1[ind] <- ps
    c(1, x1)
  }
  else
  {
    1
  }

  #### Answer
  convolve(cp, rev(cps), type = "open")[-1]
}


##################################################################################
## Compute variance of the point, cumulative and temporal average effect
##################################################################################

.norm.inf <- function(stat1, stat2, stat3, sig2, psi)
{
  #### Settings
  np <- NROW(psi)

  ## Stat 1: tau ~ Normal
  psi1 <- psi
  sd1  <- sqrt(sig2 * cumsum(psi^2))
  z1 <- (stat1 - 0) / sd1
  ## Stat 2: delta ~ Normal
  psi2 <- cumsum(psi)
  sd2  <- sqrt(sig2 * cumsum(psi2^2))
  z2 <- (stat2 - 0) / sd2
  ## Stat 3: avg.tau ~ Normal
  sd3 <- sd2 / seq(1, np, 1)
  z3 <- (stat3 - 0) / sd3
  #### Gaussian based inference
  inf <- cbind(
    tau = stat1, sd.tau = sd1,
    pvalue.tau.l = pnorm(z1), pvalue.tau.b = 2 * (1 - pnorm(abs(z1))), pvalue.tau.r = 1 - pnorm(z1),
    sum = stat2, sd.sum = sd2,
    pvalue.sum.l = pnorm(z2), pvalue.sum.b = 2 * (1 - pnorm(abs(z2))), pvalue.sum.r = 1 - pnorm(z2),
    avg = stat3, sd.avg = sd3,
    pvalue.avg.l = pnorm(z3), pvalue.avg.b = 2 * (1 - pnorm(abs(z3))), pvalue.avg.r = 1 - pnorm(z3))

  #### Answer
  list(type = "norm", inf = inf)
}

# ------------------------------------------------------------------------------
.boot.inf <- function(stat1, stat2, stat3, psi, res, nrep)
{
  #### Settings
  np <- NROW(psi)
  psi1 <- psi
  psi2 <- cumsum(psi)
  fun <- function(eps, psi)
  {
    convolve(eps, psi, conj = FALSE, type = "open")[NROW(eps):(2*NROW(eps)-1)]
  }

  #### Remove NA from res
  res <- na.omit(as.numeric(res))
  #### Bootstrap residuals
  bootm <- as.data.frame(
    matrix( sample(x = res, size = np * nrep, replace = TRUE), np, nrep ) )

  #### stat1 (arrange by row, replications by col)
  boot1 <- mapply(FUN = fun, eps = bootm, MoreArgs = list(psi = psi1))
  #### stat2 (arrange by row, replications by col)
  boot2 <- mapply(FUN = fun, eps = bootm, MoreArgs = list(psi = psi2))
  #### stat3
  boot3 <- boot2 / seq(1, np, 1)

  #### p-values
  pv1.l <- pv1.b <- pv1.r <- pv2.l <- pv2.b <- pv2.r <- pv3.l <- pv3.b <- pv3.r <- rep.int(NA, np)
  for (i in 1 : np)
  {
    stat <- stat1[i]
    boot <- boot1[i, ]
    pv1.l[i] <- mean(boot < stat)
    pv1.b[i] <- mean( boot < -abs(stat) | boot > abs(stat) )
    pv1.r[i] <- mean(stat < boot)
    stat <- stat2[i]
    boot <- boot2[i, ]
    pv2.l[i] <- mean(boot < stat)
    pv2.b[i] <- mean( boot < -abs(stat) | boot > abs(stat) )
    pv2.r[i] <- mean(stat < boot)
    stat <- stat3[i]
    boot <- boot3[i, ]
    pv3.l[i] <- mean(boot < stat)
    pv3.b[i] <- mean( boot < -abs(stat) | boot > abs(stat) )
    pv3.r[i] <- mean(stat < boot)
  }

  #### Inference
  inf <- cbind(
    tau = stat1,
    pvalue.tau.l = pv1.l, pvalue.tau.b = pv1.b, pvalue.tau.r = pv1.r,
    sum = stat2,
    pvalue.sum.l = pv2.l, pvalue.sum.b = pv2.b, pvalue.sum.r = pv2.r,
    avg = stat3,
    pvalue.avg.l = pv3.l, pvalue.avg.b = pv3.b, pvalue.avg.r = pv3.r)

  #### Answer
  list(type = "bootstrap", nrep = nrep, inf = inf)
}

.boot.inf.new <- function(model, h, nboot, y.01, xreg){

  ### Generating bootstrap simulations
  simulated <- matrix(NA, h, nboot)
  for(i in 1:nboot){
    simulated[,i] <- simulate(model,  future = TRUE, nsim = h, xreg = xreg, bootstrap = TRUE)
  }

  ### stat1
  dist1 <- y.01 - simulated
  stat1 <- rowMeans(dist1)
  sd1  <- apply(dist1, 1, sd)
  pv1.l <- apply(dist1, 1, FUN = function(x)(mean(x > 0)))
  pv1.b <- apply(dist1, 1, FUN = function(x)(2-2*max(mean(x < 0), mean(x > 0))))
  pv1.r <- apply(dist1, 1, FUN = function(x)(mean(x < 0)))
  ### stat2
  dist2 <- apply(dist1, 2, cumsum)
  stat2 <- rowMeans(dist2)
  sd2 <- apply(dist2, 1, sd)
  pv2.l <- apply(dist2, 1, FUN = function(x)(mean(x > 0)))
  pv2.b <- apply(dist2, 1, FUN = function(x)(2-2*max(mean(x < 0), mean(x > 0))))
  pv2.r <- apply(dist2, 1, FUN = function(x)(mean(x < 0)))
  ### stat3
  dist3 <- apply(dist2, 2, FUN = function(x)(x/seq(1, h, 1)))
  stat3 <- rowMeans(dist3)
  sd3 <- apply(dist3, 1, sd)
  pv3.l <- apply(dist3, 1, FUN = function(x)(mean(x > 0)))
  pv3.b <- apply(dist3, 1, FUN = function(x)(2-2*max(mean(x < 0), mean(x > 0))))
  pv3.r <- apply(dist3, 1, FUN = function(x)(mean(x < 0)))

  #### Bootstrap based inference
  inf <- cbind(
    tau = stat1, sd.tau = sd1,
    pvalue.tau.l = pv1.l, pvalue.tau.b = pv1.b, pvalue.tau.r = pv1.r,
    sum = stat2, sd.sum = sd2,
    pvalue.sum.l = pv2.l, pvalue.sum.b = pv2.b, pvalue.sum.r = pv2.r,
    avg = stat3, sd.avg = sd3,
    pvalue.avg.l = pv3.l, pvalue.avg.b = pv3.b, pvalue.avg.r = pv3.r)

  #### Relative effect
  # NOTA: dist1 è semplicemente (y.01 - simulated) dove y.01 = y.post
  # nel linguaggio che avevi usato in "impact"

  #dist4 <- dist1/simulated

  # inf.rel <- cbind(
  #  relative.effect = rowMeans(dist4),
  #  sd.relative = apply(dist4, 1, sd),
  #  pvalue.tau.l = apply(dist4, 1, FUN = function(x)(mean(x > 0))),
  #  pvalue.tau.b = apply(dist4, 1, FUN = function(x)(2-2*max(mean(x < 0), mean(x > 0)))),
  #  pvalue.tau.r = apply(dist4, 1, FUN = function(x)(mean(x < 0))))

  #### Answer
  list(type = "boot", inf = inf, boot.distrib = simulated)
}
