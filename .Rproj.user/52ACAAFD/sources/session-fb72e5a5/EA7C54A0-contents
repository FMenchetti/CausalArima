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
#' The function estimates the causal effect of an intervention on a time series by using
#' the best-fitting ARIMA model on pre-intervention data to forecast what would have
#' happened afterwards in the absence of the intervention. The prediction step can be
#' improved by adding covariates that are linked to the outcome variable. Then, the
#' causal effect is estimated by difference between the observed outcome post-intervention
#' ad the prediction.
#'
#' @details
#' The function accepts both \code{ts} and \code{numeric} objects. In the latter case, y
#' is internally transformed in a \code{ts} object whose frequency is set by
#' \code{findfrequency} from the \code{forecast} package.
#'
#' \code{NA} values are allowed in y but not in \code{xreg}. Users are then left to
#' choose their preferred technique to deal with missing data in the outcome (e.g., imputation,
#' deletion, analysis etc.)
#'
#' @references
#' Rubin, D. B. (1976). Inference and missing data. Biometrika, 63(3), 581-592.
#'
#' Little, R. J. A., and Rubin, D.B. (1987). Statistical Analysis with Missing Data.
#' New York: John Wiley & Sons.
#'
#' Imbens, G. W., and  and Rubin, D.B. (2015). Causal Inference in Statistics, Social,
#' and Biomedical Sciences. Cambridge, U.K.: Cambridge University Press.
#'
#' @param y Vector of observations, univariate time series of class \code{ts} or \code{numeric}.
#' @param auto If set to TRUE, \code{auto.arima} returns the best fitting model in the pre-intervention period
#'             according to the selected \code{ic}. Defaults to TRUE.
#' @param order Order of the non-seasonal part of the ARIMA model, (p, d, q). Required when \code{auto} is set to FALSE.
#' @param seasonal Order of the seasonal part of the ARIMA model, (P, D, Q). Defaults to \code{c(0, 0, 0)}.
#' @param ic Information criterion to use for model selection. Possible values in c("aicc", "aic", "bic").
#'           Defaults to \code{"aic"}.
#' @param xreg  Optional vector, matrix or data.frame of regressors to be included in the model.
#'              It must have the same number of rows as y.
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
#'             intervention with their standard deviations and p-values (left-sided, right-sided, bidirectional)
#'             estimated under the assumption of Normally distributed residuals.}
#' \item{boot}{The inferred point, cumulative and temporal average effects at each point in time after the
#'             intervention with their standard deviations and p-values (left-sided, right-sided, bidirectional) estimated
#'             by bootstrap.}
#' \item{causal.effect}{Estimated causal effect at each point in time after the intervention
#'                    date (point effect).}
#' \item{model}{The model estimated in the pre-intervention period,
#'              result of a call to \code{Arima}.}
#' \item{dates}{The vector of dates considered in the analysis.}
#' \item{int.date}{The intervention date.}
#' \item{y}{The vector of observations considered in the analysis.}
#' \item{xreg}{The covariates, if any, used in the analysis.}
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
#' horizon <- as.Date(c("2015-04-10", "2015-04-20"))
#' y.new <- y ; y.new[dates >= int.date] <- y.new[dates >= int.date]*1.25
#'
#' # Plot
#' plot(y = y.new, x = dates, type = "l", col = "cadetblue", xlim = c(as.Date("2014-10-01"), tail(dates, 1)))
#' lines(y = y, x = dates, col = "orange")
#'
#' # Causal effect estimation
#' ce <- CausalArima(y = y.new, dates = dates, int.date = int.date)
#'
#' ## Example 2 (daily data, with predictors)
#' # Loading data and setting dates
#' data(sales)
#' y <- sales[, "Sales"]
#' dates <- as.Date(sales[, "Dates"])
#' int.date <- as.Date("2018-10-04")
#' xreg <- sales[, "Price"]
#'
#' # Plot
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow = c(1,2))
#' plot(y = y, x = dates, type = "l", main = "Time series of daily sales")
#' abline(v = int.date, col = "red"); Acf(y, main = "Acf sales")
#' par(oldpar)
#'
#' # Causal effect estimation
#' # The autocorrelation function indicates a weekly sesonal pattern
#' ce <- CausalArima(y = ts(y, frequency = 7), xreg = xreg, int.date = int.date,
#'                   dates = dates, nboot = 100)
CausalArima<-function(y, auto = TRUE, order = c(0, 0, 0), seasonal = c(0, 0, 0), ic = "aic", xreg = NULL, dates,
                      int.date, arima.args = list(), auto.args = list(), nboot = NULL, alpha = 0.05){

  ### param checks
  if(class(y) != "ts" & !is.numeric(y)) stop("y must be numeric or ts")
  if(!is.ts(y)){
    y <- ts(y, frequency = findfrequency(y))
  }
  if(!is.null(xreg)) {
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
  y.00<-ts(y[!ind], frequency = frequency(y))
  y.01<-ts(y[ind], frequency = frequency(y))

  if(!is.null(xreg)) {
    xreg0<-xreg[!ind,]
    xreg1<-xreg[ind,]

  } else {
    xreg0 <- NULL
    xreg1 <- NULL
  }

  ### STEP 2. Model estimation in the pre-intervention period
  if(auto){
     model <- do.call("auto.arima", c(list(y = y.00),
                                      list(ic = ic), list(xreg = xreg0), auto.args))
   } else {
    model <- do.call("Arima", c(list(y = y.00),
                                list(order = order), list(seasonal = seasonal), list(xreg = xreg0),
                                arima.args))
  }

  ### STEP 3. Forecasting the counterfactual outcome in the absence of intervention
  h<-length(y.01)
  fcast<-forecast(model, xreg = xreg1, h = h, level = 1-alpha)
  mean.fcast.0<-as.numeric(fcast$mean)
  forecasted_low<-as.numeric(fcast$lower)
  forecasted_up<-as.numeric(fcast$upper)

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
    .boot.inf(model = model, h = h, nboot = nboot, y.01 = y.01, xreg = xreg1)

  }
  else
  {
    NULL
  }

  ### STEP 6. Saving results
  my_list <- list(norm = norm, boot = boot, causal.effect = as.numeric(causal.effect.0), model = model,
                  dates = dates, int.date = int.date, y = as.numeric(y), xreg = xreg, forecast = mean.fcast.0, forecast_lower=forecasted_low,
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
.boot.inf <- function(model, h, nboot, y.01, xreg){

  ### Generating bootstrap simulations
  simulated <- matrix(NA, h, nboot)
  for(i in 1:nboot){
    simulated[,i] <- simulate(model,  future = TRUE, nsim = h, xreg = xreg, bootstrap = TRUE)
  }

  ### stat1
  # removing rows corresponding to missing obs in y.01
  dist1 <- as.numeric(y.01) - simulated
  dist1 <- dist1[!is.na(y.01), ]
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
  dist3 <- apply(dist2, 2, FUN = function(x)(x/seq(1, dim(dist2)[1], 1)))
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

  #### Answer
  list(type = "boot", inf = inf, boot.distrib = simulated)
}
