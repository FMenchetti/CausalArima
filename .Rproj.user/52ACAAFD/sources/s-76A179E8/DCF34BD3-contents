######################################################################################
######################################################################################
####  Authors:           Cipollini F., Menchetti F.
####                                                                              ####
####  Date last update: April 2022                                                ####
####                                                                              ####
####  Content:          Causal effect of an intervention                          ####
####                                                                              ####
####  Main function :   CausalArima                                               ####
####  Dependencies:     .boot.inf                                                 ####
####                    .norm.inf                                                 ####
####                    .long                                                     ####
####                    .seas2poly                                                ####
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
#' @param dates Vector of dates of length t (with elements of class \code{Date}) corresponding to the
#'              observations in y.
#' @param int.date Date of the intervention (must be of class \code{Date}).
#' @param auto If set to TRUE, \code{auto.arima} returns the best fitting model in the pre-intervention period
#'             according to the selected \code{ic}. Defaults to TRUE.
#' @param order Order of the non-seasonal part of the ARIMA model, (p, d, q). Required when \code{auto} is set to FALSE.
#' @param seasonal Order of the seasonal part of the ARIMA model, (P, D, Q). Defaults to \code{c(0, 0, 0)}.
#' @param ic Information criterion to use for model selection. Possible values in c("aicc", "aic", "bic").
#'           Defaults to \code{"aic"}.
#' @param xreg  Optional vector, matrix or data.frame of regressors to be included in the model.
#'              It must have the same number of rows as y.
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
CausalArima <-function(y, dates, int.date, auto = TRUE, order = c(0, 0, 0), seasonal = c(0, 0, 0), ic = "aic", xreg = NULL,
                      arima.args = list(), auto.args = list(), nboot = NULL, alpha = 0.05){

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
  if((!missing(nboot) & !is.null(nboot)) && (!is.numeric(nboot) | nboot <= 0)) stop("`nboot` must be a positive numeric value")
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
  tau  <- causal.effect.0[!is.na(causal.effect.0)]
  delta <- cumsum(tau)
  avg <- cumsum(tau) / seq(1, length(tau), 1)

  # Handling covariates
  if(!is.null(xreg)){
    xreg1 <- as.matrix(xreg1)
    xreg1 <- apply(xreg1, 2, function(x)(x[!is.na(causal.effect.0)]))
  }
  if("intercept" %in% names(model$coef)){
    c <- rep(1, times = NROW(xreg1))
    xreg1 <- cbind(c, xreg1)
    model$xreg <- cbind(c = rep(1, NROW(model$xreg)), model$xreg)
  }

  # Normal based inference
  norm <- .norm.inf(tau = tau, delta = delta, avg = avg, fit = model,
                    xreg = xreg1)
  # Bootstrap based inference
  boot <- if ( NROW(nboot) > 0 && is.finite(nboot) && nboot >= 1)
  {
    nboot <- round(nboot[1])

    .boot.inf(tau = tau, delta = delta, avg = avg, fit = model,
              nrep = nboot, xreg = xreg1)
  }
  else
  {
    NULL
  }

  ### STEP 6. Saving results
  my_list <- list(norm = norm, boot = boot, causal.effect = as.numeric(causal.effect.0), model = model,
                  dates = dates, int.date = int.date, y = as.numeric(y), xreg = xreg, forecast = mean.fcast.0, forecast_lower=forecasted_low,
                  forecast_upper = forecasted_up, alpha = alpha)
  class(my_list) <- "cArima"
  return(my_list)

}


##################################################################################
## Compute variance of the point, cumulative and temporal average effect
##################################################################################

.norm.inf <- function(tau, delta, avg, fit, xreg = NULL)
{
  #### Settings
  nobs <- NROW(fit$x)
  k    <- NROW(tau)
  nk   <- nobs + k
  x1   <- fit$xreg
  x2   <- xreg

  #### Extract ARMA parameters
  model <- .Arima.parm(fit)
  #### Remap to long format
  model.l <- .parm(model = model)
  #### Derive the ACF
  acvf <- ltsa::tacvfARMA(phi = model.l$ar, theta = -model.l$ma, maxLag = nk-1,
                          sigma2 = model$sigma2)
  sigma2w <- acvf[1]
  acf  <- acvf / sigma2w

  #### Make Toeplitz matrix and extract
  R    <- toeplitz(acf)
  ind1 <- 1 : nobs
  ind2 <- (nobs+1) : nk
  R11  <- R[ind1, ind1]
  R21  <- R[ind2, ind1]
  R12  <- R[ind1, ind2]
  R22  <- R[ind2, ind2]
  rm(R)
  #### Compute
  C11  <- chol(R11)
  R12t <- backsolve(r = C11, x = R12, upper.tri = TRUE, transpose = TRUE)

  if(is.null(xreg)){
    At <- t(R12t)
  } else {
  x1t  <- backsolve(r = C11, x = x1, upper.tri = TRUE, transpose = TRUE)
  At <- (x2 - crossprod(R12t, x1t)) %*% solve( crossprod(x1t), t(x1t) ) + t(R12t)
  }

  x2 <- At %*% R12t
  vc <- sigma2w * ( R22 - x2 - t(x2) + tcrossprod(At) )

  #### Answer
  v.tau <- diag(vc)
  v.delta <- v.tau
  for (i in 2 : k )
  {
    v.delta[i] <- v.delta[i-1] + vc[i,i] + 2 * sum(vc[1:(i-1),i])
  }
  v.avg <- v.delta / (seq(1, k , 1)^2)
  z.tau <- tau / sqrt(v.tau)
  z.delta <- delta / sqrt(v.delta)
  z.avg <- avg / sqrt(v.avg)

  #### Answer
  inf <- cbind(
    tau = tau, sd.tau = sqrt(v.tau),
    pvalue.tau.l = pnorm(z.tau), pvalue.tau.b = 2 * (1 - pnorm(abs(z.tau))), pvalue.tau.r = 1 - pnorm(z.tau),
    sum = delta, sd.sum = sqrt(v.delta),
    pvalue.sum.l = pnorm(z.delta), pvalue.sum.b = 2 * (1 - pnorm(abs(z.delta))), pvalue.sum.r = 1 - pnorm(z.delta),
    avg = avg, sd.avg = sqrt(v.avg),
    pvalue.avg.l = pnorm(z.avg), pvalue.avg.b = 2 * (1 - pnorm(abs(z.avg))), pvalue.avg.r = 1 - pnorm(z.avg))

  list(type = "norm", inf = inf)

}

# ------------------------------------------------------------------------------

.boot.inf <- function(tau, delta, avg, nrep, fit, xreg)
{
  #### Settings
  nobs <- NROW(fit$x)
  k    <- NROW(tau)
  nk   <- nobs + k
  x1   <- fit$xreg
  x2   <- xreg
  res <- residuals(fit)

  #### Remove NA from res
  res <- na.omit(as.numeric(res))
  #### Bootstrap residuals
  bootm <- as.data.frame(
    matrix( sample(x = res, size = nk * nrep, replace = TRUE), nk, nrep ) )

  #### Extract ARMA parameters
  model <- .Arima.parm(fit)
  #### Remap to long format
  model.l <- .parm(model = model)
  #### Derive the ACF
  acvf <- ltsa::tacvfARMA(phi = model.l$ar, theta = -model.l$ma, maxLag = nk-1,
                          sigma2 = model$sigma2)
  sigma2w <- acvf[1]
  acf  <- acvf / sigma2w

  #### Make Toeplitz matrix and extract
  R    <- toeplitz(acf)
  ind1 <- 1 : nobs
  ind2 <- (nobs+1) : nk
  R11  <- R[ind1, ind1]
  R12  <- R[ind1, ind2]
  rm(R)

  #### Compute
  C11  <- chol(R11)
  R12t <- backsolve(r = C11, x = R12, upper.tri = TRUE, transpose = TRUE)

  if(is.null(xreg)){
    At <- t(R12t)
  } else {
    x1t  <- backsolve(r = C11, x = x1, upper.tri = TRUE, transpose = TRUE)
    At <- (x2 - crossprod(R12t, x1t)) %*% solve( crossprod(x1t), t(x1t) ) + t(R12t)
  }

  w1 <- bootm[ind1,]
  w2 <- bootm[ind2,]
  AtC <- t(backsolve(r = C11, x = t(At)))

  #### Stats
  boot1 <- matrix(NA, k, nrep)

  for(i in 1:nrep){
    w <- .arima.sim(model = list(ar = model$ar, ma = model$ma, sar = model$sar, sma = model$sma,
                                 S = model$S), innov = ts(bootm[,i], frequency = model$S), nburn = 0)
    boot1[,i] <- w[ind2] - AtC%*%w[ind1]
  }

  boot2 <- apply(boot1, 2, cumsum)
  boot3 <- apply(boot2, 2, FUN = function(x)(x / (seq (1, k, 1))))

  #### p-values
  pv1.l <- pv1.b <- pv1.r <- pv2.l <- pv2.b <- pv2.r <- pv3.l <- pv3.b <- pv3.r <- rep.int(NA, k)
  for (i in 1 : k)
  {
    stat <- tau[i]
    boot <- boot1[i, ]
    pv1.l[i] <- mean(boot < stat)
    pv1.b[i] <- mean( boot < -abs(stat) | boot > abs(stat) )
    pv1.r[i] <- mean(stat < boot)
    stat <- delta[i]
    boot <- boot2[i, ]
    pv2.l[i] <- mean(boot < stat)
    pv2.b[i] <- mean( boot < -abs(stat) | boot > abs(stat) )
    pv2.r[i] <- mean(stat < boot)
    stat <- avg[i]
    boot <- boot3[i, ]
    pv3.l[i] <- mean(boot < stat)
    pv3.b[i] <- mean( boot < -abs(stat) | boot > abs(stat) )
    pv3.r[i] <- mean(stat < boot)
  }

  #### Inference
  inf <- cbind(
    tau = tau,
    pvalue.tau.l = pv1.l, pvalue.tau.b = pv1.b, pvalue.tau.r = pv1.r,
    sum = delta,
    pvalue.sum.l = pv2.l, pvalue.sum.b = pv2.b, pvalue.sum.r = pv2.r,
    avg = avg,
    pvalue.avg.l = pv3.l, pvalue.avg.b = pv3.b, pvalue.avg.r = pv3.r)

  #### Answer
  list(type = "bootstrap", nrep = nrep, inf = inf)
}

########################################################################################
## Functions to generate replications from a give ARIMA/ARIMA model
########################################################################################

.arima.sim <- function(
    model = list(d = 0, D = 0, S = 12,
                 constant = NULL, ar = NULL, ma = NULL, sar = NULL, sma = NULL),
    innov, nburn)
{
  #### Orders
  p  <- NROW(model$ar)
  d <- ifelse( NROW(model$d) > 0, model$d[1], 0)
  q <- NROW(model$ma)
  ps <- NROW(model$sar)
  ds <- ifelse( NROW(model$D) > 0, model$D[1], 0)
  qs <- NROW(model$sma)
  S <- ifelse( NROW(model$S) > 0, model$S[1], 12)
  constant <- model$constant

  #### Long form of parameters
  model <- .sarma2larma(ar = model$ar, ma = model$ma,
                        sar = model$sar, sma = model$sma, S = S)
  model$ar <- model$ar[-1]
  model$ma <- model$ma[-1]

  #### Check roots
  if (p > 0)
  {
    minroots <- min(abs(polyroot(c(1, -model$ar))))
    if (minroots <= 1) { warning("AR side not stationary") }
  }
  if (q > 0)
  {
    minroots <- min(abs(polyroot(c(1, model$ma))))
    if (minroots <= 1) { warning("MA side not invertible") }
  }

  #### Simulate ARMA
  x <- .arma.sim(model = model, innov = innov)

  #### Add mean
  constant <- constant[1]
  if (NROW(constant) > 0 && d + ds <= 1)
  {
    x <- constant / (1 - sum(model$ar)) + x
  }

  #### d
  if (d > 0)
  {
    x <- stats::diffinv(x, lag = 1, differences = d)
  }
  if (ds > 0)
  {
    x <- stats::diffinv(x, lag = S, differences = ds)
  }

  #### Answer
  x[(nburn + 1) : NROW(innov)]
}

#--------------------------------------------------------------------------------------

.arma.sim <-
  function(model, innov = innov)
  {
    #### Extract orders
    p <- length(model$ar)
    q <- length(model$ma)
    #### Copy
    x <- innov
    #### MA
    if (length(model$ma))
    {
      x <- stats::filter(x, c(1, model$ma), sides = 1L)
      x[seq_along(model$ma)] <- 0
    }
    #### AR
    if (length(model$ar))
    {
      x <- stats::filter(x, model$ar, method = "recursive")
    }
    #### Answer
    x
  }
# ------------------------------------------------------------------------------


##################################################################################
## Merge AR(p) and SAR(P); the same for MA(q) and SMA(Q)
##################################################################################

.sarma2larma <- function(ar = NULL, ma = NULL, sar = NULL, sma = NULL, S )
{
  #### Adjust
  if ( NROW(ar) > 0 ) { ar <- -ar }
  if ( NROW(sar) > 0 ) { sar <- -sar }

  #### model
  list(
    ar = -.long2(p = ar, ps = sar, s = S),
    ma =  .long2(p = ma, ps = sma, s = S) )
}


##################################################################################
## Merge short and seasonal components
##################################################################################

.seas2poly <- function(ps, s)
{
  ####
  nps <- NROW(ps)
  if ( nps > 0 )
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
}

# ------------------------------------------------------------------------------
.long2 <- function(p, ps, s)
{
  #### Settings
  np  <- NROW( p )
  nps <- NROW( ps )

  ####
  cp  <- if ( np > 0 ) { c(1, p) } else { 1 }
  cps <- .seas2poly(ps, s)

  #### Answer
  pracma::polymul(cp, cps)
}


##################################################################################
## Extracting ARMA parameters from fitted model
##################################################################################

.Arima.parm <- function(fit)
{
  #### Extract
  d <- fit$arma[6]; D <- fit$arma[7]; S <- fit$arma[5]
  coef <- fit$coef
  ind.ar   <- substr(names(coef), 1, 2) == "ar"
  ind.ma   <- substr(names(coef), 1, 2) == "ma"
  ind.sar  <- substr(names(coef), 1, 3) == "sar"
  ind.sma  <- substr(names(coef), 1, 3) == "sma"
  ind.beta <- !(ind.ar | ind.ma | ind.sar | ind.sma)
  ar  <- coef[ ind.ar ]
  ma  <- coef[ ind.ma ]
  sar <- coef[ ind.sar ]
  sma <- coef[ ind.sma ]
  beta <- coef[ ind.beta ]
  sig2 <- fit$sigma2
  vcov <- fit$var.coef
  vcov.xreg <- vcov[ind.beta, ind.beta]
  vcov.arma <- vcov[!ind.beta, !ind.beta]
  #### Answer
  list(d = d, D = D, S = S, ar = ar, ma = ma, sar = sar, sma = sma,
       xreg = beta, sigma2 = sig2,
       vcov.arma = vcov.arma, vcov.xreg = vcov.xreg)
}

# ------------------------------------------------------------------------------
.parm <- function(

  model = list(d = 0, D = 0, S = 12,
               ar = NULL, ma = NULL, sar = NULL, sma = NULL))
{
  #### Orders
  p <- NROW(model$ar)
  d <- ifelse( NROW(model$d) > 0, model$d[1], 0)
  q <- NROW(model$ma)
  ps <- NROW(model$sar)
  ds <- ifelse( NROW(model$D) > 0, model$D[1], 0)
  qs <- NROW(model$sma)
  S <- ifelse( NROW(model$S) > 0, model$S[1], 12)

  #### Long form of parameters
  ar.ns <- .diff2poly(d = d, ds = ds, s = S)
  if ( NROW(model$ar) > 0 ) { model$ar <- -model$ar }
  if ( NROW(model$sar) > 0 ) { model$sar <- -model$sar }
  ar.s <- .long2(p = model$ar, ps = model$sar, s = S)
  ar <- -pracma::polymul(ar.ns, ar.s)[-1]
  ma <- .long2(p = model$ma, ps = model$sma, s = S)[-1]

  #### Answer
  list(ar = ar, ma = ma)
}

# ------------------------------------------------------------------------------

.diff2poly <- function(d, ds, s)
{
  #### init
  x1 <- 1
  #### d
  ind <- if ( d > 0 ) { 1 : d } else { NULL }
  for (i in ind)
  {
    x2 <- c(1, -1)
    x1 <- pracma::polymul(x1, x2)
  }
  #### ds
  ind <- if ( ds > 0 ) { 1 : ds } else { NULL }
  for (i in ind)
  {
    x2 <- c(1, numeric(s-1), -1)
    x1 <- pracma::polymul(x1, x2)
  }
  #### Answer
  x1
}

