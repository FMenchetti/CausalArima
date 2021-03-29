######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-02-02                                                ####
####                                                                              ####
####  Content:          Causal effect of an intervention                          ####
####                                                                              ####
####  Main function :   CausalArima                                               ####
####  Dependencies:     .vartau                                                    ####
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
#' @param alpha Confidence level for the estimated causal effect. Defaults to 0.05, i.e., reporting
#'              a two-sided 95\% confidence interval.
#' @param horizon Optional vector with elements of class \code{Date}. If provided the function
#'                computes a causal effect up to each specified date.
#' @param arima.args Additional arguments to be passed to Arima.
#' @param auto.args Additional arguments to be passed to auto.arima.
#'
#' @return A list with the following components:
#' \item{effect}{The estimated temporal average effect and cumulative effect
#'               with the corresponding t-statistics. Returns a list if \code{horizon}
#'               is specified.}
#' \item{forecast.mean}{Forecasted time series in the absence of intervention.
#'                      Returns a list if \code{horizon} is specified.}
#' \item{effect.lower}{Lower bound of a (1- \code{alpha})% confidence interval
#'                     around the point effect. Returns a list if \code{horizon} is specified.}
#' \item{effect.upper}{Upper bound of a (1- \code{alpha})% confidence interval
#'                     around the point effect. Returns a list if \code{horizon} is specified.}
#' \item{effect.mean}{Estimated causal effect at each point in time after the intervention
#'                    date (point effect). Returns a list if \code{horizon} is specified.}
#' \item{dates}{The vector of dates considered in the analysis.}
#' \item{y}{The vector of observations considered in the analysis.}
#' \item{model}{The model estimated in the pre-intervention period,
#'              result of a call to \code{Arima}.}
#' \item{sigma.mean}{The estimated standard deviation of the temporal average effect.}
#' \item{sigma.sum}{The estimated standard deviation of the cumulative effect.}
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
                      int.date, alpha = 0.05, horizon = NULL, arima.args = list(), auto.args = list()){

  ### param checks
  if(class(y) != "ts") stop("y must be an object of class ts")
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
  if(alpha <0 | alpha >1) stop("alpha must be between 0 and 1")
  if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`horizon` must be a Date object")

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
  start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))

  if(auto){
    model <- do.call("auto.arima", c(list(y = ts(y.00, start = start, frequency = frequency(y.new))),
                                     list(ic = ic), list(xreg = xreg0), auto.args))
    } else {
    model <- do.call("Arima", c(list(y = ts(y.00, start = start, frequency = frequency(y.new))),
                                list(order = order), list(seasonal = seasonal), list(xreg = xreg0),
                                arima.args))
  }

  ### STEP 3. Forecasting the counterfactual outcome in the absence of intervention
  h<-length(y.01)
  fcast<-forecast(model, xreg = xreg1, h = h)
  mean.fcast.0<-as.numeric(fcast$mean)

  # Check
  if(sum(y<0, na.rm = T)==0 & sum(mean.fcast.0<0, na.rm = T)>0){print("warning: negative forecasts for a positive variable")
    print(mean.fcast.0[mean.fcast.0<0])}
  if(sum(y<0, na.rm = T)==0 & sum(model$fitted<0, na.rm = T)>0){"warning: negative fitted for a positive variable"}

  ### STEP 4. Causal effect computation: direct comparison between the observed outcome (y.01) and the
  #           predicted counterfactual (mean.fcast.0)
  causal.effect.0 <- y.01 - mean.fcast.0

  ### STEP 5. Estimation of variance, confidence intervals and tstats

  if(length(horizon) > 0){

    mean.fcast <- list()
    causal.effect <- list()
    lower <- list()
    upper <- list()
    effect <- list()
    sigma.mean <- list()
    sigma.sum <- list()

    for(i in 1:length(horizon)){
      ind <- dates[dates >= int.date] <= horizon[i]
      lh <- sum(ind)
      mean.fcast[[i]] <- mean.fcast.0[ind]
      causal.effect[[i]] <- causal.effect.0[ind]

      # Variance
      var_tau <- .vartau(model = model, nobs = lh)

      # Confidence intervals & tstats
      V.point <- var_tau$V.point ; V.sum <- var_tau$V.sum ; V.mean <- var_tau$V.mean
      u.bound.sum<- sum(causal.effect.0[ind], na.rm = T) + qnorm(1-alpha/2)*sqrt(V.sum)
      l.bound.sum<- sum(causal.effect.0[ind], na.rm = T) - qnorm(1-alpha/2)*sqrt(V.sum)
      u.bound.mean<- mean(causal.effect.0[ind], na.rm = T) + qnorm(1-alpha/2)*sqrt(V.mean)
      l.bound.mean<- mean(causal.effect.0[ind], na.rm = T) - qnorm(1-alpha/2)*sqrt(V.mean)
      upper[[i]]<- causal.effect.0[ind] +qnorm(1-alpha/2)*sqrt(V.point)
      lower[[i]]<- causal.effect.0[ind] - qnorm(1-alpha/2)*sqrt(V.point)
      sigma.mean[[i]] <- sqrt(V.mean)
      sigma.sum[[i]] <- sqrt(V.sum)

      effect[[i]]<-cbind(mean = mean(causal.effect.0[ind], na.rm = T), lower = l.bound.mean, upper = u.bound.mean,
                    sum = sum(causal.effect.0[ind], na.rm = T), lower = l.bound.sum, upper = u.bound.sum,
                    tstat_mean = mean(causal.effect.0[ind], na.rm = T)/sqrt(V.mean),
                    tstat_sum = sum(causal.effect.0[ind], na.rm = T)/sqrt(V.sum))
    }

  } else {

    mean.fcast <- mean.fcast.0
    causal.effect <- causal.effect.0

    # Variance
    var_tau <- .vartau(model = model, nobs = h)

    # Confidence intervals & tstats
    V.point <- var_tau$V.point ; V.sum <- var_tau$V.sum ; V.mean <- var_tau$V.mean
    u.bound.sum<- sum(causal.effect, na.rm = T) + qnorm(1-alpha/2)*sqrt(V.sum)
    l.bound.sum<- sum(causal.effect, na.rm = T) - qnorm(1-alpha/2)*sqrt(V.sum)
    u.bound.mean<- mean(causal.effect, na.rm = T) + qnorm(1-alpha/2)*sqrt(V.mean)
    l.bound.mean<- mean(causal.effect, na.rm = T) - qnorm(1-alpha/2)*sqrt(V.mean)
    upper<-causal.effect +qnorm(1-alpha/2)*sqrt(V.point)
    lower<-causal.effect - qnorm(1-alpha/2)*sqrt(V.point)
    sigma.mean <- sqrt(V.mean)
    sigma.sum <- sqrt(V.sum)

    effect<-cbind(mean = mean(causal.effect, na.rm = T), lower = l.bound.mean, upper = u.bound.mean,
                  sum = sum(causal.effect, na.rm = T), lower = l.bound.sum, upper = u.bound.sum,
                  tstat_mean = mean(causal.effect, na.rm = T)/sqrt(V.mean),
                  tstat_sum = sum(causal.effect, na.rm = T)/sqrt(V.sum))
  }

  ### Returning estimates
  my_list<-list(effect = effect, forecast.mean = mean.fcast, effect.lower = lower,
                effect.upper = upper, effect.mean = causal.effect, dates = dates, y = y, model = model,
                sigma.mean = sigma.mean, sigma.sum = sigma.sum)
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
## Compute variance of the point, cumulative and temporal average effect (see pdf)
##################################################################################

.vartau <- function(model, nobs){

  if(nobs == 1){

    V.point <- V.sum <- V.mean <- var(model$residuals, na.rm = T)

  } else {

    # Defining K, getting frequency & residuals and extracting 'sar','sma','ar' and 'ma' coefficients
    K<- nobs

    if(is.na(arimaorder(model)["Frequency"])){
      frequency <- 1
    } else {
      frequency <- arimaorder(model)["Frequency"]
    }

    residuals<-model$residuals

    coeff<-attr(model$var.coef, "dimnames")[[1]] # chr vect with names of coefficients
    sar<-model$coef[grep("sar",coeff)]           # extracting "sar" coefficients
    sma<-model$coef[grep("sma",coeff)]           # extracting "sma" coefficients
    if(length(sar) == 0){coeff <-coeff} else {
      coeff<-coeff[-grep("sar",coeff)]
    }
    if(length(sma) == 0){coeff <-coeff} else {
      coeff<-coeff[-grep("sma",coeff)]
    }
    ar<-model$coef[grep("ar",coeff)]             # extracting "ar" coefficients
    ma<-model$coef[grep("ma",coeff)]             # extracting "ma" coefficients

    # converting to long ARMA
    larma<-.sarma2larma(ar = ar , ma = ma, sar = sar, sma = sma, S=frequency)

    # converting to MA
    arma.to.ma<-ARMAtoMA(ar = larma$ar, ma=larma$ma, lag.max = nobs)

    # variance estimation
    sum.phi<-NULL
    for(k in 1:(K-1)){
      sum.phi[k]<-(1+sum(arma.to.ma[1:(K-k)]))^2
    }
    sum.phi[K]<-1                             # This is because at time K we have just phi_0=1

    sum.phi.2<-NULL
    for(k in 2:(K)){
      sum.phi.2[k]<-1+sum((arma.to.ma[1:(k-1)])^2)
    }
    sum.phi.2[1]<-1

    V.point<-var(residuals, na.rm = T)*sum.phi.2
    V.sum<-var(residuals, na.rm = T)*sum(sum.phi)
    V.mean<-(1/K^2)*(var(residuals, na.rm = T)*sum(sum.phi))

  }

  # returning estimates
  list <- list(V.point = V.point, V.sum = V.sum, V.mean = V.mean)
  return(list)
}
