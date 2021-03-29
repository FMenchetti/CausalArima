######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-02-17                                                ####
####                                                                              ####
####  Content:          Causal effect on a multivariate time series               ####
####                                                                              ####
####  Main function :   CausalVAR                                                 ####
####  Dependencies:     .mvartau                                                  ####
######################################################################################
######################################################################################


#' Causal effect of an intervention on a multivariate time series
#'
#' The function estimates the causal effect of an intervention on a multivariate
#' time series. It uses vector autoregressive models (VAR) to learn the time
#' dynamics of the series in the pre-intervention period; then, it performs a prediction
#' step (using external regressors, if provided) to forecast the series in the absence
#' of intervention. The comparison between the observed series post-intervention
#' and the forecasted series gives the causal effect. The function also computes
#' the cumulative and the temporal average effect up to each time horizon given in
#' \code{horizon} (if NULL, those effects are estimated up to the end of the analysis period).
#'
#' @param y Matrix (or data.frame) of observations (missing values are not allowed).
#' @param exogen Optional vector, matrix or data.frame of regressors to be included in the model.
#' @param dates Vector of dates of length t (with elements of class \code{Date}) corresponding to the
#'              observations in y.
#' @param int.date Date of the intervention (must be of class \code{Date}).
#' @param alpha Confidence level for the estimated causal effect. Defaults to 0.05, i.e., reporting
#'              a two-sided 95\% confidence interval.
#' @param horizon Optional vector with elements of class \code{Date}. If provided the function
#'                computes a causal effect up to each specified date.
#' @param VAR.args Additional arguments to be passed to VAR. Optional, if not specified, the
#'                 estimation of the model in the pre-intervention period is done using
#'                 the default values of \code{VAR} in the \code{vars} package.
#'
#' @return
#' @export
#'
#' @examples
#' # work in progress

CausalVAR <- function(y, exogen = NULL, dates, int.date, alpha = 0.05, horizon = NULL, VAR.args = list()){

  ### param checks
  if(!is.matrix(y) && !is.data.frame(y)) stop("`y` must be a matrix or a data.frame")
  if(!missing(exogen)) {
    if(!is.matrix(exogen) && !is.data.frame(exogen) && !is.numeric(exogen))
      stop("`exogen` must be a numeric vector, matrix or data.frame")
    exogen <- as.matrix(exogen)
    if(nrow(exogen) != nrow(y)) stop("nrow(xreg) != nrow(y)")
  }
  if(!any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`dates` must be a vector of class Date")
  if(length(dates) != nrow(y)) stop("length(dates) != nrow(y)")
  if(length(int.date) != 1 || !any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`int.date` must be a Date of length 1")
  if(alpha <0 | alpha >1) stop("alpha must be between 0 and 1")
  if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`horizon` must be a Date object")

  ### STEP 1. Subsetting the data: before and after the intervention date
  ind<-dates>=int.date
  y.00<-y[!ind,]
  y.01<-y[ind,]

  if(!is.null(exogen)) {
    exogen0<-exogen[!ind,]
    exogen1<-exogen[ind,]

  } else {
    exogen0 <- NULL
    exogen1 <- NULL
  }

  ### STEP 2. Model estimation in the pre-intervention period
  var_model <- do.call("VAR", c(list(y = y.00), list(exogen = exogen0), VAR.args))

  ### STEP 3. Forecasting the counterfactual outcome in the absence of intervention
  h <- nrow(y.01)
  pred <- predict(var_model, n.ahead = h, dumvar = exogen1)
  mean.fcast.0 <- matrix(unlist(lapply(pred$fcst, function(x) x[,"fcst"])), nrow = h, ncol = dim(y)[2])

  ### STEP 4. Causal effect computation: direct comparison between the observed outcome (y.01) and the
  #           predicted counterfactual (mean.fcast.0)
  causal.effect.0 <- y.01 - mean.fcast.0

  ### STEP 5. Estimation of variance, confidence intervals and tstats

  if(!is.null(horizon)){
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
      mean.fcast[[i]] <- mean.fcast.0[ind,]
      causal.effect[[i]] <- causal.effect.0[ind,]

      # Variance
      var_tau <- .mvartau(var_model = var_model, nobs = lh)

      # Confidence intervals & tstats CONTROLLA
      V.point <- var_tau$V.point ; V.sum <- diag(var_tau$VCV.sum) ; V.mean <- diag(var_tau$VCV.mean)
      u.bound.sum<- colSums(causal.effect.0[ind,]) + qnorm(1-alpha/2)*sqrt(V.sum)
      l.bound.sum<- colSums(causal.effect.0[ind,]) - qnorm(1-alpha/2)*sqrt(V.sum)
      u.bound.mean<- colMeans(causal.effect.0[ind,]) + qnorm(1-alpha/2)*sqrt(V.mean)
      l.bound.mean<- colMeans(causal.effect.0[ind,]) - qnorm(1-alpha/2)*sqrt(V.mean)
      upper[[i]]<- causal.effect.0[ind,] + qnorm(1-alpha/2)*sqrt(V.point)
      lower[[i]]<- causal.effect.0[ind,] - qnorm(1-alpha/2)*sqrt(V.point)
      sigma.mean[[i]] <- sqrt(V.mean)
      sigma.sum[[i]] <- sqrt(V.sum)

      effect[[i]]<-cbind(mean = colMeans(causal.effect.0[ind,]), lower = l.bound.mean, upper = u.bound.mean,
                         sum = colSums(causal.effect.0[ind,]), lower = l.bound.sum, upper = u.bound.sum,
                         tstat_mean = colMeans(causal.effect.0[ind,])/sqrt(V.mean),
                         tstat_sum = colSums(causal.effect.0[ind,])/sqrt(V.sum))
    }
  } else {

    mean.fcast <- mean.fcast.0
    causal.effect <- causal.effect.0

    # Variance
    var_tau <- .mvartau(var_model = var_model, nobs = h)

    # Confidence intervals & tstats
    V.point <- var_tau$V.point ; V.sum <- diag(var_tau$VCV.sum) ; V.mean <- diag(var_tau$VCV.mean)
    u.bound.sum<- colSums(causal.effect) + qnorm(1-alpha/2)*sqrt(V.sum)
    l.bound.sum<- colSums(causal.effect) - qnorm(1-alpha/2)*sqrt(V.sum)
    u.bound.mean<- colMeans(causal.effect) + qnorm(1-alpha/2)*sqrt(V.mean)
    l.bound.mean<- colMeans(causal.effect) - qnorm(1-alpha/2)*sqrt(V.mean)
    upper<-causal.effect + qnorm(1-alpha/2)*sqrt(V.point)
    lower<-causal.effect - qnorm(1-alpha/2)*sqrt(V.point)
    sigma.mean <- sqrt(V.mean)
    sigma.sum <- sqrt(V.sum)

    effect<-cbind(mean = colMeans(causal.effect), lower = l.bound.mean, upper = u.bound.mean,
                  sum = colSums(causal.effect), lower = l.bound.sum, upper = u.bound.sum,
                  tstat_mean = colMeans(causal.effect)/sqrt(V.mean),
                  tstat_sum = colSums(causal.effect)/sqrt(V.sum))
  }

  ### Returning estimates
  my_list<-list(effect = effect, forecast.mean = mean.fcast, effect.lower = lower,
                effect.upper = upper, effect.mean = causal.effect, dates = dates, y = y, model = var_model,
                sigma.mean = sigma.mean, sigma.sum = sigma.sum)
  class(my_list) <- "cVAR"
  return(my_list)
}

#-------------------------------------------------------------------------------------------

.mvartau <- function(var_model, nobs){

  # Defining H, getting dimension and the variance-covariance matrix of residuals
  H <- nobs
  d <- dim(var_model$y)[2]
  varcovar <- summary(var_model)$covres

  # Computing psi (i.e., the estimated coefficient matrices of the moving average
  # representation of a stable VAR(p))
  psi <- do.call("Phi", list(var_model, nstep = H - 1))

  # Variance of the causal effect at each point in time after the intervention
  VCV.point <- array(NA, c(d,d,H))
  VCV.point[,,1] <- psi[,,1]%*%varcovar%*%t(psi[,,1])

  if(H > 1){
    for(j in 2:H){
      VCV.point[,,j] <- VCV.point[,,j-1] + psi[,,j]%*%varcovar%*%t(psi[,,j])
    }
  }

  V.point <- matrix(NA, nrow = H, ncol = d)
  for(i in 1:d){
    V.point[,i] <- VCV.point[i,i, ]
  }

  # Variance of the cumulative effect (i.e, the sum of the point effects
  # up to a pre-specified point in time)
  sum.phi <- array(NA, c(d,d,H))
  for(j in 1:H){
    sum.phi[,,H+1-j] <- apply(psi[,,1:j], c(1,2), sum)%*%varcovar%*%t(apply(psi[,,1:j], c(1,2), sum))
  }

  VCV.sum <- apply(sum.phi, c(1,2), sum)

  # Variance of the temporal average effect
  VCV.mean <- (1/H^2)*VCV.sum

  # Returning estimates
  list <- list(V.point = V.point, VCV.point = VCV.point, VCV.sum = VCV.sum, VCV.mean = VCV.mean)
  return(list)

}
