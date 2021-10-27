######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-09-28                                                ####
####                                                                              ####
####  Content:          Table method for object of class cArima                   ####
####                                                                              ####
####  Main function :   impact                                                    ####
####  Dependencies:     .impact_summary                                            ####
####                    .format_impact                                             ####
####                                                                              ####
######################################################################################
######################################################################################

#' Function to create a table of the estimated model coefficients from a call to CausalArima
#'
#' @param x Object of class \code{cArima}.
#' @param format Required format for the table. Possible values in \code{c("numeric", "html", "latex")}.
#' @param n Number of bootstrap simulations.#'
#' @param alpha ?
#' @param bootstraping ?
#' @param horizon Optional vector with elements of class Date. If provided, the function returns
#'                the estimated effects at the given time horizons.
#' @param ... Optional arguments passed to other methods.
#'
#'
#' @return A list with the following components:
#' \item{impact}{Estimated point, cumulative and temporal average effect assuming Normality
#'              of the error terms.}
#' \item{impact_boot}{Estimated point, cumulative and temporal average effect by bootstrap.}
#' \item{arima}{Arima order, coefficient estimates and accuracy measures for the estimated model
#'              in the pre-intervention period.}
#' @export
#'
#' @examples
#' ## Example 2 (weekly data, with predictors)
#' # Generating a time series of length 800 and a vector of dates
#' y <- rnorm(800, sd = 1)
#' dates <- seq.Date(from = as.Date("2005-01-01"), by = "week", length.out = 800)
#'
#' # Generating predictors
#' x1 <- rnorm(800, mean = 2, sd = 0.5)
#' x2 <- rnorm(800, mean = 3, sd = 0.5)
#' y <- y -2*x1 + x2
#'
#' # Adding a fictional intervention
#' int.date <- as.Date("2019-05-11")
#' horizon <- c(as.Date("2019-12-07"), as.Date("2020-02-15"), as.Date("2020-04-25"))
#' y.new <- y ; y.new[dates >= int.date] <- y.new[dates >= int.date]*1.40
#'
#' # Causal effect estimation
#' start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
#' ce <- CausalArima(y = ts(y.new, start = start, frequency = 1), auto = TRUE, ic = "aic", dates = dates, xreg = data.frame(x1,x2), int.date = int.date)
#'
#' # Table of the estimated temporal average effects
#' impact(ce)
#'
impact <- function(x, format="numeric", nboot=10, alpha = 0.05, bootstraping=FALSE, horizon=NULL, ...){

  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")
  if(!all(format %in% c("numeric", "html", "latex")))
    stop("allowed 'format' values are 'numeric', 'html' and 'latex'")
  if(!is.numeric(nboot) | nboot <= 0) stop("`nboot` must be a positive numeric value")
  if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`horizon` must be a Date object")
  if(any(horizon <= x$int.date)) stop("Dates in `horizon` must follow `int.date`")

  # settings
  cov<-x$xreg

  ### 1. arima

  # 1.1. arima order
  arima_order<-t(data.frame(arima_order=arimaorder(x$model)))

  # 1.2. coefficient estimates
  coef<-x$model$coef
  se<-sqrt(x$model$var.coef)
  param<-data.frame(coef, se)
  rownames(param)<- names(x$model$coef)
  colnames(param)<-c("coef", "se")

  # 1.3. accuracy measures
  accuracies<-accuracy(x$model)

  # 1.4. stats
  loglik<- x$model$loglik
  aic<- x$model$aic
  aicc<- x$model$aicc
  bic<- x$model$bic
  log_stats<-t(data.frame(metrics=c(loglik=loglik,aic=aic, bic=bic, aicc=aicc)))

  # Saving a list of results
  results_arima<-list( arima_order=arima_order, param=param, accuracy=accuracies, log_stats=log_stats)

  ### 2. impact

  # Estimated point, cumulative and temporal average effect assuming Normality of the errors
  impact_norm<-print(x, horizon=horizon)

  if(is.numeric(impact_norm)){
    names <- names(impact_norm)
    impact_norm <- matrix(as.matrix(impact_norm), nrow = 1, ncol = length(impact_norm))
    colnames(impact_norm) <- names
  }
  impact_norm_tau<-impact_norm[,grepl("tau", colnames(impact_norm) )]
  impact_norm_sum<-impact_norm[,grepl("sum", colnames(impact_norm) )]
  impact_norm_avg<-impact_norm[,grepl("avg", colnames(impact_norm) )]

  # saving a list of results
  impact_norm<-list(avg=impact_norm_avg, sum=impact_norm_sum, tau=impact_norm_tau)
  if(!is.null(horizon)){impact_norm<-lapply(impact_norm, function(z){ data.frame(horizon=horizon, z)})}

  ### 3. impact_boot

  # Estimated point, cumulative and temporal average effect by bootstrap
  impact_boot<-.impact_summary(x, xreg=cov, boot=nboot, alpha = alpha, bootstrap=bootstraping) # da modificare i parametri, giusto un test!
  impact_boot<-.format_impact(impact_boot)

  # saving a list of results
  impact_boot <- list(average=impact_boot$effect, effect_cum=impact_boot$effect_cum, p_values=impact_boot$p_values)


  ### Global savings
  results<-list(impact_norm = impact_norm, impact_boot = impact_boot, arima=results_arima)

  if(isTRUE(format=="html")){
    # results<-knitr::kable(results, format = "html")
    results<-lapply(results, function(z) lapply(z, knitr::kable, format = "html"))
  }
  if(isTRUE(format=="latex")){
    results<-lapply(results, function(z) lapply(z, knitr::kable, format = "latex"))
  }
  return(results)
}


# -----------------------------------------------------------------------------------------


.impact_summary<-function(x, xreg, boot=10, alpha = 0.05, bootstrap=FALSE){
  post_index<-x$dates>=x$int.date # select variable in the post period
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05


  if(is.null(dim(xreg))){  xreg<-xreg[post_index ]  }
  else{ xreg<-xreg[post_index, ] }

  simulated<-matrix(NA, sum(post_index), boot)
  for(i in 1:boot){
    sim<-simulate(x$model,  future=TRUE, nsim=sum(post_index), xreg=xreg, boostrap=bootstrap)
    simulated[,i]<-sim
  }


  effects <- apply(simulated, 2, function(z) {x$y[post_index]-z} )

  observed<- c(mean(x$y[post_index]), sum(x$y[post_index]))
  forecasted <- c(mean(x$forecast), sum(x$forecast))
  forecasted_low<-c(quantile(colMeans(simulated), prob.lower), quantile(colSums(simulated), prob.lower))
  forecasted_up<-c(quantile(colMeans(simulated), prob.upper), quantile(colSums(simulated), prob.upper))
  forecasted.sd <- c(sd(colMeans(simulated)), sd(colSums(simulated)))
  abs_effect <- c(mean(x$y[post_index]) - mean(x$forecast), sum(x$y[post_index]) - sum(x$forecast))

  abs_effect_lower <- c(quantile(colMeans(effects ),prob.lower), quantile(colSums(effects), prob.lower))

  abs_effect_upper <- c(quantile(colMeans(effects), prob.upper), quantile(colSums(effects), prob.upper))

  abs_effect_sd <- c(sd(colMeans( effects)), sd(colSums(effects)))



  results <-data.frame(observed=observed, forecasted=forecasted, forecasted_low=forecasted_low, forecasted_up=forecasted_up,
              forecasted.sd=forecasted.sd, abs_effect=abs_effect, abs_effect_lower=abs_effect_lower, abs_effect_upper=abs_effect_upper,
              abs_effect_sd=abs_effect_sd)



  results <- dplyr::mutate(results,
                           relative_effect = abs_effect /forecasted,
                           relative_effect_lower =abs_effect_lower /  forecasted,
                           relative_effect_upper =abs_effect_upper / forecasted,
                           relative_effect_sd = abs_effect_sd /  forecasted)

  rownames(results) <- c("Average", "Cumulative")

  # Add interval coverage, defined by alpha
  results$alpha <- alpha

  # Add one-sided tail-area probability of overall impact, p
  y.samples.post.sum <- colSums(simulated)
  y.post.sum <- sum(x$y[post_index])
  # return(list(y.samples.post.sum=y.samples.post.sum, y.post.sum=y.post.sum))
  # p <- min(sum(c(y.samples.post.sum, y.post.sum) >= y.post.sum),
  #          sum(c(y.samples.post.sum, y.post.sum) <= y.post.sum)) /
  #   (length(y.samples.post.sum) + 1)

  p <- min(mean(y.samples.post.sum >= y.post.sum), mean(y.samples.post.sum<=  y.post.sum))
  results$p <- p

  return(results)
}


# -----------------------------------------------------------------------------------------

.format_impact<-function(tab_imp){
  tab_imp<-t(tab_imp)

  effect<-tab_imp[1:(nrow(tab_imp)-2) ,"Average"]

  cum_effect<-tab_imp[1:(nrow(tab_imp)-2) ,"Cumulative"]
  p_values<-tab_imp[(nrow(tab_imp)-1):nrow(tab_imp) ,"Average"]

  effect_format<-data.frame(estimates=effect[c("observed", "forecasted", "abs_effect", "relative_effect")])
  effect_format$inf<-c(NA, effect[c("forecasted_low", "abs_effect_lower", "relative_effect_lower")])
  effect_format$sup<-c(NA, effect[c("forecasted_up", "abs_effect_upper", "relative_effect_upper")])
  effect_format$sd<-c(NA, effect[c("forecasted.sd", "abs_effect_sd", "relative_effect_sd")])
  rownames(effect_format)<-c("observed", "forecasted", "absolute_effect", "relative_effect")

  effect_cum_format<-data.frame(estimates=cum_effect[c("observed", "forecasted", "abs_effect", "relative_effect")])
  effect_cum_format$inf<-c(NA, cum_effect[c("forecasted_low", "abs_effect_lower", "relative_effect_lower")])
  effect_cum_format$sup<-c(NA, cum_effect[c("forecasted_up", "abs_effect_upper", "relative_effect_upper")])
  effect_cum_format$sd<-c(NA, cum_effect[c("forecasted.sd", "abs_effect_sd", "relative_effect_sd")])
  rownames(effect_cum_format)<-rownames(effect_format)

  res<-list(effect=effect_format, effect_cum=effect_cum_format, p_values=p_values)
  res
}

