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
#' @param horizon Optional vector with elements of class Date. If provided, the function returns
#'                the estimated effects at the given time horizons.
#' @param style Function to pass to \code{knitr_kable} objects to customize the style of the table.
#'              Defaults to \code{kable_styling}. Alternative themes for html tables include \code{kable_classic},
#'              or \code{kable_minimal}. For the full list of alternatives, see package \code{kableExtra}
#'              documentation.
#' @param digits Number of digits in table columns.
#' @param ... Optional arguments passed to other methods.
#'
#'
#' @return A list with the following components:
#' \item{impact_norm}{Estimated point, cumulative and temporal average effect assuming Normality
#'              of the error terms.}
#' \item{impact_boot}{Estimated point, cumulative and temporal average effect by bootstrap.}
#' \item{arima}{Arima order, coefficient estimates and accuracy measures for the estimated model
#'              in the pre-intervention period.}
#' @export
#'
#' @examples
#' ## Example 1 (weekly data, no predictors)
#' # Generating a time series with weekly seasonality and a vector of dates
#' y <- simulate(Arima(ts(rnorm(100),freq=4), order=c(1,0,1), seasonal=c(1,0,1)),
#'               nsim=800)
#' dates <- seq.Date(from = as.Date("2005-01-01"), by = "week", length.out = 800)
#'
#' # Adding a fictional intervention
#' int.date <- as.Date("2019-05-11")
#' horizon <- as.Date(c("2019-12-07", "2020-02-15", "2020-04-25"))
#' y.new <- y ; y.new[dates >= int.date] <- y.new[dates >= int.date]*1.40
#'
#' # Causal effect estimation
#' ce <- CausalArima(y = ts(y.new, frequency = 4), dates = dates, int.date = int.date)
#'
#' # Table of the estimated effects (numeric and latex)
#' impact(ce)
#' impact(ce, horizon = horizon)
#' tab_latex <- impact(ce, format = "latex", horizon = horizon, digits = 3, latex_options = "striped")
#' tab_latex$impact_norm$average
#'
#' ## Example 2 (daily data, with predictors)
#' # Loading data and setting dates
#' data(sales)
#' y <- sales[, "Sales"]
#' dates <- as.Date(sales[, "Dates"])
#' int.date <- as.Date("2018-10-04")
#' horizon<- as.Date(c("2018-11-04","2019-01-04","2019-04-30"))
#' xreg <- sales[, 4:12]
#'
#' # Causal effect estimation
#' ce <- CausalArima(y = ts(y, frequency = 7), xreg = xreg, int.date = int.date,
#'                   dates = dates, nboot = 100)
#'
#' # Table of the estimated effects (html)
#' tab_html <- impact(ce, format = "html", horizon = horizon)
#' tab_html$arima$param
#' tab_html <- impact(ce, format = "html", horizon = horizon, style = kable_classic,
#'                    html_font = "Cambria")
#' tab_html$arima$param
#'
impact <- function(x,  format="numeric", horizon=NULL,  style = kable_styling, digits = 3, ...){

  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")
  if(!all(format %in% c("numeric", "html", "latex")))
    stop("allowed 'format' values are 'numeric', 'html' and 'latex'")
  if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`horizon` must be a Date object")
  if(any(horizon <= x$int.date)) stop("Dates in `horizon` must follow `int.date`")

  # settings
  cov<-x$xreg
  alpha<-x$alpha

  ### 1. arima

  # 1.1. arima order
  arima_order<-t(data.frame(arima_order=arimaorder(x$model)))

  # 1.2. coefficient estimates
  coef<-x$model$coef
  se<-sqrt(diag(x$model$var.coef))
  tvalue <- coef/se
  if(length(coef)!=0){
    param<-data.frame(coef, se, tvalue)
    rownames(param)<- names(x$model$coef)
    colnames(param)<-c("coef", "se", "t value")
  }
  else{ param<-NULL}

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

  ### 2. impact_norm

  # Estimated point, cumulative and temporal average effect assuming Normality
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
  names(impact_norm)<-c("average", "sum", "point_effect")

  if(!is.null(horizon)) {
    impact_norm<-lapply(impact_norm, function(x){ colnames(x)<-c("estimate", "sd", "p_value_left", "p_value_bidirectional", "p_value_right") ; x})
    impact_norm<-lapply(impact_norm, function(z){ data.frame(horizon=horizon, z)})
  }
  else{
    impact_norm<-lapply(impact_norm, function(x){ names(x)<-c("estimate", "sd", "p_value_left", "p_value_bidirectional", "p_value_right") ; x})
    impact_norm<-lapply(impact_norm, function(x) data.frame(t(x)))
  }

  ### 3. impact_boot

  # Estimated point, cumulative and temporal average effect by bootstrap
  if(is.null(x$boot)){
    impact_boot<-NULL
  }
  else{
    if(!is.null(horizon)) {
      impact_boot<-.impact_summary_horizons(x, horizon=horizon)
      impact_boot<- lapply(impact_boot, .format_impact)
      impact_boot<-lapply(impact_boot, function(z) {
        # saving a list of results
        list(average=z$effect, effect_cum=z$effect_cum, p_values=z$p_values)
      })
    }
    else{
      impact_boot<-.impact_summary(x)
      impact_boot<-.format_impact(impact_boot)
      # saving a list of results
      impact_boot <- list(average=impact_boot$effect, effect_cum=impact_boot$effect_cum, p_values=impact_boot$p_values)
    }

  }

  ### Global savings
  results<-list(impact_norm = impact_norm, impact_boot = impact_boot, arima=results_arima)

  if(format != "numeric"){
    if(format == "html"){

      results<-lapply(results, function(z) lapply(z, kbl, format = "html", digits = digits))

    }
    if(format=="latex"){

      results<-lapply(results, function(z) lapply(z, kbl, format ="latex", digits = digits))
    }
    results<-lapply(results, function(z) lapply(z, FUN = style, ...))
  }

  return(results)
}


# -----------------------------------------------------------------------------------------


.impact_summary<-function(x){

  # Setting
  xreg<-x$xreg
  alpha<-x$alpha  # alfa coef interval
  post_index<-x$dates>=x$int.date # select variable in the post period
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05


  if(is.vector(xreg)){  xreg<-xreg[post_index ]  }
  else{ xreg<-xreg[post_index, ] }

  simulated<-ce$boot$boot.distrib

  # Select post intervention period and remove missing values
   y_post<-x$y[post_index]
   nas<-is.na(y_post)
   y_post<-y_post[!nas]
   simulated<-simulated[!nas, ]
   x$forecast<-x$forecast[!nas]

  # Estimation of statistics
  effects <- apply(simulated, 2, function(z) {y_post-z} )

  observed<- c(mean(y_post), sum(y_post))
  forecasted <- c(mean(x$forecast), sum(x$forecast))
  forecasted_low<-c(quantile(colMeans(simulated), prob.lower), quantile(colSums(simulated), prob.lower))
  forecasted_up<-c(quantile(colMeans(simulated), prob.upper), quantile(colSums(simulated), prob.upper))
  forecasted.sd <- c(sd(colMeans(simulated)), sd(colSums(simulated)))
  abs_effect <- c(mean(y_post) - mean(x$forecast),
                  sum(y_post) - sum(x$forecast))

  abs_effect_lower <- c(quantile(colMeans(effects ),prob.lower), quantile(colSums(effects), prob.lower))

  abs_effect_upper <- c(quantile(colMeans(effects), prob.upper), quantile(colSums(effects), prob.upper))

  abs_effect_sd <- c(sd(colMeans( effects)), sd(colSums(effects)))

  # Saving results
  results <-data.frame(observed=observed, forecasted=forecasted, forecasted_low=forecasted_low, forecasted_up=forecasted_up,
              forecasted.sd=forecasted.sd, abs_effect=abs_effect, abs_effect_lower=abs_effect_lower, abs_effect_upper=abs_effect_upper,
              abs_effect_sd=abs_effect_sd)

  results$relative_effect<- results$abs_effect /results$forecasted
  results$relative_effect_lower <-results$abs_effect_lower /  results$forecasted
  results$relative_effect_upper <-results$abs_effect_upper / results$forecasted
  results$relative_effect_sd <- results$abs_effect_sd /  results$forecasted

  rownames(results) <- c("Average", "Cumulative")

  # Add interval coverage, defined by alpha
  results$alpha <- alpha

  # Add one-sided tail-area probability of overall impact, p
  y.samples.post.sum <- colSums(simulated)
  y.post.sum <- sum(y_post)

  # compute p values for any effect on the sum
  p <- min(mean(y.samples.post.sum >= y.post.sum), mean(y.samples.post.sum<=  y.post.sum))
  results$p <- p

  return(results)
}

# -----------------------------------------------------------------------------------------
.impact_summary_horizons<-function(x, horizon){

  # Setting
  xreg<-x$xreg
  alpha<-x$alpha  # alfa coef interval
  post_index<-x$dates>=x$int.date # select variable in the post period
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05


  if(is.vector(xreg)){  xreg<-xreg[post_index ]  }
  else{ xreg<-xreg[post_index, ] }

  simulated<-ce$boot$boot.distrib

  # Select post intervention period and remove missing values
  y_post<-x$y[post_index]
  nas<-is.na(y_post)
  y_post<-y_post[!nas]
  simulated<-simulated[!nas, ]
  x$forecast<-x$forecast[!nas]

  # Estimation of statistics
  effects <- apply(simulated, 2, function(z) {y_post-z} )

  index<-sapply(horizon, function(z) x$dates <=z)
  index<-tail(index, length(y_post))
  results<-NULL
  for(i in 1:ncol(index)){
  y_post_h<-y_post[index[,i]]
  forecast_h<-x$forecast[index[,i]]
  simulated_h<-simulated[index[,i], ]
  effects_h<-effects[index[,i], ]

  observed<- c(mean(y_post_h), sum(y_post_h))
  forecasted <- c(mean(forecast_h), sum(forecast_h))
  forecasted_low<-c(quantile(colMeans(simulated_h), prob.lower), quantile(colSums(simulated_h), prob.lower))
  forecasted_up<-c(quantile(colMeans(simulated_h), prob.upper), quantile(colSums(simulated_h), prob.upper))
  forecasted.sd <- c(sd(colMeans(simulated_h)), sd(colSums(simulated_h)))
  abs_effect <- c(mean(y_post_h) - mean(forecast_h),
                  sum(y_post_h) - sum(forecast_h))

  abs_effect_lower <- c(quantile(colMeans(effects_h ),prob.lower), quantile(colSums(effects_h), prob.lower))

  abs_effect_upper <- c(quantile(colMeans(effects_h), prob.upper), quantile(colSums(effects_h), prob.upper))

  abs_effect_sd <- c(sd(colMeans( effects_h)), sd(colSums(effects_h)))

  # Saving results
  results_h <-data.frame(observed=observed, forecasted=forecasted, forecasted_low=forecasted_low, forecasted_up=forecasted_up,
                       forecasted.sd=forecasted.sd, abs_effect=abs_effect, abs_effect_lower=abs_effect_lower, abs_effect_upper=abs_effect_upper,
                       abs_effect_sd=abs_effect_sd)

  results_h$relative_effect<- results_h$abs_effect /results_h$forecasted
  results_h$relative_effect_lower <-results_h$abs_effect_lower /  results_h$forecasted
  results_h$relative_effect_upper <-results_h$abs_effect_upper / results_h$forecasted
  results_h$relative_effect_sd <- results_h$abs_effect_sd /  results_h$forecasted

  rownames(results_h) <- c("Average", "Cumulative")

  # Add interval coverage, defined by alpha
  results_h$alpha <- alpha

  # Add one-sided tail-area probability of overall impact, p
  y.samples.post.sum <- colSums(simulated_h)
  y.post.sum <- sum(y_post_h)

  # compute p values for any effect on the sum
  p <- min(mean(y.samples.post.sum >= y.post.sum), mean(y.samples.post.sum<=  y.post.sum))
  results_h$p <- p
  results[[i]]<-results_h
  }
  names(results)<-horizon
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

