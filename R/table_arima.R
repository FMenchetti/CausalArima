######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-09-28                                                ####
####                                                                              ####
####  Content:          Table method for object of class cArima                   ####
####                                                                              ####
####  Main function :   ResultTable, CoefficientsTable                                                    ####
####  Dependencies:     .star                                                     ####
####                                                                              ####
####                                                                              ####
######################################################################################
######################################################################################

# INCLUDERE FUNZIONE PER TABULARE TANTI MODELLI INSIEME?
# TABULARE ANCHE LE ALTRE STATISTICHE TUTTE INSIEME?
# LET THE USERS CHANGE PVALUES IN STAR?

#' Function to create ready-to-use tables of the estimated causal effects from a call to CausalArima
#'
#' @param x Object of class \code{cArima}.
#' @param type Character, indicating whether to produce a table reporting
#'             the estimated standard errors under the Normality assumption
#'             (\code{type = "norm"}) or bootstrapped errors (\code{type = "boot"}).
#' @param stat Character, indicating the test statistic to include in the table.
#'             Possible values in \code{c("tau", "sum", "avg")}.
#' @param direction Character, for the two-sided test set \code{direction = "b"} (default),
#'                  for one-sided tests set \code{direction = "l"} or \code{direction = "r"}.
#' @param horizon Optional vector with elements of class \code{Date}. If provided, the function
#'                outputs a table for the required \code{stat} at every given time horizon.
#'                If \code{NULL}, the table refers to the last date of the analysis period.
#' @param digits  Number of decimal places, defaults to 2.
#'
#' @return NULL
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
#' ResultTable(ce, type = "norm", horizon = horizon)

ResultTable <- function(x, type = "norm", stat = c("tau", "avg", "sum"), direction = "b", horizon = NULL, digits = 2, printing=FALSE){

  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")
  if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`horizon` must be a Date object")
  if(!(type %in% c("norm", "boot"))) stop("allowed 'type' values are 'norm' or 'boot'")
  if(length(x$boot) ==0 & type == "boot") stop("no bootstrap estimates to table, please check CausalArima 'nboot' parameter")
  if(!all(stat %in% c("tau", "sum", "avg"))) stop("allowed 'stat' values are 'tau', 'sum', 'avg'")
  if(!direction %in% c("l", "b", "r")) stop("allowed 'direction' values are 'l', 'b', 'r'")

  # Settings
  if(is.null(horizon)){ horizon <- tail(x$dates, 1)}
  sumry <- as.matrix(summary(x, type = type, horizon = horizon)[, paste("pvalue.", stat, ".", direction, sep = "")])
  star. <- apply(sumry, 2, FUN = ".star")

  # Table
  effects <- round(summary(x, type = type, horizon = horizon)[, paste(stat)], digits = digits)
  tab <- matrix(paste0(unlist(effects), c(unlist(star.))), nrow = length(stat), ncol = length(horizon), byrow = T)
  rownames(tab) <- stat
  colnames(tab) <- paste(horizon)


  if(type == "norm"){
    sd <- as.matrix(round(summary(x, type = type, horizon = horizon)[, paste("sd.", stat, sep = "")], digits = digits))
    sd <- t(apply(sd, 2, FUN = function(x)(paste0("(", x, ")"))))
    tab_norm <- matrix(NA, nrow = 2*length(stat), ncol = length(horizon))
    even <- as.logical(seq_len(nrow(tab_norm))%%2)
    tab_norm[even, ] <- tab
    tab_norm[!even, ] <- sd
    tab <- tab_norm
    colnames(tab) <- paste(horizon)
    names <- c(stat, paste0(stat, ".sd"))
    rownames(tab) <- names[order(names, decreasing = F)]
  }
  if(printing){
    cat(tab, sep="\n")
  }
  return(tab)
}

# -----------------------------------------------------------------------------------------

#' Function to create a table of the estimated model coefficients from a call to CausalArima
#'
#' @importFrom stargazer stargazer
#' @param x Object of class \code{cArima}.
#' @param ... Optional arguments to pass on \code{stargazer()}.
#'
#' @return NULL
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
#' CoefficientsTable(ce)
#'
CoefficientsTable <- function(x, printing=FALSE, format="text", n=10, alfa = 0.05, bootstraping=FALSE, cov, ...){

  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")

  arima_order<-t(data.frame(arima_order=arimaorder(x$model)))
  coef<-x$model$coef
  se<-sqrt(x$model$var.coef)

  param<-data.frame(coef, se)
  rownames(param)<- names(x$model$coef)
  colnames(param)<-c("coef", "se")

  loglik<- x$model$loglik
  aic<- x$model$aic
  aicc<- x$model$aicc
  bic<- x$model$bic


  log_stats<-t(data.frame(metrics=c(loglik=loglik,aic=aic, bic=bic, aicc=aicc)))
  accuracies<-accuracy(x$model)

  impact<-impact_summary(x, xreg=cov, boot=n, alpha = alfa, bootstrap=bootstraping) # da modificare i parametri, giusto un test!
  impact<-format_impact(impact)

  results_arima<-list( arima_order=arima_order, param=param, accuracy=accuracies, log_stats=log_stats)
  results_effect<-list(  average=impact$effect, effect_cum=impact$effect_cum, p_values=impact$p_values)
  # results<-list( arima_order=arima_order, param=param, accuracy=accuracies, log_stats=log_stats, effect=impact$effect, effect_cum=impact$effect_cum, p_values=impact$p_values)
  results<-list(impact=results_effect, arima=results_arima)

  if(isTRUE(printing)){
    cat("Arima Order:\n")
    print(arima_order)
    print(param)
    cat("\n")
    print(accuracies)
    cat("\n")
    print(log_stats)
    cat("\n")
    print(t(impact))
  }

  if(isTRUE(format=="html")){
    # results<-knitr::kable(results, format = "html")
    results<-lapply(results, knitr::kable, format = "html")
  }
  if(isTRUE(format=="latex")){
    results<-lapply(results, knitr::kable, format = "latex")
  }
  return(results)
}

# -----------------------------------------------------------------------------------------

.star <- function(pvalue){
  star <- rep(NA, times = length(pvalue))
  ind  <- abs(pvalue) < 0.001
  ind2 <- abs(pvalue) < 0.01 & abs(pvalue) >= 0.001
  ind3 <- abs(pvalue) < 0.05 & abs(pvalue) >= 0.01
  ind4 <- abs(pvalue) < 0.1 &  abs(pvalue) >= 0.05
  star[ind] <- "***" ; star[ind2] <- "**" ; star[ind3]<- "*"
  star[ind4] <- "." ; star[!ind & !ind2 & !ind3 & !ind4] <- " "
  star
}


# -----------------------------------------------------------------------------------------

#' Title
#'
#' @param x
#' @param xreg
#' @param boot
#' @param alpha
#' @param bootstrap
#'
#' @return
#' @export
#'
#' @examples
impact_summary<-function(x, xreg, boot=10, alpha = 0.05, bootstrap=FALSE){
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
  y.samples.post.sum <- rowSums(simulated)
  y.post.sum <- sum(x$y[post_index])
  p <- min(sum(c(y.samples.post.sum, y.post.sum) >= y.post.sum),
           sum(c(y.samples.post.sum, y.post.sum) <= y.post.sum)) /
    (length(y.samples.post.sum) + 1)
  results$p <- p

  return(results)
}


# -----------------------------------------------------------------------------------------

format_impact<-function(tab_imp){
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

# -----------------------------------------------------------------------------------------
#' Title
#'
#' @param y
#' @param x
#' @param n
#' @param beta
#' @param digits
#' @param boot
#'
#' @return
#' @export
#'
#' @examples
pretty_summary<-function(y, x= x1, n=10, beta=0.05, digits=2L, boot=FALSE, printing=FALSE){
  impact<-impact_summary(x=y, xreg=x, boot=n, alpha = beta, bootstrap=boot)

  # Define formatting helper functions
  FormatNumber <- function(x) format(x, digits = digits, trim = TRUE)
  FormatPercent <- function(x) {
    paste0(format(x * 100, digits = digits, trim = TRUE), "%")
  }
  FormatCI <- function(a, b) {
    paste0("[", format(a, digits = min(digits, 2), trim = TRUE),
           ", ", format(b, digits = min(digits, 2), trim = TRUE),
           "]")
  }
  FormatPercentCI <- function(a, b) {
    paste0("[", format(a * 100, digits = min(digits, 2), trim = TRUE),
           "%, ", format(b * 100, digits = min(digits, 2), trim = TRUE),
           "%]")
  }

  # Compile data frame with formatted numbers
  fsummary <- data.frame(
    Actual = FormatNumber(impact$observed ),
    Pred = paste0(FormatNumber(impact$forecasted),
                  " (", FormatNumber(impact$forecasted.sd), ")"),
    Pred.ci = FormatCI(impact$forecasted_low , impact$forecasted_up ),
    Separator1 = c("", ""),
    AbsEffect = paste0(FormatNumber(impact$abs_effect ),
                       " (", FormatNumber(impact$abs_effect_sd), ")"),
    AbsEffect.ci = FormatCI(impact$abs_effect_lower , impact$abs_effect_upper),
    Separator2 = c("", ""),
    RelEffect = paste0(FormatPercent(impact$relative_effect ),
                       " (", FormatPercent(impact$relative_effect_sd ), ")"),
    RelEffect.ci = FormatPercentCI(impact$relative_effect_lower ,
                                   impact$relative_effect_upper ))

  # Invert and format as table
  tsummary <- t(fsummary)
  colnames(tsummary) <- c("Average", "Cumulative")
  ci.label <- paste0(round((1 - beta) * 100), "% CI")
  row.names(tsummary) <- c("Actual", "Prediction (s.d.)", ci.label,
                           " ",
                           "Absolute effect (s.d.)", paste(ci.label, ""),
                           "  ",
                           "Relative effect (s.d.)", paste(ci.label, " "))

  # Print formatted table
  if(isTRUE(printing)){
  cat("\n")
  print.default(tsummary, print.gap = 3L, quote = FALSE)
  cat("\n")

  # Print overall tail-area probability
  p <- impact$p[1]
  cat(paste0("Posterior tail-area probability p:   ", round(p, 5), "\n"))
  cat(paste0("Posterior prob. of a causal effect:  ",
             round((1 - p) * 100, ifelse(p < 0.01, 5, ifelse(p < 0.05, 3, 0))),
             "%\n"))
  cat("\n")
  cat(paste0("For more details, type: summary(impact, \"report\")\n"))
  cat("\n")
  }
  return(t(tsummary))

}
