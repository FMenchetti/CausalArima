######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-08-09                                                ####
####                                                                              ####
####  Content:          Summary & print methods for objects of class 'cArima'     ####
####                                                                              ####
####  Main functions :   summary.cArima                                           ####
####                     print.cArima                                             ####
######################################################################################
######################################################################################

#' Printing method for object of class 'cArima'
#'
#' Quick shortcut to see the results of a call to \code{CausalArima}, i.e., the estimated
#' causal effects at each time point provided in \code{horizon}. If \code{horizon = NULL}
#' (default), the function prints the estimated effects at the last date of the
#' post-intervention period.
#'
#' @param x Object of class \code{cArima}.
#' @param type Character string indicating the summary to be produced. Possible values
#'             in \code{c("norm", "boot")}. Defaults to \code{"norm"}.
#' @param horizon Optional vector with elements of class \code{Date}. If provided, the function
#'                summarizes the point, cumulative and temporal average effects at the given
#'                time horizon(s).
#'
#' @return An extract from the data.frame returned by \code{cArima$norm$inf} or \code{cArima$boot$inf}
#'         corresponding to the dates provided in horizon (if \code{horizon = NULL},
#'         a single row corresponding to the last date of the post-intervention period) with the
#'         following columns:
#'         \item{tau}{The estimated causal effect at the given time horizon or at the end of analysis
#'                    period if \code{is.null(horizon)}.}
#'         \item{pvalue.tau}{Left-sided (\code{.l}), bidirectional (\code{.b}) and right-sided (\code{.r})
#'                           p-values for \code{tau}.}
#'         \item{sum.tau}{The estimated cumulative causal effect up to the given time horizon or at the end of analysis
#'                        period if \code{is.null(horizon)}.}
#'         \item{pvalue.sum}{Left-sided (\code{.l}), bidirectional (\code{.b}) and right-sided (\code{.r})
#'                           p-values for \code{sum.tau}.}
#'         \item{avg.tau}{The estimated temporal average causal effect up to the given time horizon or at the end of analysis
#'                        period if \code{is.null(horizon)}.}
#'         \item{pvalue.avg}{Left-sided (\code{.l}), bidirectional (\code{.b}) and right-sided (\code{.r})
#'                           p-values for \code{avg.tau}.}
#'         Additional columns are provided with the estimated standard deviations for the point, cumulative
#'         and temporal average effects.
#' @export
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
#' # Causal effect estimation
#' ce <- CausalArima(y = y.new, auto = TRUE, ic = "aic",
#'                   dates = dates, int.date = int.date, nboot = 1000)
#'
#' # Print
#' print(ce, type = "norm")
#' print(ce, type = "boot", horizon = horizon)
#'
#' ## Example 2 (daily data, with predictors)
#' # Loading data and setting dates
#' data(sales)
#' y <- sales[, "Sales"]
#' dates <- as.Date(sales[, "Dates"])
#' int.date <- as.Date("2018-10-04")
#' horizon<- as.Date(c("2018-11-04","2019-01-04","2019-04-30"))
#' xreg <- sales[, "Price"]
#'
#' # Causal effect estimation
#' ce <- CausalArima(y = ts(y, frequency = 7), xreg = xreg, int.date = int.date,
#'                   dates = dates, nboot = 100)
#'
#' # Print
#' print(ce, horizon = horizon)
#' print(ce, type = "boot", horizon = horizon)
#'
print.cArima<- function(x, type = "norm", horizon = NULL){
  if(!is.null(horizon)){

    # param checks
    if(class(x) != "cArima") stop ("`x` must be an object of class cArima")
    if(!all(type %in% c("norm", "boot")))
      stop("allowed 'type' values are 'norm' and 'boot'")
    if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
      stop("`horizon` must be a Date object")

    ## Settings & removing NA's
    int.date <- x$int.date
    dates <- x$dates
    ind <- which(dates[dates >= int.date][!is.na(x$causal.effect)] %in% horizon)
    ncol <- 1 + dim(x[[type]]$inf)[2]
    results <- data.frame(matrix(nrow = length(horizon), ncol = ncol))
    colnames(results) <- c("Time horizon", colnames(x[[type]]$inf))
    results[, 1] <- horizon
    results[, 2:ncol] <- x[[type]]$inf[ind, ]

  } else {
    h <- dim(x[[type]]$inf)[1]
    results <- x[[type]]$inf[h, ]
  }

  results
}

# ------------------------------------------------------------------------------

#' Summary method for object of class 'cArima'
#'
#' Format and prints the point, cumulative and temporal average effects (standard errors
#' and critical values) in a nice and clean output.
#'
#' @param x Object of class \code{cArima}.
#' @param type Character string indicating the summary to be produced. Possible values
#'             in \code{c("norm", "boot")}. Defaults to \code{"norm"}.
#' @param horizon Optional vector with elements of class \code{Date}. If provided, the function
#'                summarizes the point, cumulative and temporal average effects at the given
#'                time horizon(s).
#' @param digits Integer, indicating the number of decimal places to show in the output.
#'
#' @return
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
#' # Causal effect estimation
#' ce <- CausalArima(y = y.new, auto = TRUE, ic = "aic",
#'                   dates = dates, int.date = int.date, nboot = 1000)
#'
#' # Summary
#' summary(ce, type = "norm")
#' summary(ce, type = "boot", horizon = horizon)
#'
#' ## Example 2 (daily data, with predictors)
#' # Loading data and setting dates
#' data(sales)
#' y <- sales[, "Sales"]
#' dates <- as.Date(sales[, "Dates"])
#' int.date <- as.Date("2018-10-04")
#' horizon<- as.Date(c("2018-11-04","2019-01-04","2019-04-30"))
#' xreg <- sales[, "Price"]
#'
#' # Causal effect estimation
#' ce <- CausalArima(y = ts(y, frequency = 7), xreg = xreg, int.date = int.date,
#'                   dates = dates, nboot = 100)
#'
#' # Summary of the estimated effects at different time points (defined in horizon)
#' summary(ce, horizon = horizon)
#' summary(ce, type = "boot", horizon = horizon)
#'
summary.cArima<- function(x, type = "norm", horizon = NULL, digits = 3){

  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")

  # summary
  sumryy <- print(x, type = type, horizon = horizon)
  ind1 <- which(names(sumryy) == "tau"):which(names(sumryy) == "pvalue.tau.r")
  ind2 <- which(names(sumryy) == "sum"):which(names(sumryy) == "pvalue.sum.r")
  ind3 <- which(names(sumryy) == "avg"):which(names(sumryy) == "pvalue.avg.r")

  # printing
  if(is.numeric(sumryy)){
    sumry <- as.matrix(round(sumryy, digits = digits))
    obj <- rbind(as.matrix(sumry[ind1,]), "", as.matrix(sumry[ind2,]), "", as.matrix(sumry[ind3,]))
    colnames(obj) <- ""

  } else {
    sumry <- round(t(sumryy[,-1]), digits = digits)
    obj <- rbind(sumry[ind1-1,], "", sumry[ind2-1,], "", sumry[ind3-1,])
    colnames(obj) <- paste(sumryy[,1])
  }

  # setting rownames
  nnames <- c("Point causal effect", "Standard error", "Left-sided p-value", "Bidirectional p-value", "Right-sided p-value", "",
              "Cumulative causal effect", "Standard error", "Left-sided p-value", "Bidirectional p-value", "Right-sided p-value", "",
              "Temporal average causal effect", "Standard error", "Left-sided p-value", "Bidirectional p-value", "Right-sided p-value")

  rownames(obj) <- nnames
  noquote(obj)
}

