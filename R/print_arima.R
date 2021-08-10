######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-08-09                                                ####
####                                                                              ####
####  Content:          Summary methods for objects of class 'cArima'             ####
####                                                                              ####
####  Main function :   summary.cArima                                            ####
####                                                                              ####
######################################################################################
######################################################################################

#' Summary method for object of class 'cArima'
#'
#' @param x Object of class \code{cArima}.
#' @param type Character string indicating the summary to be produced. Possible values
#'             in c("norm", "boot").
#' @param horizon Optional vector with elements of class \code{Date}. If provided, the function
#'                summarizes the point, cumulative and temporal average effects at the given
#'                time horizon(s).
#'
#' @return A data.frame with as many rows as the dates provided in horizon (if \code{is.null(horizon)},
#'         a single row corresponding to the last date of the post-intervetion period) with the
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
#'         When \code{type = "norm"} additional columns are provided with the estimated standard deviations
#'         for the point, cumulative and temporal average effects under the assumption of Normally distributed residuals.
#' @export
#'
#' @examples
#' ## Example 1
#' # Generating a time series of length 1000 and a vector of dates
#' y <- 0.5*seq(0.5, 250, by = 0.5) + rnorm(500, sd = 6)
#' dates <- seq.Date(from = as.Date("2014-01-05"), by = "days", length.out = 500)
#'
#' # Adding a fictional intervention
#' int.date <- as.Date("2015-04-01")
#' horizon <- c(as.Date("2015-04-10"), as.Date("2015-04-20"))
#' y.new <- y ; y.new[dates >= int.date] <- y.new[dates >= int.date]*1.25
#'
#' # Causal effect estimation
#' start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
#' ce <- CausalArima(y = ts(y.new, start = start, frequency = 1), auto = TRUE, ic = "aic",
#'                   dates = dates, int.date = int.date, nboot = 1000)
#'
#' # Summary
#' summary(ce, type = "norm")
#' summary(ce, type = "boot", horizon = horizon)
#'
summary.cArima<- function(x, type, horizon = NULL){

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
    summary <- data.frame(matrix(nrow = length(horizon), ncol = ncol))
    colnames(summary) <- c("Time horizon", colnames(x[[type]]$inf))
    summary[, 1] <- horizon
    summary[, 2:ncol] <- x[[type]]$inf[ind, ]
    summary

  } else {
    h <- dim(x[[type]]$inf)[1]
    x[[type]]$inf[h, ]
  }
}



