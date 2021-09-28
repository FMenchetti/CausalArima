######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-09-28                                                ####
####                                                                              ####
####  Content:          Table method for object of class cArima                   ####
####                                                                              ####
####  Main function :   table                                                     ####
####  Dependencies:     .star                                                     ####
####                                                                              ####
####                                                                              ####
######################################################################################
######################################################################################

# INCLUDERE FUNZIONE PER TABULARE TANTI MODELLI INSIEME?
# TABULARE ANCHE LE ALTRE STATISTICHE TUTTE INSIEME?
# LET THE USERS CHANGE PVALUES IN STAR?

#' Function to create ready-to-use tables from a call to CausalArima
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
#' ce <- CausalArima(y = ts(y.new, start = start, frequency = 1), auto = TRUE, ic = "aic", dates = dates, int.date = int.date)
#'
#' # Table of the estimated temporal average effects
#' table(ce, type = "norm", stat = "avg", horizon = horizon)

table <- function(x, type = "norm", stat = "avg", direction = "b", horizon = NULL, digits = 2){

  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")
  if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`horizon` must be a Date object")
  if(!(type %in% c("norm", "boot"))) stop("allowed 'type' values are 'norm' or 'boot'")
  if(length(x$boot) ==0 & type == "boot") stop("no bootstrap estimates to table, please check CausalArima 'nboot' parameter")
  if(!stat %in% c("tau", "sum", "avg")) stop("allowed 'stat' values are 'tau', 'sum', 'avg'")
  if(!direction %in% c("l", "b", "r")) stop("allowed 'direction' values are 'l', 'b', 'r'")

  # Settings
  if(is.null(horizon)){ horizon <- tail(x$dates, 1)}

  tab <- matrix(NA, nrow = 1, ncol = length(horizon))
  colnames(tab) <- paste(horizon)
  rownames(tab) <- paste(stat)
  star. <- .star(summary(x, type = type, horizon = horizon)[, paste("pvalue.", stat, ".", direction, sep = "")])

  # Table
  tab[1,] <- paste(round(summary(x, type = type, horizon = horizon)[, paste(stat)], digits = digits), "^{", star., "}", sep="" )
  tab[1,] <- gsub(tab[1,], pattern = "\\^\\{ \\}", replacement = "")

  if(type == "norm"){
    tab <- rbind(tab, sd = paste("(",round(summary(x, type = type, horizon = horizon)[, paste("sd.", stat, sep = "")], digits = digits),")", sep = ""))
  }

  noquote(tab)
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
