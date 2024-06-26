% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_arima.R
\name{summary.cArima}
\alias{summary.cArima}
\title{Summary method for object of class 'cArima'}
\usage{
\method{summary}{cArima}(x, type = "norm", horizon = NULL, digits = 3)
}
\arguments{
\item{x}{Object of class \code{cArima}.}

\item{type}{Character string indicating the summary to be produced. Possible values
in \code{c("norm", "boot")}. Defaults to \code{"norm"}.}

\item{horizon}{Optional vector with elements of class \code{Date}. If provided, the function
summarizes the point, cumulative and temporal average effects at the given
time horizon(s).}

\item{digits}{Integer, indicating the number of decimal places to show in the output.}
}
\description{
Format and prints the point, cumulative and temporal average effects (standard errors
and critical values) in a nice and clean output.
}
\examples{
## Example 1 (daily data, no predictors)
# Generating a time series of length 1000 and a vector of dates
y <- 0.5*seq(0.5, 250, by = 0.5) + rnorm(500, sd = 6)
dates <- seq.Date(from = as.Date("2014-01-05"), by = "days", length.out = 500)

# Adding a fictional intervention
int.date <- as.Date("2015-04-01")
horizon <- as.Date(c("2015-04-10", "2015-04-20"))
y.new <- y ; y.new[dates >= int.date] <- y.new[dates >= int.date]*1.25

# Causal effect estimation
ce <- CausalArima(y = y.new, auto = TRUE, ic = "aic",
                  dates = dates, int.date = int.date, nboot = 1000)

# Summary
summary(ce, type = "norm")
summary(ce, type = "boot", horizon = horizon)

## Example 2 (daily data, with predictors)
# Loading data and setting dates
data(sales)
y <- sales[, "Sales"]
dates <- as.Date(sales[, "Dates"])
int.date <- as.Date("2018-10-04")
horizon<- as.Date(c("2018-11-04","2019-01-04","2019-04-30"))
xreg <- sales[, "Price"]

# Causal effect estimation
ce <- CausalArima(y = ts(y, frequency = 7), xreg = xreg, int.date = int.date,
                  dates = dates, nboot = 100)

# Summary of the estimated effects at different time points (defined in horizon)
summary(ce, horizon = horizon)
summary(ce, type = "boot", horizon = horizon)

}
