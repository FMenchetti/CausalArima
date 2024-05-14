######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-08-10                                                ####
####                                                                              ####
####  Content:          Plot method for object of class cArima                    ####
####                                                                              ####
####  Main function :   plot.cArima                                               ####
####  Dependencies:     .impact                                                   ####
####                    .forecast                                                 ####
####                    .residuals                                                ####
######################################################################################
######################################################################################

#' Plotting method for object of class 'cArima'
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom tidybayes geom_lineribbon
#' @param x Object of class \code{cArima}.
#' @param type Character string indicating the plot to be produced. Possible values
#'             in c("forecast", "impact", "residuals").
#' @param horizon Optional vector with elements of class \code{Date}. If provided, the function
#'                plots the pointwise causal effect (for \code{type = "impact"}) or the forecasted
#'                time series (for \code{type = "forecast"}) up to the required time horizon(s).
#' @param ... Optional arguments passed to other methods.
#'
#' @return NULL
#' @export
#'
#' @details
#' The option 'forecast' plots the observed time series as compared to the forecasted
#' series. The option 'impact' plots the causal effect at each point in time after the
#' intervention date, as resulting from the direct comparison between the observed and
#' forecasted time series. Finally, the option 'residuals' draws the autocorrelation and
#' partial autocorrelation functions of the residuals of the model fitted to the data
#' in the pre-intervention period as well as the Normal QQ Plot.
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
#' ce <- CausalArima(y = y.new, auto = TRUE, ic = "aic", dates = dates, int.date = int.date)
#'
#' # Plot
#' plot(ce, type = "forecast")
#' plot(ce, type = "impact", horizon = horizon)
#' plot(ce, type = "residuals")
#'
#'## Example 2 (daily data, with predictors, log-linear model)
#'# Loading data and setting dates
#' data(sales)
#' y <- log(sales[, "Sales"])
#' dates <- as.Date(sales[, "Dates"])
#' int.date <- as.Date("2018-10-04")
#' horizon<- as.Date(c("2018-11-04","2019-01-04"))
#' xreg <- sales[, 4:12]
#'
#' # Causal effect estimation
#' ce <- CausalArima(y = ts(y, frequency = 7), xreg = xreg, int.date = int.date,
#'                   dates = dates, nboot = 100)
#'
#' # Plot
#' plot(ce, type = "forecast", horizon = horizon)
#' plot(ce, type = "impact", horizon = horizon)
#' plot(ce, type = "residuals")
#'
plot.cArima <- function(x, type = c("forecast", "impact", "residuals"), horizon = NULL, ...){

  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")
  if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`horizon` must be a Date object")
  if(!all(type %in% c("forecast", "impact", "residuals")))
    stop("allowed 'type' values are 'forecast', 'impact' and 'residuals'")
  if(any(horizon <= x$int.date)) stop("Dates in `horizon` must follow `int.date`")

  # Plot "Observed vs Forecast"
  if("forecast" %in% type){
    res <- .forecast(x, horizon = horizon, ...)
    return(res)
  }

  # Plot "Causal effect"
  if("impact" %in% type){
    res <- .impact(x, horizon = horizon, ...)
    return(res)
  }

  # Residuals plots
  if("residuals" %in% type){
    .residuals(x, ...)
  }


}

# -----------------------------------------------------------------------------------------

.impact <- function(cArima, horizon = NULL, alpha = 0.05, color_line="darkblue", color_intervals="slategray2", lines_size=0.6){
  # Settings
  dates <- cArima$dates[!is.na(cArima$causal.effect)]
  int.date <- cArima$int.date
  x <- dates[dates >= int.date]
  y <- na.omit(cArima$causal.effect)
  y.upper <- y + cArima$norm$inf[, "sd.tau"]*qnorm(1-alpha/2)
  y.lower <- y - cArima$norm$inf[, "sd.tau"]*qnorm(1-alpha/2)

  # Plot effect
  dat <- data.frame(x = x, y = y, y.upper = y.upper, y.lower = y.lower)
  ylim <- c(min(dat[, "y.lower"]), max(dat[, "y.upper"]))

  g <- ggplot(data = dat, aes(x = x)) +  coord_cartesian(ylim = ylim) + labs(title = "Point effect", y = "", x = "") +
    theme_bw(base_size = 15)+
    geom_ribbon(aes(x = x, ymax = y.upper, ymin = y.lower), fill =color_intervals)+
    geom_hline(yintercept=0, colour = "darkgrey", size = lines_size, linetype = "solid")+
    geom_line(aes(y = y), color = color_line, linetype = "dashed", size = lines_size)


  # Cumulative plot
  dat_cum<-dat
  dat_cum$y<-cumsum(dat$y)
  dat_cum$y.upper<-cumsum(dat$y.upper)
  dat_cum$y.lower<-cumsum(dat$y.lower)

  g_cum <- ggplot(data = dat_cum, aes(x = x))  + labs(title = "Cumulative effect", y = "", x = "") +
    theme_bw(base_size = 15)+
    geom_ribbon(aes(x = x, ymax = y.upper, ymin = y.lower), fill = color_intervals)+
    geom_hline(yintercept=0, colour = "darkgrey", size = lines_size, linetype = "solid")+
    geom_line(aes(y = y), color =color_line, linetype = "dashed", size = lines_size)

  if(!is.null(horizon)){
    g <-g + geom_vline(xintercept = horizon, linetype="dashed", size = lines_size)
    g_cum<-g_cum+ geom_vline(xintercept = horizon, linetype="dashed", size = lines_size)
  }

  results<-list(plot=g, cumulative_plot=g_cum)
  return(results)

}

# -----------------------------------------------------------------------------------------

.forecast <- function(cArima, horizon = NULL, win = 0.4, colours=c("darkblue", "black"),
                      fill_colour="slategray2", lines_size=0.6){

  # Settings
  dates <- cArima$dates[!is.na(cArima$y)]
  int.date <- cArima$int.date
  observed <- na.omit(cArima$y)
  fitted <- cArima$model$fitted
  forecasted <- na.omit(c(fitted, cArima$forecast))

  # forecasted_up<-forecasted_inf<-rep(NA, length(na.omit(cArima$model$fitted))) # it breaks with missing values
  forecasted_up<-forecasted_inf<-rep(NA, length(fitted[!is.na(fitted)]))
  forecasted_up<-append(forecasted_up, cArima$forecast_upper)
  forecasted_inf<-append(forecasted_inf, cArima$forecast_lower)

  start <- which(dates == int.date) - round(win * sum(dates < int.date))
  end <- length(forecasted)
  x <- dates[start:end]

  # Plot
  dat <- data.frame(x = x, forecasted.cut = forecasted[start:end], observed.cut = observed[start:end],
                    forecasted_up=forecasted_up[start:end], forecasted_inf=forecasted_inf[start:end])
  ylim <- c(min(dat[, -1], na.rm = T), max(dat[, -1], na.rm = T))

  g <- ggplot(data = dat, aes(x = x, colour = "Legend")) +  coord_cartesian(ylim = ylim) +  theme_bw(base_size = 15)+
    labs(title = "Forecasted series", y = "", x = "") +
     scale_colour_manual(values =colours ) +
    geom_vline(aes(xintercept = int.date, linetype = paste(int.date)), colour = "darkgrey", size = lines_size) +
    scale_linetype_manual(values = "longdash") +
    labs(color="Time series", linetype="Intervention date") +
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))+
    guides(color=guide_legend(override.aes=list(fill=NA)))+
    geom_lineribbon(aes(y = forecasted.cut, color = "Forecast", ymin = forecasted_inf, ymax = forecasted_up),
                    size = lines_size, linetype ="dashed", fill = fill_colour)+
    geom_line(aes(y = observed.cut, color = "Observed"), size = lines_size)

  if(!is.null(horizon)){
    g<-g+ geom_vline(xintercept = horizon, colour = "darkgrey", size = lines_size, linetype = "dotdash")
  }

  return(g)
}

# -----------------------------------------------------------------------------------------
qqplot.data <- function(vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids = vec)
  colnames(d) <- "resids"

  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)

}

.residuals <- function(cArima, max_lag=30){
  # Standardized residuals
  std.res <- scale(cArima$model$residuals)
  # Acf and Pacf
    ACF<-ggAcf(std.res, lag.max	=max_lag)+ ggtitle("Autocorrelation Function") +theme_bw(base_size = 15)
    PACF<-ggPacf(std.res, lag.max	=max_lag)+ ggtitle("Partial Autocorrelation Function") +  theme_bw(base_size = 15)

  # Normal QQ plot
  QQ_plot<-qqplot.data(std.res)+ggtitle("Normal Q-Q Plot") +
    xlab("Theoretical Quantiles") + ylab("Sample Quantiles")+theme_bw(base_size = 15)

  # return plots
  results<-list(ACF=ACF,PACF=PACF, QQ_plot=QQ_plot)
  return(results)
}
