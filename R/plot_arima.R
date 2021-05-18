######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-02-03                                                ####
####                                                                              ####
####  Content:          Plot method for object of class cArima                    ####
####                                                                              ####
####  Main function :   plot.cArima                                               ####
####  Dependencies:     .impact                                                   ####
####                    .forecast                                                 ####
####                    .residuals                                                ####
######################################################################################
######################################################################################

#' Plotting method for object of class cArima
#'
#' @param x Object of class \code{cArima}.
#' @param int.date Date of the intervention (required for types "forecast" and "impact").
#' @param type Character string indicating the plot to be produced. Possible values
#'             in c("forecast", "impact", "residuals").
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
#' in the pre-intervention period as well as the Normal Q-Q Plot.
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
#' # Causal effect estimation
#' start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
#' ce <- CausalArima(y = ts(y.new, start = start, frequency = 1), auto = TRUE, ic = "aic", dates = dates, int.date = int.date)
#'
#' # Plot
#' plot(ce, type = "forecast", int.date = int.date)
#' plot(ce, type = "impact", int.date = int.date)
#' plot(ce, type = "residuals")
#'
plot.cArima <- function(x, int.date, type = c("forecast", "impact", "residuals")){

  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")
  if(!any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`int.date` must be a Date of length 1")
  if(!all(type %in% c("forecast", "impact", "residuals")))
    stop("allowed 'type' values are 'forecast', 'impact' and 'residuals'")

  # Plot "Observed vs Forecast"
  if("forecast" %in% type){
    .forecast(x, int.date = int.date, ...)
  }

  # Plot "Causal effect"
  if("impact" %in% type){
    .impact(x, int.date = int.date, ...)
  }

  # Residuals plots
  if("residuals" %in% type){
    .residuals(x)
  }

  return(invisible())
}

# -----------------------------------------------------------------------------------------

.impact <- function(cArima, int.date, ...){
  dates <- cArima$dates

  if(!is.list(cArima$effect)){
    x <- dates[dates >= int.date]
    main <- "Point effect"
    dat <- data.frame(x = x, y = cArima$effect.mean, y.upper = cArima$effect.upper, y.lower = cArima$effect.lower)
    ylim <- c(min(dat[, "y.lower"]), max(dat[, "y.upper"]))
    ggplot(data = dat, aes(x = x)) +  coord_cartesian(ylim = ylim) + labs(title = main, y = "", x = "") +
      geom_line(aes(y = y), color = "navy") +
      geom_ribbon(aes(x = x, ymax = y.upper, ymin = y.lower), fill="steelblue", alpha=.5)

  } else {
    g <- list()
    for(i in 1:length(cArima$effect)){
      start <- which(dates == int.date)
      end <- start + length(cArima$forecast.mean[[i]])
      x <- dates[start:(end -1)]
      dat <- data.frame(x = x, y = cArima$effect.mean[[i]], y.upper = cArima$effect.upper[[i]], y.lower = cArima$effect.lower[[i]])
      ylim <- c(min(dat[, "y.lower"]), max(dat[, "y.upper"]))
      main <- paste("Point effect")
      sub <- paste("Time horizon ", i, sep = "")
      g[[i]] <- ggplot(data = dat, aes(x = x)) +  coord_cartesian(ylim = ylim) + labs(title = main, subtitle = sub, y = "", x = "") +
        geom_line(aes(y = y), color = "navy") +
        geom_ribbon(aes(x = x, ymax = y.upper, ymin = y.lower), fill="steelblue", alpha=.5)
    }
    do.call(grid.arrange, c(g, ...))
  }
}

# -----------------------------------------------------------------------------------------

.forecast <- function(cArima, int.date, ...){
  dates <- cArima$dates
  observed <- cArima$y
  start <- which(dates == int.date) - round(0.4 * sum(dates < int.date))

  if(!is.list(cArima$effect)){
    forecasted <- c(cArima$model$fitted, cArima$forecast.mean)
    end <- length(forecasted)
    x <- dates[start:(end - 1)]
    dat <- data.frame(x = x, forecasted.cut = forecasted[start:(end - 1)], observed.cut = observed[start:(end - 1)])
    ylim <- c(min(dat[, -1]), max(dat[, -1]))
    main <- "Forecasted series"
    ggplot(data = dat, aes(x = x, colour = "Legend")) +  coord_cartesian(ylim = ylim) + labs(title = main, y = "", x = "") +
      geom_line(aes(y = forecasted.cut, color = "Forecast"))  +
      geom_line(aes(y = observed.cut, color = "Observed")) +
      scale_colour_manual(values = c("deepskyblue", "gray40")) +
      geom_vline(aes(xintercept = int.date, linetype = paste(int.date))) +
      scale_linetype_manual(values = "dashed") +
      labs(color="Time series", linetype="Intervention date") +
      guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))

  } else {
    g <- list()
    for(i in 1:length(cArima$effect)){
      forecasted <- c(cArima$model$fitted, cArima$forecast.mean[[i]])
      end <- length(forecasted)
      x <- dates[start:(end - 1)]
      dat <- data.frame(x = x, forecasted.cut = forecasted[start:(end - 1)], observed.cut = observed[start:(end - 1)])
      ylim <- c(min(dat[, -1]), max(dat[, -1]))
      main <- "Forecasted series"
      sub <- paste("Time horizon ", i, sep = "")
      g[[i]] <-ggplot(data = dat, aes(x = x, colour = "Legend")) +  coord_cartesian(ylim = ylim) + labs(title = main, subtitle = sub, y = "", x = "") +
        geom_line(aes(y = forecasted.cut, color = "Forecast"))  +
        geom_line(aes(y = observed.cut, color = "Observed")) +
        scale_colour_manual(values = c("deepskyblue", "gray40")) +
        geom_vline(aes(xintercept = int.date, linetype = paste(int.date))) +
        scale_linetype_manual(values = "dashed") +
        labs(color="Time series", linetype="Intervention date") +
        guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))
    }

    do.call(grid.arrange, c(g, ...))

  }
}
# -----------------------------------------------------------------------------------------

.residuals <- function(cArima){
  # Standardized residuals
  std.res <- scale(cArima$model$residuals)

  # Acf and Pacf
  Acf(std.res, main = "") ; title("Acf")
  Pacf(std.res, main = "") ; title("Pacf")

  # Normal QQ plot
  qqnorm(std.res) ; abline(0,1)

}
