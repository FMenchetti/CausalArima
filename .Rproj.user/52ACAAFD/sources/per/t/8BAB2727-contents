######################################################################################
######################################################################################
####  Authors:           Fiammetta Menchetti                                      ####
####                     Fabrizio Cipollini                                       ####
####                                                                              ####
####  Date last update: 2021-02-03                                                ####
####                                                                              ####
####  Content:          Plot method for object of class cVAR                      ####
####                                                                              ####
####  Main function :   plot.cVAR                                                 ####
####  Dependencies:     .impact                                                   ####
####                    .forecast                                                 ####
####                    .residuals                                                ####
######################################################################################
######################################################################################

#' Plotting method for object of class cVAR
#'
#' @param x Object of class \code{cVAR}.
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
#' # Work in progress

plot.cVAR <- function(x, int.date, type = c("forecast", "impact", "residuals"), ...){

  # param checks
  if(class(x) != "cVAR") stop ("`x` must be an object of class cVAR")
  if(!any(class(int.date) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`int.date` must be a Date of length 1")
  if(!all(type %in% c("forecast", "impact", "residuals")))
    stop("allowed 'type' values are 'forecast', 'impact' and 'residuals'")

  # Plot "Observed vs Forecast"
  if("forecast" %in% type){
    .forecastVAR(x, int.date = int.date, ...)
  }

  # Plot "Causal effect"
  if("impact" %in% type){
    .impactVAR(x, int.date = int.date, ...)
  }

  # Residuals plots
  if("residuals" %in% type){
    .residualsVAR(x)
  }

  return(invisible())
}

# -----------------------------------------------------------------------------------------

.forecastVAR <- function(cVAR, int.date, ...){
  dates <- cVAR$dates
  d <- dim(cVAR$y)[2]
  observed <- cVAR$y
  start <- which(dates == int.date) - round(0.4 * sum(dates < int.date))

  if(!is.list(cVAR$effect)){
    g <- list()
    for(j in 1:d){
      forecasted <- c(fitted(cVAR$model)[,j], cVAR$forecast.mean[,j])
      end <- length(forecasted)
      x <- dates[start:(end - 1)]
      dat <- data.frame(x = x, forecasted.cut = forecasted[start:(end - 1)], observed.cut = observed[start:(end - 1), j])
      ylim <- c(min(dat[, -1]), max(dat[, -1]))
      main <- paste("Forecasted series ", j, sep = "")
      g[[j]] <- ggplot(data = dat, aes(x = x, colour = "Legend")) +  coord_cartesian(ylim = ylim) + labs(title = main, y = "", x = "") +
        geom_line(aes(y = forecasted.cut, color = "Forecast"))  +
        geom_line(aes(y = observed.cut, color = "Observed")) +
        scale_colour_manual(values = c("deepskyblue", "gray40")) +
        geom_vline(aes(xintercept = int.date, linetype = paste(int.date))) +
        scale_linetype_manual(values = "dashed") +
        labs(color="Time series", linetype="Intervention date") +
        guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))
    }

    do.call(grid.arrange, c(g, ...))

  } else {

    for(i in 1:length(cVAR$effect)){
      g <- list()
      for(j in 1:d){
        forecasted <- c(fitted(cVAR$model)[,j], cVAR$forecast.mean[[i]][,j])
        end <- length(forecasted)
        x <- dates[start:(end - 1)]
        dat <- data.frame(x = x, forecasted.cut = forecasted[start:(end - 1)], observed.cut = observed[start:(end - 1), j])
        ylim <- c(min(dat[, -1]), max(dat[, -1]))
        main <- paste("Forecasted series ", j, sep = "")
        sub <- paste("Time horizon ", i, sep = "")
        g[[j]] <- ggplot(data = dat, aes(x = x, colour = "Legend")) +  coord_cartesian(ylim = ylim) + labs(title = main, subtitle = sub, y = "", x = "") +
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
}

# -----------------------------------------------------------------------------------------

.impactVAR <- function(cVAR, int.date, ...){
  dates <- cVAR$dates
  d <- dim(cVAR$y)[2]

  if(!is.list(cVAR$effect)){
    g <- list()
    for(j in 1:d){
      x <- dates[dates >= int.date]
      main <- paste("Point effect series ", j, sep = "")
      dat <- data.frame(x = x, y = cVAR$effect.mean[,j], y.upper = cVAR$effect.upper[,j], y.lower = cVAR$effect.lower[,j])
      ylim <- c(min(dat[, "y.lower"]), max(dat[, "y.upper"]))
      g[[j]] <- ggplot(data = dat, aes(x = x)) +  coord_cartesian(ylim = ylim) + labs(title = main, y = "", x = "") +
        geom_line(aes(y = y), color = "navy") +
        geom_ribbon(aes(x = x, ymax = y.upper, ymin = y.lower), fill="steelblue", alpha=.5)

    }

    do.call(grid.arrange, c(g, ...))

  } else {
    for(i in 1:length(cVAR$effect)){
      g <- list()
      for(j in 1:d){
        start <- which(dates == int.date)
        end <- start + length(cVAR$effect.mean[[i]][,j])
        x <- dates[start:(end -1)]
        dat <- data.frame(x = x, y = cVAR$effect.mean[[i]][,j], y.upper = cVAR$effect.upper[[i]][,j], y.lower = cVAR$effect.lower[[i]][,j])
        ylim <- c(min(dat[, "y.lower"]), max(dat[, "y.upper"]))
        main <- paste("Point effect series ", j, sep = "")
        sub <- paste("Time horizon ", i, sep = "")
        g[[j]] <- ggplot(data = dat, aes(x = x)) +  coord_cartesian(ylim = ylim) + labs(title = main, subtitle = sub, y = "", x = "") +
          geom_line(aes(y = y), color = "navy") +
          geom_ribbon(aes(x = x, ymax = y.upper, ymin = y.lower), fill="steelblue", alpha=.5)
      }

      do.call(grid.arrange, c(g, ...))

    }
  }
}
# -----------------------------------------------------------------------------------------

.residualsVAR <- function(cVAR){

  # Standardized residuals
  std.res <- apply(resid(cVAR$model), 2, scale)

  for(i in 1:dim(std.res)[2]){

    # Acf and Pacf
    Acf(std.res[, i], main = "") ; title(paste("Acf series ", i, sep = ""))
    Pacf(std.res[, i], main = "") ; title(paste("Pacf series ", i, sep = ""))

    # Normal QQ plot
    qqnorm(std.res[, i], main = "") ; abline(0,1)
    title(paste("Normal QQ-plot series ", i, sep = ""))
  }
}
