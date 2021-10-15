
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CausalArima

<!-- badges: start -->

<!-- badges: end -->

The goal of CausalArima is to estimates the causal effect of an
intervention on a univariate time series using ARIMA models.

## Installation

<!-- You can install the released version of CausalArima from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("CausalArima") -->

<!-- ``` -->

You can install the development version of CausalArima from from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FMenchetti/CausalArima")
```

## Example

This is a basic example which shows you how to use the package:

``` r
library(CausalArima)
#> Loading required package: forecast
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> Loading required package: ggplot2
#> Loading required package: gridExtra
#> 
#> Attaching package: 'CausalArima'
#> The following object is masked from 'package:base':
#> 
#>     table

# simulate data
n<-100
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = n)
y <- 1.2 * x1 + rnorm(n)
y[ floor(n*.71):n] <- y[ floor(n*.71):n] + 10
data <- cbind(y, x1)
dates <- seq.Date(from = as.Date("2014-01-05"), by = "days", length.out = n)
start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))

# Adding a fictional intervention
int.date <- as.Date("2014-03-161")

# fit the model - Causal effect estimation
ce <- CausalArima(y = ts(y, start = start, frequency = 1), auto = TRUE, ic = "aicc", dates = dates, int.date = int.date,
                  xreg =x1)
```

How to obtain the plot of the forecast:

``` r
forecasted<-plot(ce, type="forecast", printing=FALSE)
forecasted
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

How to obtain the plot of the estimated effects and cumulative effects:

``` r

impact<-plot(ce, type="impact", printing=FALSE)
grid.arrange(impact$plot, impact$cumulative_plot)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

How to obtain the estimates of the effects:

``` r
print(ce)
#>                                       
#> Point causal effect            12.257 
#> Standard error                 1.211  
#> Left-sided p-value             1      
#> Bidirectional p-value          0      
#> Right-sided p-value            0      
#>                                       
#> Cumulative causal effect       310.709
#> Standard error                 6.634  
#> Left-sided p-value             1      
#> Bidirectional p-value          0      
#> Right-sided p-value            0      
#>                                       
#> Temporal average causal effect 10.357 
#> Standard error                 0.221  
#> Left-sided p-value             1      
#> Bidirectional p-value          0      
#> Right-sided p-value            0
table(ce)
#>     2014-04-14 
#> avg 10.36^{***}
#> sd  (0.22)
```

How to inspect the residuals, with the plots of autocorrelation (ACF)
and partial autocorrelation (PACF) functions and QQ-plots:

``` r
residuals<-plot(ce, type="residuals", printing=FALSE)
grid.arrange(residuals$ACF, residuals$PACF, residuals$QQ_plot)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Modify the plots

The plotting functions have some graphical parameters that make easier
to personalize the plots:

``` r
forecasted_2<-plot(ce, type="forecast", printing=FALSE, fill_colour="orange",
               colours=c("red", "blue"))
forecasted_2
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

All plotting functions return a ggplot object or a list of ggplot
objects, which makes easy to modify any ggplot parameters of the theme
([here](https://ggplot2.tidyverse.org/reference/theme.html) a reference
to the many ggplot parameters that can be modified):

``` r
forecasted+ theme(legend.text = element_text(size=10, colour = "red", family ="mono"),
                legend.title = element_text(size=14, colour = "blue", family ="sans"),
                title= element_text(size=20, colour = "green", family ="serif"),
                axis.line=element_line(size=2, colour="yellow"),
                legend.position = "bottom"
                )
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

The ggthemes package can be useful to employ directly some
pre-customized themes, for example we can use the Wall Street Journal
theme simply typing:

``` r
library(ggthemes)
forecasted+theme_wsj()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

## Learn more

You can read more on [Estimating the causal effect of an intervention in
a time series setting: the C-ARIMA
approach](https://arxiv.org/abs/2103.06740) (Fiammetta Menchetti,
Fabrizio Cipollini, Fabrizia Mealli, 2021).

It is also available on youtube a video of a webinar on the topic:
[Fiammetta Menchetti: Estimating the causal effect of an intervention in
a time series setting](https://www.youtube.com/watch?v=RjMEtv3C5S0).