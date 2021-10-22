
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
#> Loading required package: tidybayes
#> Loading required package: gridExtra

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

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

How to obtain the plot of the estimated effects and cumulative effects:

``` r

impact<-plot(ce, type="impact", printing=FALSE)
grid.arrange(impact$plot, impact$cumulative_plot)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

How to obtain a summary of the model (notice that to proper display the
results as html on a Rmarkdown chunk you have to set result as ‘asis’.
Other possible format include “numeric”, useful to retrieve the
statistics and use them in calculations, and “latex”.)

``` r
summary_model<-CoefficientsTable(ce, printing=FALSE, format="html", boot=10000, alfa = 0.05, bootstrapping=FALSE, cov=x1)
summary_model$arima
```

$arima\_order

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

p

</th>

<th style="text-align:right;">

d

</th>

<th style="text-align:right;">

q

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

arima\_order

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

</tbody>

</table>

$param

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

coef

</th>

<th style="text-align:right;">

se

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

xreg

</td>

<td style="text-align:right;">

1.199333

</td>

<td style="text-align:right;">

0.0016581

</td>

</tr>

</tbody>

</table>

$accuracy

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ME

</th>

<th style="text-align:right;">

RMSE

</th>

<th style="text-align:right;">

MAE

</th>

<th style="text-align:right;">

MPE

</th>

<th style="text-align:right;">

MAPE

</th>

<th style="text-align:right;">

MASE

</th>

<th style="text-align:right;">

ACF1

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Training set

</td>

<td style="text-align:right;">

0.0043228

</td>

<td style="text-align:right;">

1.202503

</td>

<td style="text-align:right;">

0.9464393

</td>

<td style="text-align:right;">

\-0.0051705

</td>

<td style="text-align:right;">

0.9072633

</td>

<td style="text-align:right;">

0.5734012

</td>

<td style="text-align:right;">

0.1407503

</td>

</tr>

</tbody>

</table>

$log\_stats

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

loglik

</th>

<th style="text-align:right;">

aic

</th>

<th style="text-align:right;">

bic

</th>

<th style="text-align:right;">

aicc

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

metrics

</td>

<td style="text-align:right;">

\-112.234

</td>

<td style="text-align:right;">

228.4681

</td>

<td style="text-align:right;">

232.9651

</td>

<td style="text-align:right;">

228.6472

</td>

</tr>

</tbody>

</table>

and of the summary of the casual impact:

``` r
summary_model$impact
```

$avg

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

x

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

avg

</td>

<td style="text-align:right;">

10.3569824

</td>

</tr>

<tr>

<td style="text-align:left;">

sd.avg

</td>

<td style="text-align:right;">

0.2211311

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.avg.l

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.avg.b

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.avg.r

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

</tbody>

</table>

$sum

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

x

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

sum

</td>

<td style="text-align:right;">

310.709471

</td>

</tr>

<tr>

<td style="text-align:left;">

sd.sum

</td>

<td style="text-align:right;">

6.633933

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.sum.l

</td>

<td style="text-align:right;">

1.000000

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.sum.b

</td>

<td style="text-align:right;">

0.000000

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.sum.r

</td>

<td style="text-align:right;">

0.000000

</td>

</tr>

</tbody>

</table>

$tau

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

x

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

tau

</td>

<td style="text-align:right;">

12.257154

</td>

</tr>

<tr>

<td style="text-align:left;">

sd.tau

</td>

<td style="text-align:right;">

1.211185

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.tau.l

</td>

<td style="text-align:right;">

1.000000

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.tau.b

</td>

<td style="text-align:right;">

0.000000

</td>

</tr>

<tr>

<td style="text-align:left;">

pvalue.tau.r

</td>

<td style="text-align:right;">

0.000000

</td>

</tr>

</tbody>

</table>

and of the summary of the casual impact based on boostrap:

``` r
summary_model$impact_boot
```

$average

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

estimates

</th>

<th style="text-align:right;">

inf

</th>

<th style="text-align:right;">

sup

</th>

<th style="text-align:right;">

sd

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

observed

</td>

<td style="text-align:right;">

117.0485168

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

forecasted

</td>

<td style="text-align:right;">

106.6915345

</td>

<td style="text-align:right;">

106.3420568

</td>

<td style="text-align:right;">

106.9277643

</td>

<td style="text-align:right;">

0.1861650

</td>

</tr>

<tr>

<td style="text-align:left;">

absolute\_effect

</td>

<td style="text-align:right;">

10.3569824

</td>

<td style="text-align:right;">

10.1207525

</td>

<td style="text-align:right;">

10.7064600

</td>

<td style="text-align:right;">

0.1861650

</td>

</tr>

<tr>

<td style="text-align:left;">

relative\_effect

</td>

<td style="text-align:right;">

0.0970741

</td>

<td style="text-align:right;">

0.0948599

</td>

<td style="text-align:right;">

0.1003497

</td>

<td style="text-align:right;">

0.0017449

</td>

</tr>

</tbody>

</table>

$effect\_cum

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

estimates

</th>

<th style="text-align:right;">

inf

</th>

<th style="text-align:right;">

sup

</th>

<th style="text-align:right;">

sd

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

observed

</td>

<td style="text-align:right;">

3511.4555050

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

forecasted

</td>

<td style="text-align:right;">

3200.7460337

</td>

<td style="text-align:right;">

3190.2617046

</td>

<td style="text-align:right;">

3207.8329294

</td>

<td style="text-align:right;">

5.5849514

</td>

</tr>

<tr>

<td style="text-align:left;">

absolute\_effect

</td>

<td style="text-align:right;">

310.7094713

</td>

<td style="text-align:right;">

303.6225756

</td>

<td style="text-align:right;">

321.1938004

</td>

<td style="text-align:right;">

5.5849514

</td>

</tr>

<tr>

<td style="text-align:left;">

relative\_effect

</td>

<td style="text-align:right;">

0.0970741

</td>

<td style="text-align:right;">

0.0948599

</td>

<td style="text-align:right;">

0.1003497

</td>

<td style="text-align:right;">

0.0017449

</td>

</tr>

</tbody>

</table>

$p\_values

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

x

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

alpha

</td>

<td style="text-align:right;">

0.05

</td>

</tr>

<tr>

<td style="text-align:left;">

p

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

</tbody>

</table>

How to inspect the residuals, with the plots of autocorrelation (ACF)
and partial autocorrelation (PACF) functions and QQ-plots:

``` r
residuals<-plot(ce, type="residuals", printing=FALSE)
grid.arrange(residuals$ACF, residuals$PACF, residuals$QQ_plot)
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

## Modify the plots

The plotting functions have some graphical parameters that make easier
to personalize the plots:

``` r
forecasted_2<-plot(ce, type="forecast", printing=FALSE, fill_colour="orange",
               colours=c("red", "blue"))
forecasted_2
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

All plotting functions return a ggplot object or a list of ggplot
objects, which makes easy to modify any ggplot parameters of the theme.
The ggthemes package can be useful to employ directly some
pre-customized themes, for example we can use the Wall Street Journal
theme simply typing:

``` r
library(ggthemes)
forecasted+theme_wsj()
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

## Learn more

You can read more on [Estimating the causal effect of an intervention in
a time series setting: the C-ARIMA
approach](https://arxiv.org/abs/2103.06740) (Fiammetta Menchetti,
Fabrizio Cipollini, Fabrizia Mealli, 2021).

It is also available on youtube a video of a webinar on the topic:
[Fiammetta Menchetti: Estimating the causal effect of an intervention in
a time series setting](https://www.youtube.com/watch?v=RjMEtv3C5S0).
