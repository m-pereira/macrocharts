
<!-- README.md is generated from README.Rmd. Please edit that file -->

# macrocharts

<!-- badges: start -->
<!-- badges: end -->

The goal of macrocharts is to help macroeconomics students to understand
effects of the IS-LM model.

This package allows you to create a theoretical charts easily, our main
goal is make easier teach macroeconomics to students, as example the
“investment-savings” (IS) and “liquidity preference-money supply” that
has a lot of varieties, with open economy or closed economy. Our package
help the visualization of the impacts of variables.

## Installation

You can install the development version of macrocharts from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("m-pereira/macrocharts")
```

## Example

This is a basic example which shows you how create a shiny app for
IS-LM:

``` example
## load the package
library(macrocharts)
## create a shiny app with the 
chart_IS_LM()
```
