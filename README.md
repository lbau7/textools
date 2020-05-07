
<!-- README.md is generated from README.Rmd. -->

[![Build
Status](https://travis-ci.com/lbau7/textools.svg?branch=master)](https://travis-ci.com/lbau7/textools)

# textools

textools create nice looking LaTeX tables.

## Installation

If you only want to use the packages:

``` r
# install.packages("devtools")
devtools::install_github("lbau7/textools")
```

If you also want to build the vignettes (attention: this will install
all suggested packages):

``` r
# install.packages("devtools")
devtools::install_github("lbau7/textools", build_vignettes = TRUE)
```

Then you can check out the vignettes:

``` r
browseVignettes("textools")
```

## Usage

The main function of textools is `texmod()` which creates LaTeX tables
for the regression output of various models.

``` r
library(textools)

iris.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Species, data = iris)
texmod(iris.lm)
```

Check out the vignettes for some examples.
