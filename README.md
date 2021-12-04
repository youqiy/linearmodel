# linearmodel

<!-- badges: start -->
[![R-CMD-check](https://github.com/youqiy/linearmodel/workflows/R-CMD-check/badge.svg)](https://github.com/youqiy/linearmodel/actions)
<!-- badges: end -->

## Overview
This R-package is designed for performing a linear regression model and some
basic anaylses:

* `lr()` returns a list of results, including fitted values, residuals, a
summary of coefficients, degree of freedom, r-squared, residual standard error,
and f-test.

A unique design in this package is that it can be used in model without 
intercept, and model with interactions. Also, the function can be applied for categorical variables.

## Installation
```
# install packages from github
install.packages("devtools")
devtools::install_github("youqiy/linearmodel", build_vignettes = T)

# generate the tutorial
browseVignettes("multilinear") 
```

## Usage
```
# Load the package
library(linearmodel)

# Prepare the data
library(NHANES)
library(dplyr)
data(NHANES)
data <- NHANES |> select("Age", "Race1", "Weight", "Height")
data <- na.omit(data)

# By default, the intercept is included, while the interactions are not.
lr(Age~Weight+Height, data)

# When intercept = FALSE, the model would not include the intercept.
lr(Age~Weight+Height, data, intercept = FALSE)

# When interaction = TRUE, the model would include the interactions between covariates.
lr(Age~Weight+Height, data, interact = TRUE)

# Regarding the categorical variables, if category = 1, a reference cell coding is performed.
lr(Age~Race1, data, category = 1)

# If category = 2, then a cell means coding is performed.
lr(Age~Race1, data, category = 2)
```
