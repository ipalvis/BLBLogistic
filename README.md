# BLBLogistic
Bag of Little Bootstrap for Logistic Regression Model. Taking in a clean binary logit dataset, this package utilizes Bag of Little Bootstraps upon a logistic model to generate various estimated intervals and errors, including:

1) Logistic Regression coefficient estimates and their corresponding confidence intervals
2) Sigmas and its corresponding confidence interval
3) Prediction intervals and their corresponding prediction intervals

## To load and build Package onto R
Open a new R file, and run:
```r
library(devtools)
devtools::install_github("ipalvis/BLBLogistic")
library(BLBLogistic)
```
Authors:
