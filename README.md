# BLBLogistic
Bag of Little Bootstrap for Binary Logistic Regression Model. Taking in a clean binary logit dataset, this package utilizes Bag of Little Bootstraps upon a binomial logistic model to generate various estimated intervals and errors, including:

1) Logistic Regression coefficient estimates and their corresponding confidence intervals
2) Sigma and its corresponding confidence interval
3) Prediction values and their corresponding prediction intervals

This package was created as a final project for STA 141C, Winter Quarter 2020, UC Davis.

## To load and build Package onto R
Open a new R file, and run:
```r
library(devtools)
devtools::install_github("ipalvis/BLBLogistic")
library(BLBLogistic)
```
## Authors
Alvis Ip  
Koral Buch  
Nikhila Thota  
Songwen Su
