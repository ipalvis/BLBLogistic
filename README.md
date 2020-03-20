# BLBLogistic
Bag of Little Bootstrap for Logistic Regression Model. Taking in a clean binary logit dataset, this package utilizes Bag of Little Bootstraps upon a logistic model to generate various estimated intervals and errors, including:

1) Logistic Regression coefficient estimates and their corresponding confidence intervals
2) Sigmas and its corresponding confidence interval
3) Prediction intervals and their corresponding prediction intervals

## To build an R-Package Skeleton
1) Create a new R package named "LogisticRegressionBLB", then run:
```r
library(dev_tools)
use_readme_md()
use_package("purrr")
use_package("magrittr")
use_roxygen_md()
use_namespace()
use_test("LogisticRegressionBLB")
use_r("blbglm.R")
```
