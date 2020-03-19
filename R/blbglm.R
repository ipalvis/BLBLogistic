#' @import tidyverse
#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @details
# Logistic Regression with Little Bag of Bootstraps
"_PACKAGE"

#' @export
blbglm <- function(formula, data, m = 2, B = 10, parallel = FALSE) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  m = m
  B = B
  res <- list(estimates = estimates, formula = formula, m=m, B=B)
  class(res) <- "blbglm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  indx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(indx)
}


#' compute the estimates
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)

}


#' estimate the regression estimates based on given number of repetitions
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs, family = binomial("logit"))
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef <- fit$coefficients
}


#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights

  sqrt(sum(e^2*w)/(p-1))
}


#' @export
#' @method print blbglm
print.blbglm <- function(x, ...) {
  cat("blblm model")
}


#' @export
#' @method sigma blbglm
sigma.blbglm <- function(object, confidence = FALSE, level = 0.95, ...) {
  if (is.null(level)) {
    level = 0.95
  } else {
    level = level
  }

  est <- object$estimates
  sigma_list <- est %>%
    map(function(x){map(x,function(a){a[[2]]})})

  sigma_mat <- sigma_list %>%
    map(function(x){as.matrix(matrix(unlist(x), nrow=length(unlist(x[[1]]))))})

  mean_list = sigma_mat %>% map(mean)
  sigma_mean = Reduce("+", mean_list) / length(mean_list)

  ci_list = sigma_mat %>%
    map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})
  sigma_ci = Reduce("+", ci_list) / length(ci_list)

  colnames(sigma_ci) <- "sigma"
  sigma_ci <- t(sigma_ci)

  if(confidence == FALSE) {
    sigma_mean
  } else {
    sigma_ci
  }
}


#' @export
#' @method coef blbglm
coef.blbglm <- function(object, ...) {
  est <- object$estimates
  coef_names <- names(est[[1]][[1]]$coef)

  coef_sub <- est %>%
    map(function(x){map(x,function(a){a[[1]]})}) %>%
    map(function(x){data.frame(Reduce(cbind, x))})

  coef_sub_means <- map(coef_sub, rowMeans)
  coef_mean <- as.matrix(Reduce("+", coef_sub_means)/length(coef_sub_means))
  colnames(coef_mean) <- "Estimates"
  coef_mean
}


#' @export
#' @method confint blbglm
confint.blbglm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(level)) {
    level = 0.95
  } else {
    level = level
  }

  if (is.null(parm)) {
    est <- object$estimates
    ci_list <- est %>%
      map(function(x){map(x,function(a){a[[1]]})}) %>%
      map(function(x){data.frame(Reduce(cbind, x))}) %>%
      map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})


  } else {
    est <- object$estimates
    ci_list <- est %>%
      map(function(x){map(x,function(a){a[[1]][parm]})}) %>%
      map(function(x){data.frame(Reduce(cbind, x))}) %>%
      map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})
  }

  confint <- t(Reduce("+", ci_list) / length(ci_list))
  confint
}

#' @export
#' @method predict blbglm
predict.blbglm <- function(object, newdata, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), newdata)
  if (confidence) {
    # YOUR CODE to compute the predictions and their confidence intervals
  } else {
    # YOUR CODE to compute the predictions
  }
}


