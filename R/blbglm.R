#' @import tidyverse
#' @import purrr
#' @import furrr
#' @import future
#' @import stats
#' @importFrom magrittr %>%
#' @details Logistic Regression with Bag of Little Bootstraps
"_PACKAGE"

#' @export
blbglm <- function(formula, data, m = 2, B = 10, parallel = FALSE) {
  data_list <- split_data(data, m)
  if (isTRUE(parallel)){
    plan(multiprocess, workers = 4)
    estimates <- future_map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  } else {
    estimates <- map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  }

  m = m
  B = B
  parallel = parallel
  call <- match.call()
  res <- list(estimates = estimates, formula = formula, m=m, B=B, call = call, parallel = parallel)
  class(res) <- "blbglm"
  invisible(res)
}

#' This splits our data into a requested m sizes.
split_data <- function(data, m) {
  indx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(indx)
}


#' This computes estimates for the partitioned subsamples from split_data.
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}

#' Under BLB, this computes regression estimates.
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)
}


#' Based on freqs - the number of repetitions, this generates the glm model for each and determines the coefficients and sigmas of each BLB.
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs, family = binomial("logit"))
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}



#' Using fit, we extract the coefficients.
blbcoef <- function(fit) {
  coef <- fit$coefficients
}


#' Using fit, we calculate the sigmas.
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
  # Print the model called, coefficient estimates, sigma estimate, and size of subsamples and bootstraps used

  cat("\nblbglm model\n\nCall:\n ", paste(deparse(x$call)))
  if(length(coef.blbglm(x))) {
    cat("\n\nCoefficients\n")
    print.default(coef.blbglm(x))
  } else {cat("\nNo Coefficients\n\n")}

  cat("\nSigma:", sigma.blbglm(x))
  cat("\n\nSubsamples:", paste(x$m))
  cat("\nBootstrap Size:", paste(x$B))

}


#' @export
#' @method sigma blbglm
sigma.blbglm <- function(object, confidence = FALSE, level = 0.95, ...) {
  # Allow user to choose confidence level, default is 0.95
  if (is.null(level)) {
    level = 0.95
  } else {
    level = level
  }
  par <- object$parallel
  est <- object$estimates

  if(isTRUE(par)){
    sigma_list <- est %>%
      future_map(function(x){map(x,function(a){a[[2]]})})
    sigma_mat <- sigma_list %>%
      future_map(function(x){as.matrix(matrix(unlist(x), nrow=length(unlist(x[[1]]))))})
    sigma_mean_list = sigma_mat %>% future_map(mean)
    sigma_mean = Reduce("+", sigma_mean_list) / length(sigma_mean_list)
    sigma_ci_list = sigma_mat %>%
      future_map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})

  } else {
    sigma_list <- est %>%
      map(function(x){map(x,function(a){a[[2]]})})
    sigma_mat <- sigma_list %>%
      map(function(x){as.matrix(matrix(unlist(x), nrow=length(unlist(x[[1]]))))})
    sigma_mean_list = sigma_mat %>% map(mean)
    sigma_mean = Reduce("+", sigma_mean_list) / length(sigma_mean_list)
    sigma_ci_list = sigma_mat %>%
      map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})
  }
  sigma_ci = Reduce("+", sigma_ci_list) / length(sigma_ci_list)

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
  par <- object$parallel
  coef_names <- names(est[[1]][[1]]$coef)

  if(isTRUE(par)){
    coef_sub <- est %>%
      future_map(function(x){future_map(x,function(a){a[[1]]})}) %>%
      future_map(function(x){data.frame(Reduce(cbind, x))})
  }
  else{
    coef_sub <- est %>%
      map(function(x){map(x,function(a){a[[1]]})}) %>%
      map(function(x){data.frame(Reduce(cbind, x))})
  }
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
    if (isTRUE(object$parallel)){
      ci_list <- est %>%
        future_map(function(x){future_map(x,function(a){a[[1]]})}) %>%
        future_map(function(x){data.frame(Reduce(cbind, x))}) %>%
        future_map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})
    } else {
      ci_list <- est %>%
        map(function(x){map(x,function(a){a[[1]]})}) %>%
        map(function(x){data.frame(Reduce(cbind, x))}) %>%
        map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})
    }

  } else { # else for the parameters

    est <- object$estimates
    if (isTRUE(object$parallel)){
      ci_list <- est %>%
        future_map(function(x){future_map(x,function(a){a[[1]][parm]})}) %>%
        future_map(function(x){data.frame(Reduce(cbind, x))}) %>%
        future_map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})
    } else {
      ci_list <- est %>%
        map(function(x){map(x,function(a){a[[1]][parm]})}) %>%
        map(function(x){data.frame(Reduce(cbind, x))}) %>%
        map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))})
    }
  }
  confint <- t(Reduce("+", ci_list) / length(ci_list))
  confint
}

#' @export
#' @method predict blbglm
predict.blbglm <- function(object, newdata, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X = model.matrix(reformulate(attr(terms(object$formula), "term.labels")), newdata)
  new <- t(X)
  func <- function(x) {x*new}
  coef_sub <- est %>%
    map(function(x){map(x,function(a){a[[1]]})}) %>%
    map(function(x){data.frame(Reduce(cbind, x))})
  pred_sub_list <- coef_sub %>%
    map(function(x){lapply(x, FUN = function(a) new * a)}) %>%
    map(function(x){lapply(x, colSums)}) %>%
    map(function(x){data.frame(Reduce(cbind, x))})
  pred_sub = pred_sub_list %>%
    map(function(x){rowMeans(x)})
  pred <- Reduce("+", pred_sub)/length(pred_sub)
  if (confidence) {
    if (level) {
      level = level
    } else {
      level = 0.95
    }

    pred_ci <- pred_sub_list %>%
      map(function(x){apply(x, 1, quantile, c((1-level)/2, (1+level)/2))}) %>%
      Reduce("+", .) / length(pred_sub_list)

    pred_ci <- t(pred_ci)
    pred_and_ci <- cbind(pred, pred_ci)
    pred_and_ci

  } else {
    pred
  }
}
