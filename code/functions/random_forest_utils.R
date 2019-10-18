library(tidyverse)
library(Formula)
library(grf)
library(modelr)
library(furrr)
library(splines)

#===========================================================================
# RANDOM FOREST HELPER FUNCTIONS
#===========================================================================
overlap_average_treatment_effect <- function(cf) {
  W.resid <- cf$W.orig - cf$W.hat
  Y.resid <- cf$Y.orig - cf$Y.hat
  return(lm(Y.resid ~ W.resid))
}

# estimate treatment effects with a formula instead of manually passing matrices
# y ~ w | x1 + x2 + x3 means estimate a causal forest for the outcome y, where
# treatment is given by w, and we wish to match on x1, x2, and x3
# to get an average treatment effect (or ATT, etc), pass the results of this
# to the average_treatment_effect() routine in the grf package
causal_forest2 <- function(f, d, ...) {
  f <- Formula(f)

  Y <-
    f %>%
    formula(lhs = 1, rhs = 0) %>%
    model.frame(d) %>%
    as.matrix

  W <-
    formula(f, rhs = 1, lhs = 0) %>%
    update(~ 0 + as.numeric(.)) %>%
    model.matrix(d)

  X <-
    formula(f, rhs = 2, lhs = 0) %>%
    update(~ 0 + .) %>%
    model.matrix(d)

  cf <- causal_forest(X, Y, W, ...)

  cf[["formula"]] <- f
  class(cf) <- c("causal_forest", "grf")

  return(cf)
}

# estimate a regression forest with a formula instead of manually passing
# matrices.  the formula y ~ x1 + x2 + x3 means estimate a regression forest
# for the outcome y, where the splitting variables are x1, x2, and x3
# to get predicted values, pass the results of this to predict()
regression_forest2 <- function(f, d, ...) {
  f <- Formula(f)

  Y <-
    f %>%
    formula(lhs = 1, rhs = 0) %>%
    model.frame(d) %>%
    as.matrix

  X <-
    formula(f, rhs = 1, lhs = 0) %>%
    update(~ 0 + .) %>%
    model.matrix(d)

  ff <- regression_forest(X, Y, ...)
  
  ff[["formula"]] <- f
  class(ff) <- c("regression_forest", "grf")

  return(ff)
}

#===========================================================================
# DML specific code
#===========================================================================
# idea: psi is a function that gives the value of the neyman-orthogonal moment
# at a given value of theta, and psi_grad gives the gradient of it with respect
# to theta.  both functions take as inputs: theta (a scalar), Y (a vector),
# D (a vector), gamma (a vector of E[Y|X=x]) and delta (a vector of E[D|X=x])

# dml_estimate(Y, D, gamma, delta, psi, psi_grad, ...))
# lower = -5000, upper = 5000
# square <- function(x) x^2
square <- function(x) 0.5 * t(x) %*% x
dml_estimate <- function(Y, D, gamma, delta,
                         psi, psi_grad, psi_op,
                         bounds = NULL) {
  obj <- function(theta) {
    list(Y, D, gamma, delta) %>%
      pmap(function(Y, D, gamma, delta) {
        psi(theta, Y, D, gamma, delta)
      }) %>%
      reduce(`+`) / length(D)
  }

  grad <- function(theta) {
    list(Y, D, gamma, delta) %>%
      pmap(function(Y, D, gamma, delta) {
        psi_grad(theta, Y, D, gamma, delta)
      }) %>%
      reduce(`+`) / length(D)
  }

  theta0 <- rep(0, dim(D[[1]])[2])
  names(theta0) <- colnames(D[[1]])
  theta <-
    optim(theta0,
          function(x) square(obj(x)),
          function(x) 2 * grad(x) %*% obj(x),
          method = "BFGS",
          control = list(maxit = 500))

  J0 <-
    list(Y, D, gamma, delta) %>%
    pmap(function(Y, D, gamma, delta) {
      psi_grad(theta$par, Y, D, gamma, delta)
    }) %>%
    reduce(`+`) / length(D)

  s2 <-
    list(Y, D, gamma, delta) %>%
    pmap(function(Y, D, gamma, delta) {
      psi_op(theta$par, Y, D, gamma, delta)
    }) %>%
    reduce(`+`) / length(D)

  s2 <- solve(t(J0)) %*% s2 %*% solve(J0)

  # note that what we want for inference is actually s2/N, implemented in the
  # get_medians function below
  return(list(theta$par, s2))
}

predict_rf2 <- function(forest, newdata = NULL) {
  f <- forest[["formula"]]

  if(!is.null(newdata)) {
    X <-
      formula(f, rhs = 1, lhs = 0) %>%
      update(~ 0 + .) %>%
      model.matrix(newdata)
    
    return(pluck(predict(forest, X), "predictions"))
  } else {
    return(pluck(predict(forest), "predictions"))
  }
}

get_rhs_cols <- function(f, d, part = 1) {
  f %>%
    formula(rhs = part, lhs = 0) %>%
    model.matrix(d) %>%
    as_tibble %>%
    rename_all(~ str_replace_all(., regex("[(, =)]"), "_"))
}

get_lhs_col <- function(f, d) {
  f %>%
    formula(lhs = 1, rhs = 0) %>%
    model.frame(d) %>%
    as_tibble %>%
    rename_all(~ str_replace_all(., regex("[(, =)]"), "_"))
}

# estimate theta and s2 in a single sample split of the data
# formula should be y ~ d | x
dml_step <- function(f, d, psi, psi_grad, psi_op, ...) {
  # step 0: ensure fm has no intercept in the parametric part
  f <-
    Formula(f) %>%
    update(. ~ 0 + . | 0 + .)

  # step 1: make the estimation dataset
  # (a) expand out any non-linear formula for y and sanitize names
  ty <- get_lhs_col(f, d)
  
  # (b) expand out any non-linear formula for d and sanitize names
  td <- get_rhs_cols(f, d, 1)

  # (c) expand out any non-linear formula for x and sanitize names
  # NOTE: no real reason to have nonlinear formulae here but might as well
  # be robust to it
  tx <- get_rhs_cols(f, d, 2)
  
  # (c) make a new dataset of transformed y, transformed d and (transformed) x
  newdata <- bind_cols(ty, td, tx)

  # (d) finally, generate cross validation folds of this
  folds <- crossv_kfold(newdata)
  
  # step 2: save formulae for gamma and delta, based on transformed y and d
  xvars <- paste("0", paste0(names(tx), collapse = " + "), sep = " + ")
  
  f_gamma <-
    paste(names(ty), xvars, sep = " ~ ") %>%
    as.formula
  
  fs_delta <-
    names(td) %>%
    map(~ paste(., xvars, sep = " ~ ")) %>%
    map(as.formula)

  # step 3: train models for delta and gamma, ON THE TRAINING DATA
  gamma_models <- map(folds$train, function(y) {
    regression_forest2(f_gamma, d = y,
                       num.trees = 1000,
                       compute.oob.predictions = FALSE,
                       seed = rfseed)
  })

  delta_models <-
    folds$train %>%
    map(function(x) map(fs_delta, function(y) {
      regression_forest2(y, d = x,
                         num.trees = 1000,
                         compute.oob.predictions = FALSE,
                         seed = rfseed)
      }))

  # step 4: estimate values of delta and gamma IN THE TEST DATA
  gamma <-
    map2(gamma_models, folds$test, ~ predict_rf2(.x, .y)) %>%
    map(as.matrix)

  delta <-
    map2(delta_models, folds$test,
      function(x, y)
          map2(x, list(y), ~ predict_rf2(.x, .y)) %>%
          unlist %>%
          matrix(ncol = length(fs_delta)))

  # step 5: put together the values of Y and D in the hold out sample, and pass
  # things off to the dml_estimate routine
  ynames <- names(ty)
  Y <- map(folds$test, ~ as.matrix(select(as.data.frame(.), !!ynames)))

  dnames <- names(td)
  D <- map(folds$test, ~ as.matrix(select(as.data.frame(.), !!dnames)))

  return(dml_estimate(Y, D, gamma, delta, psi, psi_grad, psi_op, ...))
}

# DEFINE DML MOMENTS
# partial linear model
# recall this is (Y - gamma(X) - theta * (D - delta(X))) * (D - delta(X))
psi_plr <- function(theta, Y, D, gamma, delta) {
  theta <- matrix(theta, nrow = dim(D)[2])
  N <- nrow(Y)
  return((1 / N) * t(D - delta) %*% (Y - gamma - (D - delta) %*% theta))
}

psi_plr_grad <- function(theta, Y, D, gamma, delta) {
  N <- nrow(Y)  
  return(-1 * (1 / N) * t(D - delta) %*% (D - delta))
}

psi_plr_op <- function(theta, Y, D, gamma, delta) {
  theta <- matrix(theta, nrow = dim(D)[2])
  N <- nrow(Y)
  op <- (D - delta) * as.vector(Y - gamma - (D - delta) %*% theta)
  return((1 / N) * t(op) %*% op)
}

# partially linear poisson model
# recall this is:
# (Y - exp(D*theta) * gamma(X) * z(theta, X)) *
# (D - exp(theta) * delta(X) * z(theta, X))
# where z(theta, X) = 1 / (exp(theta) * delta(X) + 1 - delta(X))
psi_plpr_with_grad <- function(theta, Y, D, gamma, delta) {
  # first verify that D is univariate and 0/1 valued
  K <- ncol(D)
  nvals <- length(unique(D))
  minval <- min(D)
  maxval <- max(D)
  
  if(!(K == 1 & nvals == 2 & minval == 0 & maxval == 1)) {
    stop("treatment variable not univariate and binary")
  } else {
    z <- 1 / (exp(theta) * delta + 1 - delta)
    A <- (Y - exp(D * theta) * gamma * z)
    B <- (D - exp(theta) * delta * z)
    
    # psi is A * B so gradient is dA * B + A * dB
    dz <- -1 * (exp(theta) * delta) * z^2
    dA <- -1 * (D * exp(D * theta) * gamma * z + exp(D * theta) * gamma * dz)
    dB <- -1 * (exp(theta) * delta * z + exp(theta) * delta * dz)
    
    return(list(A * B, dA * B + A * dB))
  }
}

psi_plpr <- function(theta, Y, D, gamma, delta) {
  return(mean(psi_plpr_with_grad(theta, Y, D, gamma, delta)[[1]]))
}

psi_plpr_grad <- function(theta, Y, D, gamma, delta) {
  return(mean(psi_plpr_with_grad(theta, Y, D, gamma, delta)[[2]]))
}

psi_plpr_op <- function(theta, Y, D, gamma, delta) {
  vals <- psi_plpr_with_grad(theta, Y, D, gamma, delta)[[1]]
  return(mean(vals^2))
}

coef.dml <- function(x) x$coefficients
vcov.dml <- function(x) x$vcov
fitted.dml <- function(x) rep(0, x$nobs)
glance.dml <- function(x) tibble("r.squared" = NA_real_,
                                 "adj.r.squared" = NA_real_)

# final routine which takes a set of DML sample split estimates and returns the
# median point estimate and the "median" covariance matrix
# as suggested in the DML paper, the "median" covariance matrix is selected
# using the matrix operator norm, which is the highest svd of a matrix
get_medians <- function(estimates, n) {
  median_theta <-
    estimates %>%
    map(~ pluck(., 1)) %>%
    reduce(rbind) %>%
    apply(2, median)

  names(median_theta) <- names(estimates[[1]][[1]])

  medsq <- function(x) (x - median_theta) %*% t(x - median_theta)
  s2s <-
    estimates %>%
    map(~ pluck(., 2) + medsq(pluck(., 1)))

  s2_norms <-
    s2s %>%
    map_dbl(~ norm(., type = "2")) %>%
    enframe %>%
    filter(value == median(value))

  median_s2 <- s2s[[s2_norms$name]]

  return(structure(list(coefficients = median_theta,
                        vcov = (1 / n) * median_s2,
                        nobs = n),
                   class = "dml"))
}

# main user-facing routine
dml <- function(f, d, psi, psi_grad, psi_op, n = dml_n, nw = 4, ...) {
  plan(future::multisession, .init = nw)

  # to make the median well-defined, add 1 to n if the user requests an even
  # number of splits
  nn <- if_else(n %% 2 == 0, n + 1, n)

  seq(1, nn) %>%
    future_map(~ dml_step(f, d, psi, psi_grad, psi_op),
               .options = future_options(packages = c("splines"),
                                         seed = as.integer(rfseed))) %>%
    get_medians(nrow(d))
}

### try implementing a "fast" dml which just uses a single forest
dml_fast <- function(f, d, psi, psi_grad, psi_op) {
  # step 0: ensure fm has no intercept in the parametric part
  f <-
    Formula(f) %>%
    update(. ~ 0 + . | 0 + .)

  # step 1: make the estimation dataset
  # (a) expand out any non-linear formula for y and sanitize names
  ty <- get_lhs_col(f, d)
  
  # (b) expand out any non-linear formula for d and sanitize names
  td <- get_rhs_cols(f, d, 1)

  # (c) expand out any non-linear formula for x and sanitize names
  # NOTE: no real reason to have nonlinear formulae here but might as well
  # be robust to it
  tx <- get_rhs_cols(f, d, 2)
  
  # (c) make a new dataset of transformed y, transformed d and (transformed) x
  newdata <- bind_cols(ty, td, tx)

  # step 2: save formulae for gamma and delta, based on transformed y and d
  xvars <- paste("0", paste0(names(tx), collapse = " + "), sep = " + ")
  
  f_gamma <-
    paste(names(ty), xvars, sep = " ~ ") %>%
    as.formula
  
  fs_delta <-
    names(td) %>%
    map(~ paste(., xvars, sep = " ~ ")) %>%
    map(as.formula)

  # step 3: train models for delta and gamma
  gamma_model <- regression_forest2(f_gamma, newdata,
                                    num.trees = 10000,
                                    honesty = TRUE,
                                    honesty.fraction = NULL,
                                    seed = rfseed)

  delta_models <-
    fs_delta %>%
    map(~ regression_forest2(., newdata,
                             num.trees = 10000,
                             honesty = TRUE,
                             honesty.fraction = NULL,
                             seed = rfseed))

  # step 4: estimate OOB values of delta and gamma
  gamma <-
    predict_rf2(gamma_model) %>%
    as.matrix

  delta <-
    delta_models %>%
    map(predict_rf2) %>%
    unlist %>%
    matrix(ncol = length(fs_delta))

  # step 5: generate Y and D with useful names
  ynames <- names(ty)
  Y <- as.matrix(select(as.data.frame(newdata), !!ynames))

  dnames <- names(td)
  D <- as.matrix(select(as.data.frame(newdata), !!dnames))

  # step 6: do a single DML step 
  obj <- function(theta) psi(theta, Y, D, gamma, delta)

  grad <- function(theta) psi_grad(theta, Y, D, gamma, delta)

  theta0 <- rep(0, dim(D)[2])
  names(theta0) <- colnames(D)
  theta <-
    optim(theta0,
          function(x) square(obj(x)),
          function(x) 2 * grad(x) %*% obj(x),
          method = "BFGS",
          control = list(maxit = 500))

  J0 <- psi_grad(theta$par, Y, D, gamma, delta)

  s2 <- psi_op(theta$par, Y, D, gamma, delta)

  s2 <- solve(t(J0)) %*% s2 %*% solve(J0)

  n <- nrow(newdata)
  return(structure(list(coefficients = theta$par,
                        vcov = (1 / n) * s2,
                        nobs = n),
                   class = "dml"))
  
}
