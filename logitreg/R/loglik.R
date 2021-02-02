#' The logistic distribution function
#'
#' @param x vector of quantiles.
#' @return cummulativ distribution function of x.
#' @importFrom stats plogis
logistic <- function(x) stats::plogis(x)

#' Negative log likelihood and negative gradient computation
#'
#' @description
#' computes the negative log likelihood
#' and the gradient of the logistic density
#' for some data points in the design matrix.
#' @param coefs regression parameter vector.
#' @param design design matrix.
#' @param response binary dependent variable.
#' @return `neg_loglik` gives the negative log likelihood.
neg_loglik <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  -sum(response * log(probabilities) + (1 - response) * log(1 - probabilities))
}

#' @rdname neg_loglik
#' @return `neg_loglik_deriv` gives the gradient of the negative log likelihood.
neg_loglik_deriv <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  -t(response - probabilities) %*% design
}

#' Generate synthetic data sets for tests.
#'
#' @param n number of observations to create.
#' @param numerics number of gaussian variables (~N(0,1)).
#' @param factors number of dummy variables.
#' @param seed RNG seed.
#' @param dataframe logical value
#' specifying if the returned object is a list or a datafram.
#' @return list(x, y, b): design, response, true
#' coefs or data.frame with attribute('b').
#' @export
sim_data <- function(n = 1500, numerics = 3,
                     factors = 0, seed = NULL, dataframe = FALSE) {
  # set RNG seed for reproducibility:
  if (!is.null(seed)) {
    set.seed(seed)
  }

  covariates <- 1 + numerics + factors

  design <- matrix(0, nrow = n, ncol = covariates)
  design[, 1] <- 1
  design[, seq_len(numerics) + 1] <- matrix(rnorm(n * numerics), nrow = n)
  if (factors) {
    # add binary factors
    dummies <- matrix(sample(c(0, 1), n * factors, replace = TRUE), nrow = n)
    design[, -seq_len(1 + numerics)] <- dummies
  }

  coefs <- runif(covariates, min = -3, max = 3)

  probabilities <- logistic(design %*% coefs)
  response <- rbinom(n, prob = probabilities, size = 1)

  if (!dataframe) {
    return(list(design = design, response = response, coefs = coefs))
  }
  structure(
    data.frame(response = response, design = design[, -1, drop = FALSE]),
    coefs = coefs
  )
}
