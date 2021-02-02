#' Fit logistic regression
#'
#' @description
#' Finds the MLE of regression coefficients using numerical optimization.
#'
#' @title logitreg: logistic regression function.
#' @param object object to apply logistic regression on.
#' @param ... other arguments.
#' @return A list of class "logitreg".
#' containing the optimized coefficients,
#' fitted and predicted response probabilities
#' and the original design matrix.
#' @export
logitreg <- function(object, ...) {
  UseMethod("logitreg")
}

#' @param object design matrix.
#' @param response binary dependent variable.
#' @param ... further arguments passed to the `optim`
#' function used for optimizaiton.
#' @import stats
#' @rdname logitreg
#' @export
logitreg.default <- function(object, response, ...) {
  design <- na.omit(object)
  if (!is.null(attr(design, "na.action"))) {
    response <- response[-attr(design, "na.action")]
  }
  result <- stats::optim(
    par = c(0, rep(1, ncol(design) - 1)),
    fn = neg_loglik,
    gr = neg_loglik_deriv,
    design = design,
    response = response,
    ...
  )
  if (result$convergence != 0) {
    warning("Optim algorithm did not converge")
  }
  coefficients <- result$par
  predict <- design %*% coefficients
  fitted <- logistic(predict)
  structure(
    list(
      coefficients = coefficients,
      fitted = fitted,
      predict = predict,
      design = design,
      response = response
    ),
    class = "logitreg"
  )
}

#' Fit logistic regression for formula objects
#'
#' @param formula an object of class "formula".
#' (or one that can be coerced to that class).
#' @param data a data frame.
#' @param ... other arguments used by `optim`.
#' @rdname logitreg
#' @export
logitreg.formula <- function(formula, data, ...) {
  design <- model.matrix(formula, data)
  response <- model.response(model.frame(formula, data = data))
  logitreg.default(design, response, ...)
}

#' Predict function for logitreg object
#'
#' @param object a model object for which prediction is desired.
#' @param type the type of prediction required.
#' @param ... additional arguments affecting the predictions produced.
#' @return a vector of preditions.
#' @export
predict.logitreg <- function(object, type = "link", ...) {
  if (type == "response") {
    predictor <- as.vector(object$predict)
    return(1 / (1 + exp(-predictor)))
  }
  as.vector(object$predict)
}

#' Plot ROC curve
#'
#' @param ... arguments to pass to ROCR::plot.
#' @method plot logitreg
#' @import ROCR
#' @import stats
#' @examples
#' data <- logitreg::sim_data(300, seed = 321, numerics = 2, dataframe = TRUE)
#' model <- logitreg(response ~ ., data = data)
#' pred <- ROCR::prediction(predict(model,
#'   type = "response"
#' ), data$response)
#' perf <- ROCR::performance(pred, "acc")
#' plot(perf)
#' @export
plot.logitreg <- function(...) {
  ROCR::plot(...)
}
