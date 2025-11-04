#' @include LearnerSubspaceElips.R
#' @title coef method for LearnerSubspaceElips
#' @param object A LearnerSubspaceElips object
#' @param ... Additional arguments (unused)
#' @export
coef.LearnerSubspaceElips <- function(
  object,
  ...
) {
  if (is.null(object$result)) {
    stop("Use 'train()' method first")
  }
  if (!is.null(object$task$cat_hps)) {
    A <- lapply(object$result, \(x) x$A)
    b <- lapply(object$result, \(x) x$b)
    used_hps <- lapply(object$result, \(x) x$used_hps)
    dt <- data.table(names(object$result), used_hps, A, b)
    setnames(dt, c(object$task$cat_hps, "hyperparameters", "A", "b"))
    return(dt)
  } else {
    return(data.table(
      hyperparamters = list(object$result$used_hps),
      A = list(object$result$A),
      b = list(object$result$b)
    ))
  }
}
