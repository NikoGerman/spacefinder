#' @include LearnerSubspaceBoxGeneral.R
#' @title coef method for LearnerSubspaceBoxGeneral
#' @param object A LearnerSubspaceBoxGeneral object
#' @param ... Additional arguments (unused)
#' @export
coef.LearnerSubspaceBoxGeneral <- function(
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
    dt <- data.table::data.table(names(object$result), used_hps, A, b)
    data.table::setnames(
      dt,
      c(object$task$cat_hps, "hyperparameters", "A", "b")
    )
    return(dt)
  } else {
    return(data.table::data.table(
      hyperparameters = list(object$result$used_hps),
      A = list(object$result$A),
      b = list(object$result$b)
    ))
  }
}
