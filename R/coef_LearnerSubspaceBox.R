#' @include LearnerSubspaceBox.R
#' @title coef method for LearnerSubspaceBox
#' @param object A LearnerSubspaceBox object
#' @param ... Additional arguments (unused)
#' @export
coef.LearnerSubspaceBox <- function(
  object,
  ...
) {
  if (is.null(object$result)) {
    stop("Use 'train()' method first")
  }
  if (!is.null(object$task$cat_hps)) {
    return(data.table::rbindlist(
      lapply(object$result, \(x) x$coefficients),
      idcol = object$task$cat_hps
    ))
  } else {
    return(object$result$coefficients)
  }
}
