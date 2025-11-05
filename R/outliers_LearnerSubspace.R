#' @include LearnerSubspaceBox.R LearnerSubspaceElips.R
#' @title outliers method for learners inheriting from LearnerSubspace
#' @param object An object inheriting from LearnerSubspace
#' @param ... Additional arguments (unused)
#' @export
outliers <- function(object, ...) {
  stopifnot(inherits(object, "LearnerSubspace"))
  if (is.null(object$result)) {
    stop("Use 'train()' method first")
  } else {
    if (!is.null(object$task$cat_hps)) {
      outliers <- list()
      levels <- names(object$result)
      for (level in levels) {
        data <- object$top_configs[get(object$task$cat_hps) == level, ]
        outliers[[length(outliers) + 1]] <- data[
          object$result[[level]]$outliers,
        ]
      }
      if (all(vapply(outliers, \(x) nrow(x) == 0, logical(1)))) {
        message("no outliers!\n")
      }
      return(data.table::rbindlist(outliers))
    } else {
      if (is.null(object$result$outliers)) {
        message("no outliers!\n")
      }
      return(object$top_configs[object$result$outliers, ])
    }
  }
}
