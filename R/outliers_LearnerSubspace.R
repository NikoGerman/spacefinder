#' @include LearnerSubspaceBox.R LearnerSubspacePolygon.R LearnerSubspaceEllipsoid.R
#' @title Extract outlier configurations
#' @description
#' Extracts hyperparameter configurations identified as outliers during subspace
#' fitting. Outliers are configurations that violated the subspace constraints and
#' were excluded via slack variables during regularized optimization.
#'
#' @param object A trained \code{LearnerSubspace} object (or subclass) with fitted
#'   subspace parameters
#' @param ... Additional arguments (currently unused)
#'
#' @return A \code{data.table} containing the outlier configurations with all
#'   columns from the original task data. Returns an empty \code{data.table} if
#'   no outliers were identified. A message is printed when no outliers exist.
#'
#' @details
#' \strong{When Outliers Exist:}
#'
#' Outliers are only identified when training with \code{lambda > 0}. The
#' regularization parameter allows the optimization to exclude some configurations
#' from the fitted subspace by introducing slack variables.
#'
#' For a configuration to be considered an outlier, its slack variable must exceed
#' the threshold of \eqn{10^{-5}}.
#'
#' When \code{lambda = NULL}, all configurations are forced to fit within the
#' subspace (hard constraints), so no outliers exist.
#'
#' \strong{Categorical Hyperparameters:}
#'
#' When the task includes categorical hyperparameters, outliers are identified
#' separately for each categorical level and combined in the returned \code{data.table}.
#'
#' \strong{Interpretation:}
#'
#' Outliers typically represent:
#' \itemize{
#'   \item Configurations in sparse regions of hyperparameter space
#'   \item Anomalous configurations with unusual performance
#'   \item Configurations that don't fit the dominant subspace pattern
#' }
#'
#' The number and characteristics of outliers can guide decisions about:
#' \itemize{
#'   \item Adjusting the \code{lambda} parameter
#'   \item Investigating unusual configurations
#'   \item Understanding the geometry of high-performing regions
#' }
#'
#' @seealso
#' \code{\link{LearnerSubspace}} for the base learner class.
#' \code{\link{LearnerSubspaceBox}} for axis-aligned hyperrectangles.
#' \code{\link{LearnerSubspacePolygon}} for oriented hyperrectangles.
#' \code{\link{LearnerSubspaceEllipsoid}} for ellipsoids.
#'
#' @examples
#' \dontrun{
#' # Train with regularization (allows outliers)
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"))
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train(q_val = 0.9, lambda = 0.1)
#'
#' # Extract outliers
#' outlier_configs <- outliers(learner)
#' print(nrow(outlier_configs))  # Number of outliers
#' print(outlier_configs)  # View outlier configurations
#'
#' # Check outlier information from result
#' print(learner$result$n_violations)  # Total number of outliers
#' print(learner$result$outliers)  # Outlier indices
#'
#' # Train without regularization (no outliers)
#' learner$train(q_val = 0.9, lambda = NULL)
#' outlier_configs <- outliers(learner)  # Returns empty data.table
#'
#' # With categorical hyperparameters
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"),
#'                          cat_hps = "optimizer")
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train(q_val = 0.9, lambda = 0.1)
#' outlier_configs <- outliers(learner)  # Combined across all levels
#' }
#'
#' @export
outliers <- function(object, ...) {
  stopifnot(inherits(object, "LearnerSubspace"))

  if (is.null(object$result)) {
    stop(
      "Learner has not been trained. Use train() method first.",
      call. = FALSE
    )
  }

  if (!is.null(object$task$cat_hps)) {
    outliers_list <- list()
    levels <- names(object$result)
    for (level in levels) {
      outlier_indices <- object$result[[level]]$outliers
      if (!is.null(outlier_indices) && length(outlier_indices) > 0) {
        data <- object$top_configs[get(object$task$cat_hps) == level, ]
        outliers_list[[length(outliers_list) + 1]] <- data[outlier_indices, ]
      }
    }

    if (length(outliers_list) == 0) {
      message(
        "No outliers found. This occurs when lambda = NULL or all configurations fit within the subspace."
      )
      return(data.table::data.table())
    }

    return(data.table::rbindlist(outliers_list))
  } else {
    outlier_indices <- object$result$outliers

    if (is.null(outlier_indices) || length(outlier_indices) == 0) {
      message(
        "No outliers found. This occurs when lambda = NULL or all configurations fit within the subspace."
      )
      return(data.table::data.table())
    }

    return(object$top_configs[outlier_indices, ])
  }
}
