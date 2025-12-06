#' @include LearnerSubspaceBox.R LearnerSubspacePolygon.R LearnerSubspaceEllipsoid.R
#' @title Summarize fitted subspace learner
#' @description
#' Provides a comprehensive summary of a trained subspace learner, including
#' task information, fitted coefficients, optimization status, and outlier
#' configurations. Prints formatted tables to the console and invisibly returns
#' the summary information.
#'
#' @param object A trained \code{LearnerSubspace} object (or subclass) with fitted
#'   subspace parameters
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns a list with components:
#'   \itemize{
#'     \item \code{status}: \code{data.table} with optimization status, objective
#'       values, number of violations, and observation counts
#'     \item \code{coefficients}: \code{data.table} with fitted subspace parameters
#'       (format depends on learner type)
#'     \item \code{outliers}: \code{data.table} with outlier configurations (empty
#'       if no outliers or \code{lambda = NULL})
#'   }
#'
#' @details
#' \strong{Printed Output:}
#'
#' The function prints three formatted tables:
#'
#' \strong{1. Summary Table:}
#' \itemize{
#'   \item Target measure being optimized
#'   \item Numeric hyperparameters included in subspace
#'   \item Categorical hyperparameters (if any)
#' }
#'
#' \strong{2. Coefficients Table:}
#' \itemize{
#'   \item \strong{Box}: min/max bounds per hyperparameter
#'   \item \strong{Polygon/Ellipsoid}: A matrices and b vectors in list columns
#'   \item Separate rows for each categorical level (if applicable)
#' }
#'
#' \strong{3. Status Table:}
#' \itemize{
#'   \item \code{status}: Solver convergence status (e.g., "optimal", "solved")
#'   \item \code{objective_value}: Final objective function value
#'   \item \code{n_violations}: Number of configurations treated as outliers
#'   \item \code{observations}: Number of top configurations used for fitting
#' }
#'
#' \strong{Status Values:}
#'
#' Fields may be \code{NULL} when:
#' \itemize{
#'   \item \code{lambda = NULL}: Simple min/max fitting (Box learner only)
#'   \item No outliers: All configurations fit within subspace
#' }
#'
#' \strong{Dependencies:}
#'
#' Requires \code{knitr} package for formatted table output. If not available,
#' falls back to basic printing.
#'
#' @seealso
#' \code{\link{LearnerSubspace}} for the base learner class.
#' Methods \code{coef()} and \code{augment()} for extracting fitted parameters.
#' \code{\link{outliers}} for extracting outlier configurations.
#'
#' @examples
#' \dontrun{
#' # Train learner
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"))
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train(q_val = 0.9, lambda = 0.1)
#'
#' # Print summary to console
#' summary(learner)
#'
#' # Capture summary information
#' info <- summary(learner)
#' print(info$status)
#' print(info$coefficients)
#' print(info$outliers)
#'
#' # With categorical hyperparameters
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"),
#'                          cat_hps = "optimizer")
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train(q_val = 0.9, lambda = 0.1)
#' summary(learner)  # Separate status rows per optimizer
#' }
#'
#' @exportS3Method
summary.LearnerSubspace <- function(
  object,
  ...
) {
  check_packages("knitr")

  if (is.null(object$result)) {
    stop(
      "Learner has not been trained. Use train() method first.",
      call. = FALSE
    )
  }

  # Build status table
  if (!is.null(object$task$cat_hps)) {
    levels <- names(object$result)
    observations <- vapply(
      levels,
      function(level) {
        nrow(object$top_configs[get(object$task$cat_hps) == level, ])
      },
      integer(1)
    )
    status <- lapply(object$result, \(x) x$status)
    objective_value <- lapply(object$result, \(x) x$objective_value)
    n_violations <- lapply(object$result, \(x) x$n_violations)

    status_dt <- data.table::data.table(
      level = levels,
      status = status,
      objective_value = objective_value,
      n_violations = n_violations,
      observations = observations
    )
    data.table::setnames(
      status_dt,
      "level",
      object$task$cat_hps
    )
  } else {
    status_dt <- data.table::data.table(
      status = object$result$status,
      objective_value = object$result$objective_value,
      n_violations = object$result$n_violations,
      observations = nrow(object$top_configs)
    )
  }

  # Get coefficients and outliers
  coefs <- stats::coef(object)
  outlier_configs <- suppressMessages(outliers(object))

  # Create task information table
  info_dt <- data.table::data.table(
    Property = c(
      "Target Measure",
      "Numeric Hyperparameters",
      "Categorical Hyperparameters"
    ),
    Value = c(
      object$task$target_measure,
      paste(object$task$hps, collapse = ", "),
      if (!is.null(object$task$cat_hps)) {
        paste(object$task$cat_hps, collapse = ", ")
      } else {
        "None"
      }
    )
  )

  # Print formatted output
  cat("SUMMARY\n")
  cat(strrep("-", 50), "\n")
  cat(knitr::kable(info_dt, format = "simple", align = c("l", "l")), sep = "\n")

  cat("\n\nCOEFFICIENTS\n")
  cat(strrep("-", 50), "\n")
  cat(knitr::kable(coefs, format = "simple"), sep = "\n")

  cat("\n\nSTATUS\n")
  cat(strrep("-", 50), "\n")
  cat(
    knitr::kable(
      status_dt,
      format = "simple",
      align = c("l", "l", "r", "r", "r")
    ),
    sep = "\n"
  )
  cat("\n")

  return(invisible(list(
    status = status_dt,
    coefficients = coefs,
    outliers = outlier_configs
  )))
}
