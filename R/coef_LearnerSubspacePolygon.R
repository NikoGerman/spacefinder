#' @include LearnerSubspacePolygon.R
#' @title Extract coefficients from fitted oriented hyperrectangle learner
#' @description
#' Extracts fitted subspace parameters from a trained \code{LearnerSubspacePolygon}
#' object. Returns the transformation matrix \eqn{A} and translation vector \eqn{b}
#' that define the fitted oriented hyperrectangle.
#'
#' @param object A \code{LearnerSubspacePolygon} object with fitted subspace parameters
#' @param ... Additional arguments (currently unused)
#'
#' @return A \code{data.table} with columns:
#'   \itemize{
#'     \item \code{hyperparameters}: List column containing hyperparameter names
#'     \item \code{A}: List column of positive definite matrices defining
#'       hyperrectangle shape and orientation
#'     \item \code{b}: List column of translation vectors
#'     \item \code{cat_hp}: Categorical level (only if task has categorical hyperparameters)
#'   }
#'
#' @details
#' The oriented hyperrectangle is defined by \eqn{\|Ax + b\|_\infty \leq 1} where:
#' \itemize{
#'   \item \eqn{A \in \mathbb{R}^{p \times p}} is a positive definite matrix
#'   \item \eqn{b \in \mathbb{R}^p} is the translation vector
#'   \item \eqn{x} are points inside the hyperrectangle in original coordinates
#'   \item The L-infinity norm maintains rectangular shape with sharp corners
#' }
#'
#' The center of the hyperrectangle is: \eqn{c = -A^{-1}b}
#'
#' Unlike \code{LearnerSubspaceBox} where \eqn{A} is diagonal (axis-aligned),
#' here \eqn{A} is a general positive definite matrix allowing arbitrary rotation
#' while maintaining the rectangular structure.
#'
#' The eigendecomposition of \eqn{A} reveals the orientation: if \eqn{A = V\Lambda V^T},
#' then \eqn{V} gives the principal directions and \eqn{\Lambda} the scaling along
#' those directions.
#'
#' When the task includes categorical hyperparameters, separate coefficient sets
#' are returned for each categorical level, identified by the \code{cat_hp} column.
#'
#' @section Error Handling:
#' Throws an error if the learner has not been trained. Call \code{train()} before
#' extracting coefficients.
#'
#' @seealso
#' \code{\link{LearnerSubspacePolygon}} for the learner class.
#' \code{\link{coef.LearnerSubspaceBox}} for the axis-aligned variant.
#' \code{\link{coef.LearnerSubspaceEllipsoid}} for the ellipsoid variant.
#' \code{\link{augment.LearnerSubspacePolygon}} for adding density parameters.
#'
#' @examples
#' \dontrun{
#' # Train learner
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"))
#' learner <- LearnerSubspacePolygon$new(task)
#' learner$train(q_val = 0.9, lambda = 0.1)
#'
#' # Extract coefficients
#' coefs <- coef(learner)
#' print(coefs$A[[1]])  # Shape matrix (not diagonal)
#' print(coefs$b[[1]])  # Translation vector
#'
#' # Compute hyperrectangle center
#' A <- coefs$A[[1]]
#' b <- coefs$b[[1]]
#' center <- -solve(A) %*% b
#'
#' # Analyze orientation
#' eigen_decomp <- eigen(A)
#' principal_directions <- eigen_decomp$vectors
#' scalings <- eigen_decomp$values
#' }
#'
#' @exportS3Method
coef.LearnerSubspacePolygon <- function(
  object,
  ...
) {
  if (is.null(object$result)) {
    stop(
      "Learner has not been trained. Use train() method first.",
      call. = FALSE
    )
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
