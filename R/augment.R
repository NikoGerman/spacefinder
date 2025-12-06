#' @include helper.R
#' @title Augment subspace learner with beta density parameters
#' @description
#' Generic function to augment a trained subspace learner with univariate beta
#' distribution parameters for each hyperparameter dimension. Transforms data
#' from the fitted hyperrectangle subspace to the unit hypercube \eqn{[0,1]^d},
#' filters points within the unit hypercube, and estimates dimension-wise beta
#' distributions via weighted maximum likelihood estimation.
#'
#' @param object A trained subspace learner object (e.g., \code{LearnerSubspaceBox}
#'   or \code{LearnerSubspacePolygon})
#' @param ... Additional arguments passed to the specific method and ultimately
#'   to \code{\link{fit_beta_mle_single}} (e.g., \code{regularize}, \code{tol},
#'   \code{max_iter}, \code{clip_eps})
#'
#' @return A \code{data.table} containing fitted beta distribution parameters with
#'   columns: \code{parameter} (hyperparameter name), \code{alpha} (shape parameter),
#'   \code{beta} (shape parameter), \code{converged} (convergence status),
#'   \code{iterations} (MLE iterations), and optionally \code{cat_hp} (categorical
#'   level) if the task has categorical hyperparameters.
#'
#' @seealso
#' \code{\link{augment.LearnerSubspaceBox}} for axis-aligned hyperrectangles.
#' \code{\link{augment.LearnerSubspacePolygon}} for oriented hyperrectangles.
#' \code{\link{augment_subspace_learner}} for the underlying implementation.
#'
#' @export
augment <- function(object, ...) {
  UseMethod("augment")
}

#' @title Default augment method
#' @description
#' Default method that throws an error for unsupported object types.
#' Augmentation is only implemented for subspace learner objects.
#'
#' @param object An object of unsupported class
#' @param ... Ignored
#'
#' @return This method always throws an error
#' @exportS3Method
augment.default <- function(object, ...) {
  stop(
    "augment() is not implemented for class: ",
    class(object)[1],
    "\nSupported classes: LearnerSubspaceBox, LearnerSubspacePolygon",
    call. = FALSE
  )
}
