#' @include LearnerSubspaceBox.R helper.R augment.R
#' @title Augment axis-aligned box learner with beta densities
#' @description
#' Transforms data from the fitted axis-aligned hyperrectangle subspace to the unit
#' hypercube and fits univariate beta distributions to each hyperparameter dimension
#' using weighted maximum likelihood estimation. Uses diagonal matrix inversion since
#' the box transformation matrix A is diagonal (independent scaling per dimension).
#'
#' @inheritParams augment_subspace_learner
#' @param object A \code{LearnerSubspaceBox} object with fitted subspace parameters
#'
#' @return A \code{data.table} with fitted beta distribution parameters for each
#'   hyperparameter dimension. See \code{\link{augment_subspace_learner}} for details.
#'
#' @seealso
#' \code{\link{augment_subspace_learner}} for implementation details and return value structure.
#' \code{\link{augment.LearnerSubspacePolygon}} for the general hyperrectangle variant.
#'
#' @exportS3Method
augment.LearnerSubspaceBox <- function(object, regularize = TRUE, ...) {
  augment_subspace_learner(
    object,
    regularize,
    invert_fn = \(A) diag(1 / diag(A)),
    ...
  )
}
