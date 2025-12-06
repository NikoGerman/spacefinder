#' @include LearnerSubspacePolygon.R helper.R augment.R
#' @title Augment oriented hyperrectangle learner with beta densities
#' @description
#' Transforms data from the fitted oriented (rotated) hyperrectangle subspace to the
#' unit hypercube and fits univariate beta distributions to each hyperparameter dimension
#' using weighted maximum likelihood estimation. Uses Cholesky decomposition to invert
#' the transformation matrix A, which is a general positive definite matrix allowing
#' the hyperrectangle to be arbitrarily oriented in the hyperparameter space.
#'
#' @inheritParams augment_subspace_learner
#' @param object A \code{LearnerSubspacePolygon} object with fitted subspace parameters
#'
#' @return A \code{data.table} with fitted beta distribution parameters for each
#'   hyperparameter dimension. See \code{\link{augment_subspace_learner}} for details.
#'
#' @seealso
#' \code{\link{augment_subspace_learner}} for implementation details and return value structure.
#' \code{\link{augment.LearnerSubspaceBox}} for the axis-aligned hyperrectangle variant.
#'
#' @exportS3Method
augment.LearnerSubspacePolygon <- function(object, regularize = TRUE, ...) {
  augment_subspace_learner(object, regularize, ...)
}
