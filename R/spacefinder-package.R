#' spacefinder: Subspace Learning for Hyperparameter Optimization
#'
#' @description
#' Learn promising hyperparameter subspaces from benchmark data using
#' geometric methods. The package provides learners for fitting axis-aligned
#' hyperrectangles, oriented hyperrectangles, and ellipsoids to high-performing
#' hyperparameter configurations.
#'
#' @details
#' \strong{Main Components:}
#' \itemize{
#'   \item \code{\link{TaskSubspace}}: Define hyperparameter optimization tasks
#'   \item \code{\link{LearnerSubspaceBox}}: Fit axis-aligned hyperrectangles
#'   \item \code{\link{LearnerSubspacePolygon}}: Fit oriented hyperrectangles
#'   \item \code{\link{LearnerSubspaceEllipsoid}}: Fit ellipsoids
#' }
#'
#' \strong{Key Methods:}
#' \itemize{
#'   \item \code{train()}: Fit subspace to top-performing configurations
#'   \item \code{coef()}: Extract fitted subspace parameters
#'   \item \code{augment()}: Add beta density parameters
#'   \item \code{autoplot()}: Visualize fitted subspaces
#'   \item \code{outliers()}: Extract outlier configurations
#' }
#'
#' @keywords internal
#' @import CVXR
#' @import data.table
#' @importFrom stats coef
#' @importFrom generics augment
#' @importFrom R6 R6Class
#' @importFrom checkmate assertFlag assertDataTable assertChoice assertCharacter assertNumeric
#' @importFrom knitr kable
"_PACKAGE"

# Suppress R CMD check notes for non-standard evaluation
# Used in data.table and ggplot2 NSE contexts
utils::globalVariables(c(
  # data.table NSE
  ".SD",
  ".keep",
  "cat_hp",
  "count",
  "hyperparameter",
  "min",
  "max",
  "q",
  "task",
  "w",
  # ggplot2 NSE (used in autoplot methods)
  "x",
  "y"
))
