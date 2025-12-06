#' @include LearnerSubspace.R
#' @title Oriented hyperrectangle subspace learner
#' @description
#' Learns oriented (rotated) hyperrectangles that contain high-quality
#' hyperparameter configurations. Allows arbitrary rotation while maintaining
#' rectangular shape.
#'
#' @details
#' \strong{Geometry:}
#'
#' Fits a rotated hyperrectangle defined by:
#' \deqn{\{x \in \mathbb{R}^p : \|Ax + b\|_\infty \leq 1\}}
#'
#' The transformation matrix \eqn{A \in \mathbb{R}^{p \times p}} is positive
#' definite but not restricted to diagonal, allowing rotation in hyperparameter
#' space. The L-infinity norm constraint maintains rectangular shape (sharp corners).
#'
#' \strong{Optimization (\code{lambda} specified):}
#' \deqn{\min_{A \succeq 0, b, s} \lambda \cdot (-\log\det(A)) + \frac{1}{n}\sum_{t=1}^n s_t}
#' subject to: \eqn{\|Ax^{(t)} + b\|_\infty \leq 1 + s_t}, \eqn{s_t \geq 0}
#'
#' Uses SCS solver for semidefinite programming. Volume minimization via
#' \eqn{-\log\det(A)}.
#'
#' \strong{Simple mode (\code{lambda = NULL}):}
#'
#' Minimizes volume without slack variables. All points must satisfy
#' \eqn{\|Ax^{(t)} + b\|_\infty \leq 1}.
#'
#' \strong{Key Properties:}
#' \itemize{
#'   \item Intermediate flexibility: captures correlations between hyperparameters
#'   \item Maintains rectangular shape with sharp corners (unlike ellipsoids)
#'   \item More flexible than Box (can rotate), less flexible than Ellipsoid
#'   \item Computational cost between Box and Ellipsoid
#'   \item Good balance of interpretability and expressiveness
#' }
#'
#' \strong{Comparison with other learners:}
#' \itemize{
#'   \item \strong{Box}: Axis-aligned, diagonal A, fastest, most interpretable
#'   \item \strong{Polygon}: Can rotate, general A, rectangular shape, intermediate cost
#'   \item \strong{Ellipsoid}: Can rotate, general A, smooth boundaries, slowest
#' }
#'
#' @seealso
#' \code{\link{LearnerSubspace}} for inherited methods and general workflow.
#' \code{\link{LearnerSubspaceBox}} for axis-aligned hyperrectangles (faster).
#' \code{\link{LearnerSubspaceEllipsoid}} for ellipsoids (more flexible).
#' \code{\link{coef.LearnerSubspacePolygon}} for extracting fitted parameters.
#' \code{\link{augment.LearnerSubspacePolygon}} for adding density parameters.
#'
#' @examples
#' \dontrun{
#' # Create task and learner
#' task <- SubspaceTask$new(data, target_measure = "accuracy")
#' learner <- LearnerSubspacePolygon$new(task)
#'
#' # Minimum-volume oriented box (hard constraints)
#' learner$train(lambda = NULL)
#' result <- learner$result
#' print(result$A)  # Shape matrix (not diagonal)
#' print(result$b)  # Translation vector
#'
#' # Regularized optimization (allows outliers)
#' learner$train(q_val = 0.9, lambda = 0.1)
#' result <- learner$result
#' print(result$n_violations)  # Number of outliers
#' print(result$outliers)      # Outlier indices
#'
#' # Check orientation
#' eigendecomp <- eigen(result$A)
#' print(eigendecomp$vectors)  # Principal directions
#' print(1 / eigendecomp$values)  # Box widths along principal axes
#'
#' # With categorical hyperparameters
#' task <- SubspaceTask$new(data, target_measure = "accuracy",
#'                          cat_hps = "optimizer")
#' learner <- LearnerSubspacePolygon$new(task)
#' learner$train(q_val = 0.95, lambda = 0.05)
#' coef(learner, vectorize = TRUE)  # Separate A, b per optimizer
#' }
#'
#' @export
LearnerSubspacePolygon <- R6::R6Class(
  "LearnerSubspacePolygon",
  inherit = LearnerSubspace,
  private = list(
    .fit_subspace = function(data, lambda = NULL) {
      X <- as.matrix(data)
      n <- nrow(X)
      p <- ncol(X)

      if (p == 1) {
        a <- CVXR::Variable(1, pos = TRUE)
        b <- CVXR::Variable(1)
        if (!is.null(lambda)) {
          s <- CVXR::Variable(n, pos = TRUE)
          objective <- CVXR::Minimize(lambda * -log(a) + mean(s))
          constraints <- lapply(1:n, function(t) {
            x_t <- X[t, 1]
            list(abs(a * x_t + b) <= 1 + s[t])
          })
          constraints <- unlist(constraints, recursive = FALSE)
        } else {
          objective <- CVXR::Minimize(-log(a))
          constraints <- lapply(1:n, function(t) {
            x_t <- X[t, 1]
            abs(a * x_t + b) <= 1
          })
        }
        problem <- CVXR::Problem(objective, constraints)
        result <- suppressMessages(
          CVXR::solve(problem, solver = "SCS", verbose = FALSE)
        )
        solution <- list(
          A = matrix(result$getValue(a), 1, 1),
          b = result$getValue(b),
          used_hps = colnames(X),
          status = result$status,
          objective_value = result$value
        )
        if (!is.null(lambda)) {
          solution$slack_values <- as.vector(result$getValue(s))
          solution$n_violations <- sum(solution$slack_values > 1e-5)
          solution$outliers <- which(solution$slack_values > 1e-5)
        }
        return(solution)
      }

      A <- CVXR::Variable(p, p, PSD = TRUE)
      b <- CVXR::Variable(p)
      if (!is.null(lambda)) {
        s <- CVXR::Variable(n, pos = TRUE)
        objective <- CVXR::Minimize(
          lambda * -CVXR::log_det(A) + mean(s)
        )
        constraints <- lapply(1:n, function(t) {
          x_t <- X[t, ]
          CVXR::norm_inf(A %*% x_t + b) <= 1 + s[t]
        })
      } else {
        objective <- CVXR::Minimize(-CVXR::log_det(A))
        constraints <- lapply(1:n, function(t) {
          x_t <- X[t, ]
          CVXR::norm_inf(A %*% x_t + b) <= 1
        })
      }
      problem <- CVXR::Problem(objective, constraints)
      result <- suppressMessages(
        CVXR::solve(problem, solver = "SCS", verbose = FALSE)
      )

      solution <- list(
        A = result$getValue(A),
        b = result$getValue(b),
        used_hps = colnames(X),
        status = result$status,
        objective_value = result$value
      )
      if (!is.null(lambda)) {
        solution$slack_values <- as.vector(result$getValue(s))
        solution$n_violations <- sum(solution$slack_values > 1e-5)
        solution$outliers <- which(solution$slack_values > 1e-5)
      }
      return(solution)
    }
  )
)
