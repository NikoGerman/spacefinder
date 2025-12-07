#' @include LearnerSubspace.R
#' @title Ellipsoidal subspace learner
#' @description
#' Learns minimum-volume ellipsoids that contain high-quality hyperparameter
#' configurations. Most flexible geometry with smooth boundaries.
#'
#' @details
#' \strong{Geometry:}
#'
#' Fits an ellipsoid: \eqn{E = \{x \in \mathbb{R}^p : \|Ax + b\|_2 \leq 1\}}
#'
#' Matrix \eqn{A \in \mathbb{R}^{p \times p}} is positive definite. Center at
#' \eqn{c = -A^{-1}b}. Semi-axes determined by eigenvalues of \eqn{A^{-1}}.
#'
#' \strong{Optimization (\code{lambda} specified):}
#' \deqn{\min_{A \succeq 0, b, s} \lambda \cdot (-\log\det(A)) + \frac{1}{n}\sum_{t=1}^n s_t}
#' subject to: \eqn{\|Ax^{(t)} + b\|_2 \leq 1 + s_t}, \eqn{s_t \geq 0}
#'
#' Uses SCS solver for semidefinite programming. Volume minimization via
#' \eqn{-\log\det(A)}. The L2 norm creates smooth ellipsoidal boundaries.
#'
#' \strong{Simple mode (\code{lambda = NULL}):}
#'
#' Minimizes volume without slack variables. All points must satisfy
#' \eqn{\|Ax^{(t)} + b\|_2 \leq 1}.
#'
#' \strong{Key Properties:}
#' \itemize{
#'   \item Most flexible: arbitrary rotations and scaling
#'   \item Smooth boundaries (no corners)
#'   \item Optimal for normally distributed data
#'   \item Most expensive: \eqn{O(p^3)} scaling
#'   \item For \eqn{p > 50}, consider Box or Polygon learners
#' }
#'
#' \strong{Comparison with other learners:}
#' \itemize{
#'   \item \strong{Box}: Axis-aligned, rectangular, fastest
#'   \item \strong{Polygon}: Can rotate, rectangular shape, intermediate cost
#'   \item \strong{Ellipsoid}: Can rotate, smooth boundaries, most flexible, slowest
#' }
#'
#' @note
#' This learner does not implement an \code{augment()} method.
#'
#' @seealso
#' \code{\link{LearnerSubspace}} for inherited methods and general workflow.
#' \code{\link{LearnerSubspaceBox}} for axis-aligned hyperrectangles (faster).
#' \code{\link{LearnerSubspacePolygon}} for oriented hyperrectangles (intermediate).
#' \code{\link{coef.LearnerSubspaceEllipsoid}} for extracting fitted parameters.
#'
#' @examples
#' \dontrun{
#' # Create task and learner
#' task <- SubspaceTask$new(data, target_measure = "accuracy")
#' learner <- LearnerSubspaceEllipsoid$new(task)
#'
#' # Minimum-volume ellipsoid (hard constraints)
#' learner$train(lambda = NULL)
#' result <- learner$result
#' print(result$A)
#' print(result$b)
#'
#' # Regularized optimization (allows outliers)
#' learner$train(q_val = 0.9, lambda = 0.1)
#' result <- learner$result
#' print(result$n_violations)
#' print(result$outliers)
#'
#' # Inspect ellipsoid geometry
#' center <- -solve(result$A) %*% result$b
#' eigendecomp <- eigen(solve(result$A))
#' semi_axes <- sqrt(eigendecomp$values)
#' print(semi_axes)
#'
#' # With categorical hyperparameters
#' task <- SubspaceTask$new(data, target_measure = "accuracy",
#'                          cat_hps = "optimizer")
#' learner <- LearnerSubspaceEllipsoid$new(task)
#' learner$train(q_val = 0.95, lambda = 0.05)
#' coef(learner, vectorize = TRUE)
#' }
#'
#' @export
LearnerSubspaceEllipsoid <- R6::R6Class(
  "LearnerSubspaceEllipsoid",
  inherit = LearnerSubspace,
  private = list(
    .fit_subspace = function(data, lambda) {
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
          constraints <- lapply(seq_len(n), function(t) {
            x_t <- X[t, 1]
            abs(a * x_t + b) <= 1
          })
        }
        problem <- CVXR::Problem(objective, constraints)
        result <- CVXR::solve(problem, solver = "SCS")
        solution <- list(
          A = matrix(result$getValue(a), 1, 1),
          b = result$getValue(b),
          used_hps = colnames(X),
          status = result$status,
          objective_value = result$value
        )
        if (!is.null(lambda)) {
          solution$slack_values = as.vector(result$getValue(s))
          solution$n_violations = sum(solution$slack_values > 1e-5)
          solution$outliers = which(solution$slack_values > 1e-5)
        }
        return(solution)
      } else {
        A <- CVXR::Variable(p, p, PSD = TRUE)
        b <- CVXR::Variable(p)
        if (!is.null(lambda)) {
          s <- CVXR::Variable(n, pos = TRUE)
          objective <- CVXR::Minimize(lambda * -CVXR::log_det(A) + mean(s))
          constraints <- lapply(1:n, function(t) {
            x_t <- X[t, ]
            list(CVXR::norm2(A %*% x_t + b) <= 1 + s[t])
          })
          constraints <- unlist(constraints, recursive = FALSE)
        } else {
          objective <- CVXR::Minimize(-CVXR::log_det(A))
          constraints <- lapply(1:n, function(t) {
            x_t <- X[t, ]
            CVXR::norm2(A %*% x_t + b) <= 1
          })
        }
        problem <- CVXR::Problem(objective, constraints)
        result <- CVXR::solve(problem, solver = "SCS")
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
          solution$outliers = which(solution$slack_values > 1e-5)
        }
        return(solution)
      }
    }
  )
)
