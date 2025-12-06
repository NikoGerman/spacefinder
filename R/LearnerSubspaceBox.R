#' @include LearnerSubspace.R
#' @title Axis-aligned hyperrectangle subspace learner
#' @description
#' Learns axis-aligned hyperrectangles (boxes with edges parallel to coordinate
#' axes) that contain high-quality hyperparameter configurations.
#'
#' @details
#' \strong{Geometry:}
#'
#' Fits a hyperrectangle defined by independent bounds per dimension:
#' \deqn{S = \{x \in \mathbb{R}^p : l_i \leq x_i \leq u_i\}}
#'
#' The transformation matrix \eqn{A = \text{diag}(u - l)} is diagonal, representing
#' independent scaling per dimension.
#'
#' \strong{Optimization (\code{lambda} specified):}
#' \deqn{\min_{l,u,\xi} \frac{\lambda}{2}\|u-l\|_2^2 + \frac{1}{2n}\sum_t(\xi_t^- + \xi_t^+)}
#' subject to: \eqn{l - \xi_t^- \leq x^{(t)} \leq u + \xi_t^+}, \eqn{\xi_t^-, \xi_t^+ \geq 0}
#'
#' Uses ECOS solver. Special handling for univariate case (\eqn{p=1}).
#'
#' \strong{Simple mode (\code{lambda = NULL}):}
#'
#' Uses coordinate-wise min/max: \eqn{l_i = \min_t x_i^{(t)}}, \eqn{u_i = \max_t x_i^{(t)}}
#'
#' \strong{Key Properties:}
#' \itemize{
#'   \item Most interpretable: bounds directly specify valid ranges
#'   \item Fastest: linear algebra with diagonal matrices
#'   \item Best for independent hyperparameters
#'   \item Cannot capture correlations between hyperparameters
#' }
#'
#' \strong{Comparison with other learners:}
#' \itemize{
#'   \item \strong{Box}: Axis-aligned only, fastest, most interpretable
#'   \item \strong{Polygon}: Can rotate, maintains rectangular shape
#'   \item \strong{Ellipsoid}: Can rotate, smooth boundaries, most flexible
#' }
#'
#' @seealso
#' \code{\link{LearnerSubspace}} for inherited methods and general workflow.
#' \code{\link{LearnerSubspacePolygon}} for oriented hyperrectangles.
#' \code{\link{LearnerSubspaceEllipsoid}} for ellipsoids.
#' \code{\link{coef.LearnerSubspaceBox}} for extracting fitted parameters.
#' \code{\link{augment.LearnerSubspaceBox}} for adding density parameters.
#'
#' @examples
#' \dontrun{
#' # Create task and learner
#' task <- SubspaceTask$new(data, target_measure = "accuracy")
#' learner <- LearnerSubspaceBox$new(task)
#'
#' # Simple min/max bounds (no regularization)
#' learner$train(lambda = NULL)
#' coef(learner)
#'
#' # Regularized optimization
#' learner$train(q_val = 0.9, lambda = 0.1)
#' result <- learner$result
#' print(result$coefficients)  # Bounds per hyperparameter
#' print(result$n_violations)  # Number of outliers
#' print(result$outliers)      # Outlier indices
#'
#' # With categorical hyperparameters
#' task <- SubspaceTask$new(data, target_measure = "accuracy",
#'                          cat_hps = "optimizer")
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train(q_val = 0.95, lambda = 0.05)
#' coef(learner)  # Returns separate bounds per optimizer
#'
#' # Get transformation matrices
#' coef(learner, vectorize = TRUE)  # Returns A (diagonal), b
#' }
#'
#' @export
LearnerSubspaceBox <- R6::R6Class(
  "LearnerSubspaceBox",
  inherit = LearnerSubspace,
  private = list(
    .fit_subspace = function(data, lambda, ...) {
      X <- as.matrix(data)
      n <- nrow(X)
      p <- ncol(X)

      if (!is.null(lambda)) {
        l0_abs <- abs(rep(1, p))
        u0_abs <- abs(rep(1, p))

        if (p == 1) {
          l <- CVXR::Variable(1)
          u <- CVXR::Variable(1)
          xi_minus <- CVXR::Variable(n, pos = TRUE)
          xi_plus <- CVXR::Variable(n, pos = TRUE)

          objective <- CVXR::Minimize(
            (lambda / 2) *
              CVXR::power(u - l, 2) +
              (1 / (2 * n)) *
                (CVXR::sum_entries(xi_minus) + CVXR::sum_entries(xi_plus))
          )

          constraints <- list()

          for (t in seq_len(n)) {
            constraints <- c(
              constraints,
              l - xi_minus[t] * l0_abs[1] <= X[t, 1],
              X[t, 1] <= u + xi_plus[t] * u0_abs[1]
            )
          }

          problem <- CVXR::Problem(objective, constraints)
          result <- CVXR::solve(problem, solver = "ECOS")

          return(list(
            coefficients = data.table::data.table(
              hyperparameter = colnames(X),
              min = c(result$getValue(l)),
              max = c(result$getValue(u))
            ),
            used_hps = colnames(X),
            status = result$status,
            objective_value = result$value,
            xi_minus = result$getValue(xi_minus),
            xi_plus = result$getValue(xi_plus),
            n_violations = sum(
              result$getValue(xi_minus) > 1e-5 |
                result$getValue(xi_plus) > 1e-5
            ),
            outliers = which(
              result$getValue(xi_minus) > 1e-5 |
                result$getValue(xi_plus) > 1e-5
            )
          ))
        }

        l <- CVXR::Variable(p)
        u <- CVXR::Variable(p)
        xi_minus <- CVXR::Variable(n, pos = TRUE)
        xi_plus <- CVXR::Variable(n, pos = TRUE)

        objective <- CVXR::Minimize(
          (lambda / 2) *
            CVXR::sum_squares(u - l) +
            (1 / (2 * n)) *
              (CVXR::sum_entries(xi_minus) + CVXR::sum_entries(xi_plus))
        )

        constraints <- list()

        for (t in seq_len(n)) {
          constraints <- c(
            constraints,
            l - xi_minus[t] * l0_abs <= X[t, ],
            X[t, ] <= u + xi_plus[t] * u0_abs
          )
        }

        problem <- CVXR::Problem(objective, constraints)
        result <- CVXR::solve(problem, solver = "ECOS")

        return(list(
          coefficients = data.table::data.table(
            hyperparameter = colnames(X),
            min = c(result$getValue(l)),
            max = c(result$getValue(u))
          ),
          used_hps = colnames(X),
          status = result$status,
          objective_value = result$value,
          xi_minus = result$getValue(xi_minus),
          xi_plus = result$getValue(xi_plus),
          n_violations = sum(
            result$getValue(xi_minus) > 1e-5 |
              result$getValue(xi_plus) > 1e-5
          ),
          outliers = which(
            result$getValue(xi_minus) > 1e-5 |
              result$getValue(xi_plus) > 1e-5
          )
        ))
      } else {
        col_min <- apply(X, 2, min)
        col_max <- apply(X, 2, max)
        return(list(
          coefficients = data.table::data.table(
            hyperparameter = colnames(X),
            min = col_min,
            max = col_max
          ),
          used_hps = colnames(X),
          status = NULL,
          objective_value = NULL,
          xi_minus = NULL,
          xi_plus = NULL,
          n_violations = NULL,
          outliers = NULL
        ))
      }
    }
  )
)
