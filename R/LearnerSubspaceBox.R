#' @include LearnerSubspace.R
#' @title LearnerSubspaceBox class
#' @export
LearnerSubspaceBox <- R6::R6Class(
  "LearnerSubspaceBox",
  inherit = LearnerSubspace,
  private = list(
    #' Fit minimum volume hyperrectangle containing all points
    #' Uses convex optimization to find min/max parameters
    #' @param data Matrix or data.table of hyperparameter values
    #' @param lambda Regularization parameter for slack variables (default = 0)
    #'               When lambda > 0, allows some points outside hyperrectangle
    #'               with penalty. Higher lambda = more tolerance for outliers
    #' @param ... additional parameters like l0, u0: reference lower and
    #'            upper value for slack scaling
    .fit_subspace = function(data, lambda, ...) {
      X <- as.matrix(data) # Convert to n × p matrix
      n <- nrow(X) # number of observations
      p <- ncol(X) # dimension

      if (!is.null(lambda)) {
        l0_abs <- abs(rep(1, p))
        u0_abs <- abs(rep(1, p))

        # Special handling for p = 1
        if (p == 1) {
          # Define optimization variables as scalars
          l <- CVXR::Variable(1) # lower bound
          u <- CVXR::Variable(1) # upper bound
          xi_minus <- CVXR::Variable(n, pos = TRUE) # slack variables for lower bound violations
          xi_plus <- CVXR::Variable(n, pos = TRUE) # slack variables for upper bound violations

          # Define the objective function
          objective <- CVXR::Minimize(
            (lambda / 2) *
              CVXR::power(u - l, 2) +
              (1 / (2 * n)) *
                (CVXR::sum_entries(xi_minus) + CVXR::sum_entries(xi_plus))
          )

          # Define constraints
          constraints <- list()

          # Add constraints for each observation
          for (t in seq_len(n)) {
            constraints <- c(
              constraints,
              l - xi_minus[t] * l0_abs[1] <= X[t, 1],
              X[t, 1] <= u + xi_plus[t] * u0_abs[1]
            )
          }

          # Formulate and solve the problem
          problem <- CVXR::Problem(objective, constraints)
          result <- CVXR::solve(problem, solver = "ECOS")

          # Return results
          return(list(
            coefficients = data.table(
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

        # p > 1
        # Define optimization variables
        l <- CVXR::Variable(p) # lower bounds
        u <- CVXR::Variable(p) # upper bounds
        xi_minus <- CVXR::Variable(n, pos = TRUE) # slack variables for lower bound violations
        xi_plus <- CVXR::Variable(n, pos = TRUE) # slack variables for upper bound violations

        # Define the objective function
        objective <- CVXR::Minimize(
          (lambda / 2) *
            CVXR::sum_squares(u - l) +
            (1 / (2 * n)) *
              (CVXR::sum_entries(xi_minus) + CVXR::sum_entries(xi_plus))
        )

        # Define constraints
        constraints <- list()

        # Add constraints for each observation
        for (t in seq_len(n)) {
          constraints <- c(
            constraints,
            l - xi_minus[t] * l0_abs <= X[t, ],
            X[t, ] <= u + xi_plus[t] * u0_abs
          )
        }

        # Formulate and solve the problem
        problem <- CVXR::Problem(objective, constraints)
        result <- CVXR::solve(problem, solver = "ECOS")

        # Return results
        return(list(
          coefficients = data.table(
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
        # Simple min/max approach (works for any p, including p = 1)
        col_min <- apply(X, 2, min)
        col_max <- apply(X, 2, max)
        return(list(
          coefficients = data.table(
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
