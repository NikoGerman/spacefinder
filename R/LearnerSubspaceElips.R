#' @include LearnerSubspace.R
#' @title LearnerSubspaceElips class
#' @export
LearnerSubspaceElips <- R6::R6Class(
  "LearnerSubspaceElips",
  inherit = LearnerSubspace,

  private = list(
    #' Fit minimum volume ellipsoid containing all points
    #' Uses convex optimization to find ellipsoid parameters
    #' @param data Matrix or data.table of hyperparameter values
    #' @param lambda Regularization parameter for slack variables (default = 0)
    #'               When lambda > 0, allows some points outside ellipsoid
    #'               with penalty. Higher lambda = more tolerance for outliers
    .fit_subspace = function(data, lambda) {
      X <- as.matrix(data) # Convert to n × p matrix
      n <- nrow(X)
      p <- ncol(X)

      # Special handling for p = 1
      if (p == 1) {
        a <- CVXR::Variable(1, pos = TRUE) # Scalar, must be positive
        b <- CVXR::Variable(1)

        if (!is.null(lambda)) {
          # 1D case with slack variables
          s <- CVXR::Variable(n, pos = TRUE)

          objective <- CVXR::Minimize(
            lambda * -log(a) + mean(s)
          )

          constraints <- lapply(1:n, function(t) {
            x_t <- X[t, 1]
            list(
              abs(a * x_t + b) <= 1 + s[t],
              s[t] >= 0
            )
          })
          constraints <- unlist(constraints, recursive = FALSE)
        } else {
          # 1D case without slack variables
          objective <- CVXR::Minimize(-log(a))

          constraints <- lapply(seq_len(n), function(t) {
            x_t <- X[t, 1]
            abs(a * x_t + b) <= 1
          })
        }

        problem <- CVXR::Problem(objective, constraints)
        result <- CVXR::solve(problem, solver = "SCS")

        # Return as 1x1 matrix for consistency
        solution <- list(
          A = matrix(result$getValue(a), 1, 1),
          b = result$getValue(b),
          used_hps = colnames(X),
          status = result$status,
          objective_value = result$value
        )

        # Add slack info if lambda was provided
        if (!is.null(lambda)) {
          solution$slack_values = as.vector(result$getValue(s))
          solution$n_violations = sum(solution$slack_values > 1e-5)
          solution$outliers = which(solution$slack_values > 1e-5)
        }

        return(solution)
      } else {
        # Define optimization variables
        A <- CVXR::Variable(p, p, PSD = TRUE)
        b <- CVXR::Variable(p)

        if (!is.null(lambda)) {
          # With slack variables for robustness
          # Allow some points to violate the constraint with penalty
          s <- CVXR::Variable(n, pos = TRUE) # Slack variables, one per point

          # Objective: minimize volume + penalty for constraint violations
          # -log(det(A)) minimizes volume, lambda * sum(s) penalizes slack
          objective <- CVXR::Minimize(
            lambda * -CVXR::log_det(A) + mean(s)
          )

          # Constraints: ||A * x_t + b||_2 <= 1 + s_t for all t
          #              s_t >= 0 (slack must be non-negative)
          constraints <- lapply(1:n, function(t) {
            x_t <- X[t, ]
            list(
              CVXR::norm2(A %*% x_t + b) <= 1 + s[t],
              s[t] >= 0
            )
          })
          constraints <- unlist(constraints, recursive = FALSE)
        } else {
          # Formulation without slack variables
          # Objective: minimize volume = minimize log(det(A^{-1})) = -log(det(A))
          objective <- CVXR::Minimize(-CVXR::log_det(A))

          # Constraints: all points must be inside ellipsoid
          # ||A * x_t + b||_2 <= 1 for all t
          constraints <- lapply(1:n, function(t) {
            x_t <- X[t, ]
            CVXR::norm2(A %*% x_t + b) <= 1
          })
        }

        # Solve optimization problem
        problem <- CVXR::Problem(objective, constraints)
        result <- CVXR::solve(problem, solver = "SCS")

        # Extract solution
        solution <- list(
          A = result$getValue(A),
          b = result$getValue(b),
          used_hps = colnames(X),
          status = result$status,
          objective_value = result$value
        )

        # If using slack variables, also return them
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
