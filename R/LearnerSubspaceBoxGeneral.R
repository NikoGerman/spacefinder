#' @include LearnerSubspace.R
#' @title LearnerSubspaceOrientedBox class
#' @export
LearnerSubspaceBoxGeneral <- R6::R6Class(
  "LearnerSubspaceBoxGeneral",
  inherit = LearnerSubspace,
  private = list(
    .fit_subspace = function(data, lambda = NULL) {
      X <- as.matrix(data)
      n <- nrow(X)
      p <- ncol(X)

      # Special handling for p = 1
      if (p == 1) {
        a <- CVXR::Variable(1, pos = TRUE)
        b <- CVXR::Variable(1)

        if (!is.null(lambda)) {
          s <- CVXR::Variable(n, pos = TRUE)
          objective <- CVXR::Minimize(lambda * -log(a) + mean(s))
          constraints <- lapply(1:n, function(t) {
            x_t <- X[t, 1]
            list(
              abs(a * x_t + b) <= 1 + s[t]
            )
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

      # p > 1: Define optimization variables
      A <- CVXR::Variable(p, p, PSD = TRUE)
      b <- CVXR::Variable(p)

      if (!is.null(lambda)) {
        # With slack variables
        s <- CVXR::Variable(n, pos = TRUE)

        objective <- CVXR::Minimize(
          lambda * -CVXR::log_det(A) + mean(s)
        )

        constraints <- lapply(1:n, function(t) {
          x_t <- X[t, ]
          CVXR::norm_inf(A %*% x_t + b) <= 1 + s[t]
        })
      } else {
        # Without slack variables
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

      # Extract solution
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
