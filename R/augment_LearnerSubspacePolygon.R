#' @include LearnerSubspacePolygon.R helper.R
#' @title Augment oriented hyperrectangle learner with beta densities
#' @description
#' Transforms data from the fitted oriented (rotated) hyperrectangle subspace to the
#' unit hypercube and fits univariate beta distributions to each hyperparameter dimension
#' using weighted maximum likelihood estimation. The polygon learner uses the L-infinity
#' norm, so the fitted subspace is \eqn{\|Ax + b\|_\infty \leq 1}, which maps to
#' \eqn{[-1, 1]^d} before being rescaled to \eqn{[0, 1]^d} for beta fitting.
#'
#' @param x A \code{LearnerSubspacePolygon} object with fitted subspace parameters
#' @param regularize Logical indicating whether to enforce \code{alpha >= 1} and
#'   \code{beta >= 1} to avoid U-shaped densities. Default: \code{TRUE}
#' @param ... Additional arguments passed to \code{\link{fit_beta_mle_single}}
#'   (e.g., \code{tol}, \code{max_iter}, \code{clip_eps})
#'
#' @return A \code{data.table} with columns:
#'   \itemize{
#'     \item \code{parameter}: Hyperparameter name
#'     \item \code{alpha}: Fitted beta shape parameter (alpha > 0)
#'     \item \code{beta}: Fitted beta shape parameter (beta > 0)
#'     \item \code{converged}: Logical indicating whether MLE converged
#'     \item \code{iterations}: Number of Newton-Raphson iterations used
#'     \item \code{cat_hp}: Categorical level (only if task has categorical hyperparameters)
#'   }
#'
#' @details
#' \strong{Algorithm:}
#'
#' For each categorical level (or globally if no categorical hyperparameters):
#' \enumerate{
#'   \item Transform data: \eqn{z = A^{-1}(y - b)} to map to \eqn{[-1, 1]^d}
#'   \item Rescale to unit cube: \eqn{x = (z + 1) / 2} to map to \eqn{[0, 1]^d}
#'   \item Filter points where all coordinates lie in \eqn{[0, 1]^d}
#'   \item Fit \eqn{Beta(\alpha, \beta)} to each dimension via weighted MLE
#'   \item Apply regularization if requested: \eqn{\alpha, \beta \geq 1}
#' }
#'
#' The key difference from the Box learner is that oriented hyperrectangles use the
#' L-infinity norm \eqn{\|z\|_\infty \leq 1}, which defines a hypercube in the
#' transformed space \eqn{[-1, 1]^d}. We rescale this to \eqn{[0, 1]^d} before
#' fitting beta distributions.
#'
#' \strong{Matrix Inversion:}
#'
#' Since \eqn{A} is a general positive definite matrix (not diagonal), uses Cholesky
#' decomposition for efficient and stable inversion: \eqn{A^{-1} = (R^T R)^{-1} = R^{-1}R^{-T}}
#' where \eqn{R} is the Cholesky factor.
#'
#' \strong{Weights:}
#'
#' Data points are weighted by their performance (target measure values), normalized
#' to sum to 1. This gives more influence to higher-performing configurations.
#'
#' \strong{Regularization:}
#'
#' When \code{regularize = TRUE} (default), ensures \eqn{\alpha \geq 1} and
#' \eqn{\beta \geq 1}. This prevents U-shaped densities and ensures the mode exists
#' in the interior of \eqn{[0,1]}.
#'
#' \strong{Categorical Hyperparameters:}
#'
#' When the task includes categorical hyperparameters, separate beta distributions
#' are fitted for each combination of hyperparameter and categorical level.
#'
#' @seealso
#' \code{\link{LearnerSubspacePolygon}} for the learner class.
#' \code{\link{fit_beta_mle_single}} for the univariate beta MLE algorithm.
#' \code{\link{augment.LearnerSubspaceBox}} for the axis-aligned variant.
#'
#' @examples
#' \dontrun{
#' # Train learner
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"))
#' learner <- LearnerSubspacePolygon$new(task)
#' learner$train(q_val = 0.9, lambda = 0.1)
#'
#' # Fit beta densities
#' densities <- augment(learner)
#' print(densities)
#'
#' # Without regularization
#' densities_unreg <- augment(learner, regularize = FALSE)
#' }
#'
#' @exportS3Method
augment.LearnerSubspacePolygon <- function(x, regularize = TRUE, ...) {
  if (is.null(x$result)) {
    stop(
      "Learner has not been trained. Use train() method first.",
      call. = FALSE
    )
  }

  data <- x$task$data
  coefs <- coef(x)
  cat_hps <- x$task$cat_hps

  # Helper function to fit beta densities for one level
  fit_level <- function(level_data, hps, A, b, w) {
    # Transform to [-1,1]^d: z = Ay + b
    y <- as.matrix(level_data[, mget(hps)])
    z <- sweep(t(A %*% t(y)), 2, b, FUN = `+`)
    # Rescale from [-1, 1] to [0, 1]: z = (z + 1) / 2
    z <- (z + 1) / 2

    # Create data.table with transformed coordinates
    DT <- data.table::as.data.table(z)
    data.table::setnames(DT, hps)
    DT[, w := w]

    # Filter valid points (within [0,1]^d)
    DT[, .keep := rowSums(.SD < 0 | .SD > 1) == 0, .SDcols = hps]
    DT <- DT[.keep == TRUE][, .keep := NULL]

    # Handle case with no valid points
    if (nrow(DT) == 0) {
      warning(
        "No valid points in unit cube, returning uniform prior Beta(1,1)",
        call. = FALSE
      )
      return(data.table::data.table(
        parameter = hps,
        alpha = 1.0,
        beta = 1.0,
        converged = TRUE,
        iterations = 0L
      ))
    }

    # normalize weights over the valid points only
    DT[, w := w / sum(w)]

    # Fit beta MLE for each hyperparameter
    data.table::rbindlist(lapply(hps, function(hp) {
      fit <- fit_beta_mle_single(DT[[hp]], DT$w, ...)

      # Apply regularization
      if (regularize) {
        fit$alpha <- max(1.0, fit$alpha)
        fit$beta <- max(1.0, fit$beta)
      }

      data.table::data.table(
        parameter = hp,
        alpha = fit$alpha,
        beta = fit$beta,
        converged = fit$converged,
        iterations = fit$iterations
      )
    }))
  }

  # Process by categorical level if present
  if (!is.null(cat_hps)) {
    data.table::rbindlist(lapply(unique(coefs[, get(cat_hps)]), function(lvl) {
      coef_row <- coefs[get(cat_hps) == lvl]
      fit_level(
        level_data = data[get(cat_hps) == lvl],
        hps = coef_row$hyperparameters[[1]],
        A = coef_row$A[[1]],
        b = coef_row$b[[1]],
        w = data[get(cat_hps) == lvl, get(x$task$target_measure)]
      )[, (cat_hps) := lvl]
    }))
  } else {
    fit_level(
      level_data = data,
      hps = coefs$hyperparameters[[1]],
      A = coefs$A[[1]],
      b = coefs$b[[1]],
      w = data[[x$task$target_measure]]
    )
  }
}
