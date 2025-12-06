#' @keywords internal
#' @title Check for required package dependencies
#' @description
#' Verifies that required packages are installed and can be loaded.
#' Throws an error with a helpful message listing any missing packages.
#'
#' @param pkgs `character` vector of package names to check
#'
#' @return Invisible `NULL` if all packages are available. Otherwise throws an error.
#'
#' @details
#' Uses \code{requireNamespace()} to check package availability without loading them.
#' This is preferred over \code{require()} for package dependency checks as it
#' doesn't attach packages to the search path.
#'
#' @examples
#' \dontrun{
#' # Check single package
#' check_packages("data.table")
#'
#' # Check multiple packages
#' check_packages(c("ggplot2", "dplyr"))
#' }
check_packages <- function(pkgs) {
  missing <- vapply(
    pkgs,
    \(pkg) !requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )
  if (any(missing)) {
    stop(
      sprintf(
        "Package(s) %s needed for this function. Please install it/them.",
        paste0(sprintf("'%s'", pkgs[missing]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

#' @keywords internal
#' @title Resolve hyperparameter selection
#' @description
#' Validates and resolves which hyperparameters to use based on the selection
#' argument and those present in the task. Handles both explicit selection and
#' the special "all" keyword.
#'
#' @param object A learner object containing a task with hyperparameters
#' @param select `character` vector of hyperparameter names to select, or the
#'   string "all" to select all available hyperparameters in the task
#'
#' @return `character` vector of validated hyperparameter names that exist in
#'   the task and were requested via \code{select}
#'
#' @details
#' When \code{select = "all"}, returns all hyperparameters in the task.
#' Otherwise, returns the intersection of requested hyperparameters and those
#' present in the task. Throws an error if none of the selected hyperparameters
#' exist in the task.
#'
#' @examples
#' \dontrun{
#' # Select all hyperparameters
#' resolve_selected(learner, "all")
#'
#' # Select specific hyperparameters
#' resolve_selected(learner, c("learning_rate", "max_depth"))
#' }
resolve_selected <- function(object, select) {
  present_hps <- object$task$hps
  if (select == "all") {
    return(present_hps)
  }
  selected <- base::intersect(present_hps, select)
  if (length(selected) == 0) {
    stop(
      "None of the selected hyperparameters are present in the task.\n",
      "  Requested: ",
      paste0(select, collapse = ", "),
      "\n",
      "  Available: ",
      paste0(present_hps, collapse = ", "),
      call. = FALSE
    )
  }
  return(selected)
}

#' @keywords internal
#' @title Fit univariate beta distribution via weighted MLE
#' @description
#' Fits a Beta(alpha, beta) distribution to univariate data using weighted
#' maximum likelihood estimation. Initializes parameters via weighted moment
#' matching, then refines estimates using Newton-Raphson optimization.
#'
#' @param x `numeric` vector of observations in the interval (0, 1)
#' @param w `numeric` vector of normalized weights that sum to 1. Must have
#'   the same length as \code{x}
#' @param tol `numeric` convergence tolerance for gradient norm (default: 1e-6)
#' @param max_iter `integer` maximum number of Newton-Raphson iterations (default: 100)
#' @param clip_eps `numeric` clipping epsilon for numerical stability. Values
#'   outside \code{[clip_eps, 1 - clip_eps]} are clipped. Default: same as \code{tol}
#'
#' @return A `list` with components:
#'   \item{alpha}{fitted shape parameter (> 0)}
#'   \item{beta}{fitted shape parameter (> 0)}
#'   \item{converged}{logical indicating whether optimization converged}
#'   \item{iterations}{number of Newton-Raphson iterations performed}
#'
#' @details
#' The function optimizes the weighted log-likelihood:
#' \deqn{\sum_i w_i \log f(x_i; \alpha, \beta)}
#' where \eqn{f(x; \alpha, \beta)} is the beta density.
#'
#' \strong{Algorithm:}
#' \enumerate{
#'   \item Clip \code{x} to \code{[clip_eps, 1 - clip_eps]} for numerical stability
#'   \item Initialize via weighted method of moments
#'   \item Iteratively update parameters using Newton-Raphson with backtracking line search
#'   \item Convergence declared when gradient norm < \code{tol}
#' }
#'
#' \strong{Numerical considerations:}
#' \itemize{
#'   \item Returns alpha = beta = 100 for degenerate cases (variance < 1e-10)
#'   \item Uses backtracking line search to maintain positivity of parameters
#'   \item Warns if Hessian becomes singular (determinant < 1e-10)
#'   \item Warns if maximum iterations reached without convergence
#' }
#'
#' @examples
#' \dontrun{
#' # Unweighted fit
#' x <- rbeta(100, 2, 5)
#' w <- rep(1/100, 100)
#' fit <- fit_beta_mle_single(x, w)
#'
#' # Weighted fit
#' x <- rbeta(100, 3, 3)
#' w <- runif(100)
#' w <- w / sum(w)
#' fit <- fit_beta_mle_single(x, w, tol = 1e-8, max_iter = 200)
#' }
fit_beta_mle_single <- function(
  x,
  w,
  tol = 1e-6,
  max_iter = 100,
  clip_eps = NULL
) {
  # Use tol as default for clip_eps if not specified
  if (is.null(clip_eps)) {
    clip_eps <- tol
  }

  # Remove any values outside (0,1) and clip to avoid log issues
  x <- pmax(clip_eps, pmin(1 - clip_eps, x))

  # Sufficient statistics
  log_x_bar <- sum(w * log(x))
  log_1mx_bar <- sum(w * log(1 - x))

  # Initialize with weighted method of moments
  xbar <- sum(w * x)
  s2 <- sum(w * (x - xbar)^2)

  # Check for degenerate case
  if (s2 < 1e-10) {
    return(list(alpha = 100, beta = 100, converged = TRUE, iterations = 0))
  }

  nu <- xbar * (1 - xbar) / s2 - 1
  alpha <- max(0.5, xbar * nu)
  beta <- max(0.5, (1 - xbar) * nu)

  # Newton-Raphson iteration
  for (iter in seq_len(max_iter)) {
    ab <- alpha + beta

    # Digamma and trigamma functions
    psi_a <- digamma(alpha)
    psi_b <- digamma(beta)
    psi_ab <- digamma(ab)

    psi1_a <- trigamma(alpha)
    psi1_b <- trigamma(beta)
    psi1_ab <- trigamma(ab)

    # Gradient
    g1 <- psi_a - psi_ab - log_x_bar
    g2 <- psi_b - psi_ab - log_1mx_bar

    # Check convergence
    if (abs(g1) < tol && abs(g2) < tol) {
      return(list(
        alpha = alpha,
        beta = beta,
        converged = TRUE,
        iterations = iter
      ))
    }

    # Hessian
    H11 <- psi1_a - psi1_ab
    H12 <- -psi1_ab
    H22 <- psi1_b - psi1_ab

    # Determinant
    det_H <- H11 * H22 - H12^2

    # Check for numerical issues
    if (abs(det_H) < 1e-10) {
      warning("Hessian near singular, returning current estimates")
      return(list(
        alpha = alpha,
        beta = beta,
        converged = FALSE,
        iterations = iter
      ))
    }

    # Newton step (solve H * delta = -g)
    dalpha <- -(H22 * g1 - H12 * g2) / det_H
    dbeta <- -(H11 * g2 - H12 * g1) / det_H

    # Update with damping to ensure positivity
    alpha_new <- alpha + dalpha
    beta_new <- beta + dbeta

    # Line search / damping
    step_size <- 1.0
    while ((alpha_new <= 0 || beta_new <= 0) && step_size > 0.01) {
      step_size <- step_size * 0.5
      alpha_new <- alpha + step_size * dalpha
      beta_new <- beta + step_size * dbeta
    }

    if (alpha_new <= 0 || beta_new <= 0) {
      warning("Cannot maintain positivity, returning current estimates")
      break
    }

    alpha <- alpha_new
    beta <- beta_new
  }

  # If reached max_iter without convergence
  warning(sprintf(
    "Max iterations reached without convergence (g1=%.2e, g2=%.2e)",
    g1,
    g2
  ))
  return(list(
    alpha = alpha,
    beta = beta,
    converged = FALSE,
    iterations = max_iter
  ))
}
