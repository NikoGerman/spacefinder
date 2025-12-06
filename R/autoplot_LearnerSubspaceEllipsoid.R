#' @include LearnerSubspaceEllipsoid.R helper.R
#' @title Visualize fitted ellipsoids
#' @description
#' Creates pairwise scatter plots showing fitted ellipsoids overlaid on
#' hyperparameter configurations. Uses marginal ellipse projections to
#' visualize the smooth ellipsoidal boundaries in 2D pairwise plots.
#'
#' @param object A trained \code{LearnerSubspaceEllipsoid} object with fitted subspace
#' @param select Character vector of hyperparameter names to plot, or "all"
#'   (default) to plot all hyperparameters
#' @param wrap Logical indicating whether to combine plots using
#'   \code{patchwork::wrap_plots()}. Default: \code{TRUE}
#' @param force Logical indicating whether to skip the confirmation prompt when
#'   plotting many hyperparameters. Default: \code{FALSE}
#' @param size_top Numeric point size for top-performing configurations
#'   (orange crosses). Default: 0.7
#' @param size_all Numeric point size for all data points (gray background).
#'   Default: 0.5
#' @param ... Additional arguments passed to \code{patchwork::wrap_plots()}
#'   (only used when \code{wrap = TRUE})
#'
#' @return
#' If \code{wrap = TRUE}: A single patchwork object combining all plots.
#'
#' If \code{wrap = FALSE} and no categorical hyperparameters: A list of ggplot objects,
#' one per hyperparameter pair.
#'
#' If \code{wrap = FALSE} and categorical hyperparameters present: A named list where
#' each element is a list of ggplot objects for that categorical level.
#'
#' @details
#' \strong{Plot Structure:}
#'
#' Each plot shows:
#' \itemize{
#'   \item Gray points: All configurations in the dataset (low alpha)
#'   \item Orange crosses: Top-performing configurations used for fitting
#'   \item Blue ellipse: Projection of the fitted ellipsoid onto 2D plane
#'   \item Blue diamond: Center of the ellipsoid
#' }
#'
#' For \eqn{p} selected hyperparameters, creates \eqn{\binom{p}{2}} pairwise plots.
#'
#' \strong{Visualization Method:}
#'
#' The ellipsoid is defined by \eqn{\|Ax + b\|_2 \leq 1}. For visualization:
#' \enumerate{
#'   \item Computes center: \eqn{c = -A^{-1}b}
#'   \item Computes covariance: \eqn{\Sigma = A^{-1}(A^{-1})^T}
#'   \item For each 2D pair \eqn{(i,j)}, extracts marginal covariance \eqn{\Sigma_{ij}}
#'   \item Uses Cholesky decomposition to transform unit circle to ellipse:
#'     \eqn{x = L \theta + c_{ij}} where \eqn{L} is lower Cholesky factor of \eqn{\Sigma_{ij}}
#'     and \eqn{\theta} are points on unit circle
#' }
#'
#' The resulting ellipse represents the marginal distribution of the ellipsoid
#' projected onto the 2D hyperparameter pair, showing smooth boundaries characteristic
#' of ellipsoidal subspaces.
#'
#' \strong{Univariate Case:}
#'
#' When only one hyperparameter is selected, displays a histogram with vertical
#' lines marking the ellipsoid boundaries (reduces to interval) and rug plot for
#' top configurations.
#'
#' \strong{Categorical Hyperparameters:}
#'
#' When the task includes a categorical hyperparameter, separate plots are created
#' for each categorical level, showing the corresponding fitted ellipsoid.
#'
#' \strong{Interactive Prompt:}
#'
#' When plotting more than 3 hyperparameters with wrapping enabled, the function
#' prompts for confirmation due to potential readability issues. Use \code{force = TRUE}
#' to bypass this prompt.
#'
#' \strong{Dependencies:}
#'
#' Requires \code{ggplot2} and \code{scales}. If \code{wrap = TRUE},
#' also requires \code{patchwork}.
#'
#' @seealso
#' \code{\link{LearnerSubspaceEllipsoid}} for the learner class.
#' \code{\link{coef.LearnerSubspaceEllipsoid}} for extracting fitted parameters.
#' \code{\link{autoplot.LearnerSubspaceBox}} for axis-aligned visualization.
#' \code{\link{autoplot.LearnerSubspacePolygon}} for oriented hyperrectangle visualization.
#'
#' @examples
#' \dontrun{
#' # Train learner
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"))
#' learner <- LearnerSubspaceEllipsoid$new(task)
#' learner$train(q_val = 0.9, lambda = 0.1)
#'
#' # Plot all hyperparameters (wrapped)
#' autoplot(learner)
#'
#' # Plot specific hyperparameters
#' autoplot(learner, select = c("learning_rate", "max_depth"))
#'
#' # Get individual plots without wrapping
#' plots <- autoplot(learner, wrap = FALSE)
#' plots[[1]]  # First pairwise plot
#'
#' # Customize ellipse smoothness
#' autoplot(learner, n_points = 500)  # More points for smoother curves
#'
#' # Customize wrapping layout
#' autoplot(learner, ncol = 2, guides = "collect")
#'
#' # With categorical hyperparameters
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"),
#'                          cat_hps = "optimizer")
#' learner <- LearnerSubspaceEllipsoid$new(task)
#' learner$train(q_val = 0.9)
#' autoplot(learner)  # Separate plots per optimizer
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.LearnerSubspaceEllipsoid <- function(
  object,
  select = "all",
  force = FALSE,
  wrap = TRUE,
  size_top = .7,
  size_all = .5,
  ...
) {
  pkgs <- c("ggplot2", "scales")
  if (wrap) {
    pkgs <- c(pkgs, "patchwork")
  }
  check_packages(pkgs)

  if (is.null(object$result)) {
    stop("No result found. Run train() first.", call. = FALSE)
  }

  selected_cols <- resolve_selected(object, select)

  if (!force && wrap && length(selected_cols) > 3) {
    message(sprintf(
      "Plotting with %d selected hyperparameters results in up to %d plots (per category).
Wrapping this many plots onto one results in very poor readability.",
      length(selected_cols),
      choose(length(selected_cols), 2)
    ))
    response <- readline("Continue? (y/n): ")
    if (!tolower(response) %in% c("y", "yes")) {
      message("Plotting cancelled.")
      return(invisible(object))
    }
  }

  if (is.null(object$task$cat_hps)) {
    plots <- create_ellipsoid_pairwise_plots(
      stats::coef(object),
      hps = selected_cols,
      top_configs = object$top_configs[, mget(selected_cols)],
      data = object$task$data[, mget(selected_cols)],
      n_points = 300,
      size_top = size_top,
      size_all = size_all,
      wrap = wrap
    )
    return(plots)
  } else {
    coefs <- stats::coef(object)
    levels <- names(object$result)
    cat_hps <- object$task$cat_hps
    plots <- lapply(levels, \(x) {
      create_ellipsoid_pairwise_plots(
        coefficients = coefs[get(cat_hps) == x, ],
        hps = selected_cols,
        top_configs = object$top_configs[
          get(cat_hps) == x,
          mget(selected_cols)
        ],
        data = object$task$data[get(cat_hps) == x, mget(selected_cols)],
        n_points = 300,
        size_top = size_top,
        size_all = size_all,
        wrap = wrap,
        cat_hps = cat_hps,
        level = x
      )
    })
    names(plots) <- levels
    return(plots)
  }
}

#' @keywords internal
#' @title Create ellipse pairwise plots for ellipsoids
#' @description
#' Helper function that generates pairwise scatter plots with marginal ellipse
#' projections of fitted ellipsoids.
#'
#' @param coefficients Coefficient data.table from coef(learner, vectorize = TRUE)
#' @param hps Character vector of hyperparameter names to plot
#' @param top_configs data.table of top-performing configurations
#' @param data data.table of all configurations
#' @param n_points Number of points to use for drawing smooth ellipse curves (default: 300)
#' @param size_top Point size for top configurations
#' @param size_all Point size for all data points
#' @param wrap Logical indicating whether to wrap plots
#' @param cat_hps Name of categorical hyperparameter (if any)
#' @param level Current categorical level being plotted (if any)
#' @param ... Additional arguments passed to patchwork::wrap_plots()
#'
#' @return
#' If \code{wrap = TRUE}: A patchwork object combining all plots.
#' If \code{wrap = FALSE}: A list of ggplot objects.
#' For univariate case: A single ggplot histogram.
create_ellipsoid_pairwise_plots <- function(
  coefficients,
  hps,
  top_configs,
  data,
  n_points,
  size_top,
  size_all,
  wrap,
  cat_hps = NULL,
  level = NULL,
  ...
) {
  A <- coefficients$A[[1]]
  b <- coefficients$b[[1]]
  hps <- intersect(hps, coefficients$hyperparameters[[1]])
  p <- nrow(A)

  if (p < 2) {
    message("Need at least 2 hyperparameters for pairwise plots")

    A <- c(A)
    b <- c(b)
    center <- -b / A
    borders <- c(-1, 1) / A + center

    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!ggplot2::sym(hps))) +
      ggplot2::geom_histogram(ggplot2::aes(
        y = ggplot2::after_stat(count / sum(count))
      )) +
      ggplot2::geom_rug(
        data = top_configs,
        ggplot2::aes(x = !!ggplot2::sym(hps)),
        color = "orange"
      ) +
      ggplot2::geom_vline(
        xintercept = borders,
        color = "#2E86AB",
        linewidth = 1.5
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::labs(
        title = if (wrap || is.null(cat_hps) || is.null(level)) {
          NULL
        } else {
          sprintf("%s = %s", cat_hps, level)
        },
        x = hps,
        y = "frequency"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 10)
      )
    return(p)
  }

  dim_pairs <- utils::combn(p, 2, simplify = FALSE)

  plots <- list()

  R_full <- chol(A)
  A_inv <- chol2inv(R_full)
  Sigma <- A_inv %*% t(A_inv)
  center_full <- -A_inv %*% b

  theta <- seq(0, 2 * pi, length.out = n_points)
  circle <- cbind(cos(theta), sin(theta))

  for (pair in dim_pairs) {
    i <- pair[1]
    j <- pair[2]
    hp1 <- hps[[i]]
    hp2 <- hps[[j]]

    center_2d <- center_full[c(i, j)]
    Sigma_2d <- Sigma[c(i, j), c(i, j)]
    L <- t(chol(Sigma_2d))

    ellipse_points <- sweep(as.matrix(circle) %*% t(L), 2, center_2d, "+")
    ellipse_dt <- data.table::as.data.table(ellipse_points)
    colnames(ellipse_dt) <- c("x", "y")

    p_plot <- ggplot2::ggplot(ellipse_dt, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_polygon(fill = "#2E86AB", alpha = 0.05) +
      ggplot2::geom_path(color = "#2E86AB", linewidth = 1) +
      ggplot2::geom_point(
        x = center_2d[1],
        y = center_2d[2],
        color = "#2E86AB",
        size = 3,
        shape = 18
      ) +
      ggplot2::geom_point(
        data = top_configs,
        ggplot2::aes(x = !!ggplot2::sym(hp1), y = !!ggplot2::sym(hp2)),
        color = "orange",
        alpha = 1,
        size = size_top,
        shape = 3
      ) +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(x = !!ggplot2::sym(hp1), y = !!ggplot2::sym(hp2)),
        alpha = .05,
        size = size_all
      ) +
      ggplot2::labs(
        title = if (wrap || is.null(cat_hps) || is.null(level)) {
          NULL
        } else {
          sprintf("%s = %s", cat_hps, level)
        },
        x = hp1,
        y = hp2
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 10)
      )

    plots[[length(plots) + 1]] <- p_plot
  }

  if (wrap) {
    combined <- (patchwork::wrap_plots(plots, ...)) +
      patchwork::plot_annotation(
        title = if (is.null(cat_hps) || is.null(level)) {
          NULL
        } else {
          sprintf("%s = %s", cat_hps, level)
        }
      )
    return(combined)
  } else {
    return(plots)
  }
}
