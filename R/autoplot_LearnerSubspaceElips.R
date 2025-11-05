#' @include LearnerSubspaceElips.R helper.R
#' @title autoplot method for LearnerSubspaceElips
#' @param object A LearnerSubspaceElips object
#' @param wrap flag: Wrap plots using patchwork?
#' @param size_top point size of top_config points
#' @param size_all point size of all points in the dataset
#' @param ... Additional arguments to patchwork::wrap_plots()
#' @param ... Additional arguments (unused)
#' @exportS3Method ggplot2::autoplot
autoplot.LearnerSubspaceElips <- function(
  object,
  size_top = .7,
  size_all = .5,
  wrap = TRUE,
  ...
) {
  check_ggplot2()
  if (wrap) {
    check_patchwork()
  }

  if (is.null(object$result)) {
    stop("No result found. Run train() first.")
  }
  if (is.null(object$task$cat_hps)) {
    plots <- create_ellipsoid_pairwise_plots(
      stats::coef(object),
      hps = object$task$hps,
      top_configs = object$top_configs,
      data = object$task$data,
      n_points = 300,
      size_top = size_top,
      size_all = size_all
    )
    if (wrap) {
      return(patchwork::wrap_plots(plots, ...))
    } else {
      return(plots)
    }
  } else {
    print("not yet supported")
  }
}

#' @keywords internal
create_ellipsoid_pairwise_plots <- function(
  coefficients,
  hps,
  top_configs,
  data,
  n_points,
  size_top,
  size_all
) {
  A <- coefficients$A[[1]]
  b <- coefficients$b[[1]]
  p <- nrow(A)

  if (p < 2) {
    stop("Need at least 2 hyperparameters for pairwise plots")
  }

  # Create all pairwise combinations
  dim_pairs <- utils::combn(p, 2, simplify = FALSE)

  plots <- list()

  for (pair in dim_pairs) {
    i <- pair[1]
    j <- pair[2]
    hp1 <- hps[[i]]
    hp2 <- hps[[j]]

    # Generate ellipse points
    theta <- seq(0, 2 * pi, length.out = n_points)
    circle <- cbind(cos(theta), sin(theta))

    # Extract 2D submatrix and vector
    A_sub <- A[c(i, j), c(i, j)]
    b_sub <- b[c(i, j)]

    R <- chol(A_sub)

    ellipse_points <- t(backsolve(R, forwardsolve(t(R), t(circle) - b_sub)))

    # Compute center
    center <- -backsolve(R, forwardsolve(t(R), b_sub))

    ellipse_dt <- data.table::as.data.table(ellipse_points)
    colnames(ellipse_dt) <- c("x", "y")

    # Create title
    title <- paste0(hp1, " vs ", hp2)

    p_plot <- ggplot2::ggplot(ellipse_dt, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_polygon(fill = "#2E86AB", alpha = 0.05) +
      ggplot2::geom_path(color = "#2E86AB", linewidth = 1) +
      ggplot2::geom_point(
        x = center[1],
        y = center[2],
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
        title = title,
        x = hp1,
        y = hp2
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 10)
      )

    plots[[length(plots) + 1]] <- p_plot
  }

  return(plots)
}
