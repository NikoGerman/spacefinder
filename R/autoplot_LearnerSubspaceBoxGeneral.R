#' @include LearnerSubspaceBoxGeneral.R helper.R
#' @title autoplot method for LearnerSubspaceBoxGeneral
#' @param object A LearnerSubspaceBoxGeneral object
#' @param select selected columns to print pairwise
#' @param wrap flag: Wrap plots using patchwork?
#' @param force flag: Force wrapping?
#' @param size_top point size of top_config points
#' @param size_all point size of all points in the dataset
#' @param ... Additional arguments to patchwork::wrap_plots()
#' @exportS3Method ggplot2::autoplot
autoplot.LearnerSubspaceBoxGeneral <- function(
  object,
  select = "all",
  force = FALSE,
  wrap = TRUE,
  size_top = .7,
  size_all = .5,
  ...
) {
  check_ggplot2()
  check_grDevices()
  if (wrap) {
    check_patchwork()
  }

  if (is.null(object$result)) {
    stop("No result found. Run train() first.")
  }

  selected_cols <- resolve_selected(object, select)

  if (!force && wrap && length(selected_cols) > 3) {
    message(sprintf(
      "Plotting with %d selected hyperparameters results in up to %d plots (per category).\n
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
    plots <- create_chull_pairwise_plots(
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
      create_chull_pairwise_plots(
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
create_chull_pairwise_plots <- function(
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
  p <- nrow(A)

  if (p < 2) {
    stop("Need at least 2 hyperparameters for pairwise plots")
  }

  # Create all pairwise combinations
  dim_pairs <- utils::combn(p, 2, simplify = FALSE)

  plots <- list()

  # Compute full inverse and center ONCE
  R_full <- chol(A)
  A_inv <- chol2inv(R_full)
  center <- -A_inv %*% b
  vertices_y <- as.matrix(expand.grid(rep(list(c(-1, 1)), p)))

  n_vertices <- nrow(vertices_y)
  vertices_x <- t(A_inv %*% t(vertices_y)) +
    matrix(center, nrow = n_vertices, ncol = p, byrow = TRUE)

  for (pair in dim_pairs) {
    i <- pair[1]
    j <- pair[2]
    hp1 <- hps[[i]]
    hp2 <- hps[[j]]

    hull_idx <- grDevices::chull(vertices_x[, c(i, j)])
    hull_vertices <- vertices_x[hull_idx, c(i, j)]

    # Close the polygon
    hull_vertices <- rbind(hull_vertices, hull_vertices[1, ])

    box_dt <- data.table::data.table(
      x = hull_vertices[, 1],
      y = hull_vertices[, 2]
    )

    p_plot <- ggplot2::ggplot(box_dt, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_polygon(fill = "#2E86AB", alpha = 0.05) +
      ggplot2::geom_path(color = "#2E86AB", linewidth = 1) +
      ggplot2::geom_point(
        x = center[i],
        y = center[j],
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
