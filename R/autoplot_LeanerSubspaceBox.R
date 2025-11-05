#' @include LearnerSubspaceBox.R helper.R
#' @title autoplot method for LearnerSubspaceBox
#' @param object A LearnerSubspaceBox object
#' @param wrap flag: Wrap plots using patchwork?
#' @param size_top point size of top_config points
#' @param size_all point size of all points in the dataset
#' @param ... Additional arguments to patchwork::wrap_plots()
#' @exportS3Method ggplot2::autoplot
autoplot.LearnerSubspaceBox <- function(
  object,
  wrap = TRUE,
  size_top = .7,
  size_all = .5,
  ...
) {
  check_ggplot2()
  if (wrap) {
    check_patchwork()
  }

  if (is.null(object$result)) {
    stop("No result found. Run train() first.")
  }
  plots <- list()
  if (is.null(object$task$cat_hps)) {
    pairs <- utils::combn(object$task$hps, 2, simplify = FALSE)
    coefs <- stats::coef(object)
    for (pair in pairs) {
      hp1 <- pair[[1]]
      hp2 <- pair[[2]]

      range_hp1 <- coefs[hyperparameter == hp1, mget(c("min", "max"))]
      range_hp2 <- coefs[hyperparameter == hp2, mget(c("min", "max"))]

      p <- ggplot2::ggplot(
        object$top_configs,
        ggplot2::aes(x = !!ggplot2::sym(hp1), y = !!ggplot2::sym(hp2))
      ) +
        ggplot2::geom_rect(
          xmin = range_hp1$min,
          xmax = range_hp1$max,
          ymin = range_hp2$min,
          ymax = range_hp2$max,
          color = "#2E86AB",
          alpha = 0.00001,
          linewidth = 1
        ) +
        ggplot2::geom_point(
          color = "orange",
          alpha = 1,
          size = size_top,
          shape = 3
        ) +
        ggplot2::geom_point(
          data = object$task$data,
          alpha = .05,
          size = size_all
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 10)
        ) +
        ggplot2::ggtitle(paste(hp1, "vs", hp2))

      plots[[length(plots) + 1]] <- p
    }
  } else {
    levels <- names(object$result)
    cat_hps <- object$task$cat_hps
    for (level in levels) {
      subplots <- list()
      coefs <- stats::coef(object)[get(cat_hps) == level, ]
      hps <- unique(coefs$hyperparameter)
      if (length(hps) <= 1) {
        p <- ggplot2::ggplot(
          object$top_configs[get(cat_hps) == level],
          ggplot2::aes(x = !!ggplot2::sym(hps), y = 0)
        ) +
          ggplot2::geom_rect(
            xmin = coefs[hyperparameter == hps, get("min")],
            xmax = coefs[hyperparameter == hps, get("max")],
            ymin = -0.5,
            ymax = 0.5,
            color = "#2E86AB",
            alpha = 0.00001,
            linewidth = 1
          ) +
          ggplot2::geom_point(
            position = ggplot2::position_jitter(width = 0, height = 0.2),
            color = "orange",
            alpha = 1,
            size = size_top,
            shape = 3
          ) +
          ggplot2::geom_point(
            data = object$task$data[get(cat_hps) == level],
            position = ggplot2::position_jitter(width = 0, height = 0.2),
            alpha = .05,
            size = size_all
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 10)
          ) +
          ggplot2::ggtitle(sprintf("%s (%s = %s)", hps, cat_hps, level))
        subplots[[length(subplots) + 1]] <- p
      } else {
        pairs <- utils::combn(hps, 2, simplify = FALSE)
        for (pair in pairs) {
          hp1 <- pair[[1]]
          hp2 <- pair[[2]]

          range_hp1 <- coefs[hyperparameter == hp1, mget(c("min", "max"))]
          range_hp2 <- coefs[hyperparameter == hp2, mget(c("min", "max"))]

          p <- ggplot2::ggplot(
            object$top_configs[get(cat_hps) == level],
            ggplot2::aes(x = !!ggplot2::sym(hp1), y = !!ggplot2::sym(hp2))
          ) +
            ggplot2::geom_rect(
              xmin = range_hp1$min,
              xmax = range_hp1$max,
              ymin = range_hp2$min,
              ymax = range_hp2$max,
              color = "#2E86AB",
              alpha = 0.00001,
              linewidth = 1
            ) +
            ggplot2::geom_point(
              color = "orange",
              alpha = 1,
              size = size_top,
              shape = 3
            ) +
            ggplot2::geom_point(
              data = object$task$data[get(cat_hps) == level],
              alpha = .05,
              size = size_all
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", size = 10)
            ) +
            ggplot2::ggtitle(sprintf(
              "%s vs %s (%s = %s)",
              hp1,
              hp2,
              cat_hps,
              level
            ))
          subplots[[length(subplots) + 1]] <- p
        }
      }
      if (wrap) {
        plots[[length(plots) + 1]] <- patchwork::wrap_plots(subplots, ...)
      } else {
        plots[[length(plots) + 1]] <- subplots
      }
    }
  }
  return(plots)
}
