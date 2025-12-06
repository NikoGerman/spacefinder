#' @include LearnerSubspaceBox.R helper.R
#' @title Visualize fitted axis-aligned hyperrectangles
#' @description
#' Creates pairwise scatter plots showing fitted axis-aligned hyperrectangles
#' overlaid on hyperparameter configurations. Displays both the top-performing
#' configurations used for fitting and all data points for context.
#'
#' @param object A trained \code{LearnerSubspaceBox} object with fitted subspace
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
#'   \item Blue rectangle: Fitted axis-aligned hyperrectangle (box bounds)
#' }
#'
#' For \eqn{p} selected hyperparameters, creates \eqn{\binom{p}{2}} pairwise plots.
#'
#' \strong{Categorical Hyperparameters:}
#'
#' When the task includes a categorical hyperparameter, separate plots are created
#' for each categorical level, showing the corresponding fitted box.
#'
#' \strong{Interactive Prompt:}
#'
#' When plotting more than 3 hyperparameters with wrapping enabled, the function
#' prompts for confirmation due to potential readability issues. Use \code{force = TRUE}
#' to bypass this prompt.
#'
#' \strong{Dependencies:}
#'
#' Requires \code{ggplot2}. If \code{wrap = TRUE}, also requires \code{patchwork}.
#'
#' @seealso
#' \code{\link{LearnerSubspaceBox}} for the learner class.
#' \code{\link{coef.LearnerSubspaceBox}} for extracting fitted bounds.
#'
#' @examples
#' \dontrun{
#' # Train learner
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"))
#' learner <- LearnerSubspaceBox$new(task)
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
#' # Customize wrapping layout
#' autoplot(learner, ncol = 2, guides = "collect")
#'
#' # With categorical hyperparameters
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"),
#'                          cat_hps = "optimizer")
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train(q_val = 0.9)
#' autoplot(learner)  # Separate plots per optimizer
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.LearnerSubspaceBox <- function(
  object,
  select = "all",
  force = FALSE,
  wrap = TRUE,
  size_top = .7,
  size_all = .5,
  ...
) {
  pkgs <- c("ggplot2")
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

  plots <- list()
  if (is.null(object$task$cat_hps)) {
    pairs <- utils::combn(selected_cols, 2, simplify = FALSE)
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
        )

      plots[[length(plots) + 1]] <- p
    }
    if (wrap) {
      plots <- patchwork::wrap_plots(plots, ...)
    }
  } else {
    levels <- names(object$result)
    cat_hps <- object$task$cat_hps
    for (level in levels) {
      subplots <- list()
      coefs <- stats::coef(object)[get(cat_hps) == level, ]
      hps <- intersect(selected_cols, unique(coefs$hyperparameter))
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
          ggplot2::ggtitle(sprintf("%s = %s", cat_hps, level))
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
            ggplot2::ggtitle(
              if (wrap) {
                NULL
              } else {
                sprintf(
                  "%s = %s",
                  cat_hps,
                  level
                )
              }
            )
          subplots[[length(subplots) + 1]] <- p
        }
      }
      if (wrap) {
        plots[[length(plots) + 1]] <- patchwork::wrap_plots(subplots, ...) +
          patchwork::plot_annotation(
            title = sprintf(
              "%s = %s",
              cat_hps,
              level
            )
          )
      } else {
        plots[[length(plots) + 1]] <- subplots
      }
    }
    names(plots) <- levels
  }
  return(plots)
}
