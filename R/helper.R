#' @keywords internal
check_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' needed for this function. Please install it.",
      call. = FALSE
    )
  }
}

#' @keywords internal
check_patchwork <- function() {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' needed for this function. Please install it.",
      call. = FALSE
    )
  }
}
