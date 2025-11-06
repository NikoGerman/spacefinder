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

#' @keywords internal
check_grDevices <- function() {
  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop(
      "Package 'grDevices' needed for this function. Please install it.",
      call. = FALSE
    )
  }
}

#' @keywords internal
resolve_selected <- function(object, select) {
  present_hps <- object$task$hps
  if (select == "all") {
    return(present_hps)
  }
  selected <- base::intersect(present_hps, select)
  if (length(selected) == 0) {
    stop("selected cols not present in task")
  }
  return(selected)
}
