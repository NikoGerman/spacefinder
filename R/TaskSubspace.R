#' @title TaskSubspace class
#' @export
TaskSubspace <- R6::R6Class(
  "TaskSubspace",

  #' @field data A data table, containing hyperparameter and a target measure.
  #' needs to have a column named 'task'.
  #' @field target_measure a character string: the target metric (e.g. auc, mce, brier loss,...)
  #' @field hps One or more numeric hyperparameters
  #' @field cat_hps currently up to one categorical hyperparamter
  public = list(
    data = NULL,
    target_measure = NULL,
    hps = NULL,
    cat_hps = NULL,

    #' @param data A data.table containing task performance data
    #' @param formula target_measure ~ (hp1 + hp2 + ...) * (cat_hp1 + cat_hp2 + ...) (optional)
    #' @param target_measure Name of the performance measure column
    #' @param hps Character vector of continuous hyperparameter names
    #' @param cat_hps Character vector of categorical hyperparameter names (optional)
    initialize = function(
      data,
      formula = NULL,
      target_measure = NULL,
      hps = NULL,
      cat_hps = NULL
    ) {
      checkmate::assertDataTable(data)
      checkmate::assertChoice(c("task"), colnames(data))

      if (is.null(formula)) {
        if (is.null(target_measure) | is.null(hps)) {
          stop(
            "Provide either a formula, or at least 'target_measure' and 'hps'"
          )
        } else {
          checkmate::assertCharacter(
            hps,
            min.len = 1,
            null.ok = FALSE,
            any.missing = FALSE
          )
          checkmate::assertCharacter(
            cat_hps,
            min.len = 1,
            null.ok = TRUE,
            any.missing = FALSE
          )
          checkmate::assertCharacter(
            target_measure,
            len = 1,
            null.ok = FALSE,
            any.missing = FALSE
          )
          checkmate::assertChoice(target_measure, colnames(data))

          self$hps <- hps
          self$cat_hps <- cat_hps
          self$target_measure <- target_measure
        }
      } else {
        if (
          any(vapply(
            c(target_measure, hps, cat_hps),
            \(x) !is.null(x),
            logical(1)
          ))
        ) {
          warning("Explicitly provided entries are overwritten by 'formula'")
        }
        form <- as.formula(formula)
        checkmate::assertChoice(all.vars(form[[2]]), colnames(data))
        self$target_measure <- all.vars(form[[2]])
        rhs <- form[[3]]
        self$hps <- all.vars(rhs[[2]])

        if (length(rhs) >= 3) {
          self$cat_hps <- all.vars(rhs[[3]])
        }
      }
      self$data <- data
    }
  )
)
