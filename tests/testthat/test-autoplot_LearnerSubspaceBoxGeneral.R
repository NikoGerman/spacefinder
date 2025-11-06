test_that("basic tests", {
  DT <- withr::with_seed(
    1,
    data.table(
      task = sample(c("T1", "T2", "T3"), 300, replace = TRUE),
      auc = sample(runif(30), 100, replace = TRUE),
      hp1 = rnorm(300),
      hp2 = rnorm(300),
      hp3 = rnorm(300),
      cat_hp = sample(c("A", "B"), 300, replace = TRUE)
    )
  )

  tsk <- as_task_subspace(DT, formula = auc ~ (hp1 + hp2 + hp3) * cat_hp)
  learner <- LearnerSubspaceBoxGeneral$new(tsk)
  expect_error(autoplot(learner))

  learner$train()

  plots_wrapped <- ggplot2::autoplot(
    learner,
    size_top = 3,
    size_all = 2,
    wrap = TRUE
  )
  expect_true(all(vapply(
    plots_wrapped,
    \(plot) inherits(plot, "patchwork"),
    logical(1)
  )))

  plots_unwrapped <- ggplot2::autoplot(
    learner,
    size_top = 1.5,
    size_all = 1.2,
    wrap = FALSE
  )
  expect_true(all(
    vapply(
      plots_unwrapped,
      \(plots) {
        all(
          vapply(
            plots,
            \(x) inherits(x, "ggplot"),
            logical(1)
          )
        )
      },
      logical(1)
    )
  ))
})
