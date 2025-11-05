test_that("basic tests", {
  DT <- withr::with_seed(
    1,
    data.table(
      task = sample(c("T1", "T2", "T3"), 30, replace = TRUE),
      auc = runif(30),
      hp1 = rnorm(30),
      hp2 = rnorm(30),
      cat_hp = sample(c("A", "B"), 30, replace = TRUE)
    )
  )

  tsk <- as_task_subspace(DT, formula = auc ~ (hp1 + hp2) * cat_hp)
  learner <- LearnerSubspaceElips$new(tsk)

  expect_error(summary(learner))
  learner$train()
  summary.learner <- summary(learner)

  expect_true(all(
    c("status", "coefficients", "outliers") %in% names(summary.learner)
  ))
  expect_true(inherits(summary.learner$coefficients, "data.table"))
  expect_true(nrow(summary.learner$outliers) == 0)
})
