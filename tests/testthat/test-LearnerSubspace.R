test_that("basic tests", {
  DT <- data.table(
    task = sample(c("T1", "T2", "T3"), 10, replace = TRUE),
    auc = runif(10),
    hp1 = rnorm(10),
    hp2 = rnorm(10),
    cat_hp = sample(c("A", "B"), 10, replace = TRUE)
  )

  tsk <- as_task_subspace(DT, formula = auc ~ (hp1 + hp2) * cat_hp)
  learner <- LearnerSubspace$new(tsk)

  expect_true(inherits(learner, "LearnerSubspace"))
  expect_error(learner$train())
})
