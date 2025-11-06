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

  expect_error(coef(learner))
  learner$train()
  coefs <- coef(learner)

  expect_true(inherits(coefs, "data.table"))
  expect_true(all(
    c("cat_hp", "hyperparameters", "A", "b") %in% colnames(coefs)
  ))
  expect_true(inherits(coefs[1, A][[1]], "matrix"))
  expect_true(inherits(coefs[1, b][[1]], "array"))

  tsk2 <- as_task_subspace(DT, formula = auc ~ (hp1 + hp2))
  learner2 <- LearnerSubspaceBoxGeneral$new(tsk2)
  learner2$train(lambda = .1)
  expect_false("cat_hp" %in% coef(learner2))
})
