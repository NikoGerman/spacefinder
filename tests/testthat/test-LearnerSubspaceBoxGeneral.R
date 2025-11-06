test_that("basic tests", {
  DT <- withr::with_seed(
    1,
    data.table(
      task = sample(c("T1", "T2", "T3"), 10, replace = TRUE),
      auc = runif(10),
      hp1 = rnorm(10),
      hp2 = rnorm(10),
      cat_hp = sample(c("A", "B"), 10, replace = TRUE)
    )
  )

  tsk <- as_task_subspace(DT, formula = auc ~ (hp1 + hp2) * cat_hp)
  learner <- LearnerSubspaceBoxGeneral$new(tsk)
  expect_true(inherits(learner, "LearnerSubspace"))
  expect_true(inherits(learner, "LearnerSubspaceBoxGeneral"))

  learner$train()
  expect_true(!is.null(learner$result))
  expect_true(!is.null(learner$result$A))
  expect_true(!is.null(learner$result$B))

  learner$train(lambda = .1)
  expect_false(any(vapply(learner$result$A, is.null, logical(1))))
  expect_false(any(vapply(learner$result$B, is.null, logical(1))))
})
