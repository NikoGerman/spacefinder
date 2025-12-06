test_that("basic tests", {
  data("benchmark_data")
  task <- as_task_subspace(
    benchmark_data,
    auc ~ (learning_rate + max_depth) * optimizer
  )
  learner <- LearnerSubspacePolygon$new(task)
  learner$train()
  aug <- suppressWarnings(augment(learner))
  expect_true(inherits(aug, "data.table"))
  expect_true(all(c("alpha", "beta") %in% colnames(aug)))
})
