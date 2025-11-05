test_that("basic tests", {
  DT <- data.table(
    task = sample(c("T1", "T2", "T3"), 10, replace = TRUE),
    auc = runif(10),
    hp1 = rnorm(10),
    hp2 = rnorm(10),
    cat_hp = sample(c("A", "B"), 10, replace = TRUE)
  )
  tsk1 <- TaskSubspace$new(DT, formula = auc ~ (hp1 + hp2) * cat_hp)
  tsk2 <- TaskSubspace$new(
    DT,
    target_measure = "auc",
    hps = c("hp1", "hp2"),
    cat_hps = "cat_hp"
  )

  expect_true(inherits(tsk1, "TaskSubspace"))
  expect_true(inherits(tsk2, "TaskSubspace"))

  expect_true(all(vapply(
    c("target_measure", "hps", "cat_hps"),
    \(x) identical(tsk1[[x]], tsk2[[x]]),
    logical(1)
  )))

  expect_error(TaskSubspace$new(
    DT[, -"task"],
    formula = auc ~ (hp1 + hp2) * cat_hp
  ))
  expect_warning(TaskSubspace$new(
    DT,
    formula = auc ~ (hp1 + hp2) * cat_hp,
    target_measure = "auc"
  ))
})
