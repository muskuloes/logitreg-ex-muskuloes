test_that("fit_logitreg estimates are the same as the glm estimates", {
  data <- sim_data(
    n = 300,
    seed = 321,
    numerics = 1,
    dataframe = TRUE
  )
  logitreg_estimates <- coefficients(logitreg(
    response ~ .,
    data = data
  ))
  glm_estimates <- unname(coefficients(glm(response ~ .,
    data = data,
    family = "binomial"
  )))
  expect_equal(
    signif(logitreg_estimates, 2),
    signif(glm_estimates, 2)
  )
})

test_that("predict works", {
  data <- sim_data(
    n = 100,
    seed = 223,
    numerics = 3,
    dataframe = TRUE
  )
  logitreg_predict <- predict(logitreg(
    response ~ .,
    data = data
  ))
  glm_predict <- predict(glm(response ~ .,
    data = data,
    family = "binomial"
  ))
  expect_equal(
    signif(logitreg_predict, 2),
    signif(glm_predict, 2),
    ignore_attr = TRUE
  )
})

test_that("logitreg returns for trouble1 and trouble2 datasets", {
  load(system.file("inst", "testdata", "logitreg-data-trouble.Rdata",
    package = "logitreg"
  ))
  logitreg_estimates <- coefficients(logitreg(
    y ~ -1 + x,
    data = trouble1
  ))
  glm_estimates <- unname(coefficients(glm(y ~ -1 + x,
    data = trouble1,
    family = "binomial"
  )))
  expect_equal(
    signif(logitreg_estimates, 1),
    signif(glm_estimates, 1)
  )
})
