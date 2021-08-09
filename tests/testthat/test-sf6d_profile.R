test_that("sf6d_profile() returns zero-length `SF6D` vector", {
  x <- sf6d_profile()
  expect_s3_class(x, "SF6Dvalues_SF6D")
  expect_length(x, 0)
  expect_equal(format(x), character())
  expect_output(print(x), "<SF6D[0]>", fixed = TRUE)
})

test_that("sf6d_profile works as expected", {
  responses <- list(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
                    Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)
  sf6d <- rlang::exec(sf6d_profile, !!!responses)

  expect_equal(field(sf6d, "PF"), 3:1)
  expect_equal(field(sf6d, "RL"), rep(4, 3))
  expect_equal(attr(sf6d, "version"), "SF-12")
  expect_s3_class(sf6d, "SF6Dvalues_SF6D")
})

test_that("sf6d_profile handles missing values", {
  responses <- list(Q1 = 3:5, Q2a = c(NA, NA, NA), Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
                    Q4b = 1:3, Q5 = c(1, 2, NA), Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)
  sf6d <- rlang::exec(sf6d_profile, !!!responses)
  expect_s3_class(sf6d, "SF6Dvalues_SF6D")
  expect_equal(format(sf6d), c(".45151", ".44242", ".43.33"))
})

