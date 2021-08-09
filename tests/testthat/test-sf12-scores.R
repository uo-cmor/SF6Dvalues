sf12 <- SF12(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
             Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)

sf12_miss <- SF12(Q1 = 1:2, Q2 = c(NA, NA), Q3 = c(1, NA), Q4 = 1:2, Q5 = 1:2, Q6 = 1:2,
                  Q7 = 1:2, Q8 = 1:2, Q9 = 1:2, Q10 = 1:2, Q11 = 1:2, Q12 = 1:2)

levels <- list(PF = 1:3, RL = 2:4, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
sf6d <- rlang::exec(SF6D, !!!levels)

test_that("PCS works as expected", {
  expect_equal(PCS(sf12), c(39.166948, 40.170937, 42.250975))
})

test_that("MCS works as expected", {
  expect_equal(MCS(sf12), c(32.9888216, 34.3307077, 35.6048503))
})

test_that("PCS handles missing values", {
  expect_equal(PCS(sf12_miss), c(43.47114, NA))
})

test_that("MCS handles missing values", {
  expect_equal(MCS(sf12_miss), c(32.717848, NA))
})

test_that("sf12_PCS works as expected", {
  expect_equal(
    sf12_PCS(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
             Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3),
    c(39.166948, 40.170937, 42.250975)
  )
})

test_that("sf12_MCS works as expected", {
  expect_equal(
    sf12_MCS(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
             Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3),
    c(32.9888216, 34.3307077, 35.6048503)
  )
})

test_that("SF12_domains gives appropriate error messages", {
  expect_error(SF12_domains(sf6d), class = "SF6Dvalues_error_not_SF12")
  expect_error(SF12_domains(levels), class = "SF6Dvalues_error_not_SF12")
  expect_error(SF12_domains(1), class = "SF6Dvalues_error_not_SF12")
  expect_error(SF12_domains("111111"), class = "SF6Dvalues_error_not_SF12")
  attr(sf12, "version") <- 1L
  expect_error(SF12_domains(sf12), class = "SF6Dvalues_error_version_1")
})
