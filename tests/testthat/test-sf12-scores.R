sf12 <- SF12(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
             Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 3:1, Q7 = 1:3)

sf12_miss <- SF12(Q1 = 1:2, Q2 = c(NA, NA), Q3 = c(1, NA), Q4 = 1:2, Q5 = 1:2, Q6 = 1:2,
                  Q7 = 1:2, Q8 = 1:2, Q9 = 1:2, Q10 = 1:2, Q11 = 1:2, Q12 = 1:2)

levels <- list(PF = 1:3, RL = 2:4, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
sf6d <- rlang::exec(SF6D, !!!levels)

test_that("PCS works as expected", {
  expect_equal(PCS(sf12), c(36.4763897, 40.1709371, 44.9415328))
})

test_that("MCS works as expected", {
  expect_equal(MCS(sf12), c(38.9116090, 34.3307077, 29.6820629))
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

test_that("SF12_scores works as expected", {
  x <- SF12_scores(sf12)

  expect_equal(x$PF, c(22.108022, 39.286858, 56.465695))
  expect_equal(x$RP, c(20.323277, 29.536382, 38.749487))
  expect_equal(x$BP, c(57.443827, 47.252304, 37.060781))
  expect_equal(x$GH, c(44.740089, 29.647643, 18.867325))
  expect_equal(x$VT, c(67.875325, 57.812452, 47.749579))
  expect_equal(x$SF, c(16.176357, 26.274205, 36.372053))
  expect_equal(x$RE, c(11.346970, 22.529936, 33.712901))
  expect_equal(x$MH, c(52.349477, 40.157905, 27.966334))
})

test_that("SF12_domains gives appropriate error messages", {
  expect_error(SF12_domains(sf6d), class = "SF6Dvalues_error_not_SF12")
  expect_error(SF12_domains(levels), class = "SF6Dvalues_error_not_SF12")
  expect_error(SF12_domains(1), class = "SF6Dvalues_error_not_SF12")
  expect_error(SF12_domains("111111"), class = "SF6Dvalues_error_not_SF12")
  attr(sf12, "version") <- 1L
  expect_error(SF12_domains(sf12), class = "SF6Dvalues_error_version_1")
})
