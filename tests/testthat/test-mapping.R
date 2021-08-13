levels <- list(PF = 1:3, RL = 2:4, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
sf6d <- rlang::exec(SF6D, !!!levels)

responses <- list(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
                  Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)
sf12 <- rlang::exec(SF12, !!!responses)
sf6d_sf12 <- as_SF6D(sf12)

responses_sf36 <- list(
  Q1 = 1:2, Q2 = 1:2, Q3 = 1:2, Q4 = 1:2, Q5 = 1:2, Q6 = 1:2, Q7 = 1:2, Q8 = 1:2, Q9 = 1:2,
  Q10 = 1:2, Q11 = 1:2, Q12 = 1:2, Q13 = 1:2, Q14 = 1:2, Q15 = 1:2, Q16 = 1:2, Q17 = 1:2, Q18 = 1:2,
  Q19 = 1:2, Q20 = 1:2, Q21 = 1:2, Q22 = 1:2, Q23 = 1:2, Q24 = 1:2, Q25 = 1:2, Q26 = 1:2, Q27 = 1:2,
  Q28 = 1:2, Q29 = 1:2, Q30 = 1:2, Q31 = 1:2, Q32 = 1:2, Q33 = 1:2, Q34 = 1:2, Q35 = 1:2, Q36 = 1:2
)
sf36 <- rlang::exec(SF36, !!!responses_sf36)
sf6d_sf36 <- as_SF6D(sf36)

test_that("gk works as expected", {
  expect_equal(gk(sf6d_sf36, instrument = "EQ-5D-5L"), c(0.62968, 0.68152))
})

test_that("rowen works as expected", {
  expect_equal(rowen(sf6d_sf36, instrument = "EQ-5D-3L"), c(0.237334, 0.287606))
  expect_equal(rowen(sf6d_sf36, instrument = "HUI2"), c(0.545818, 0.581562))
})

test_that("richardson works as expected", {
  expect_equal(richardson(sf6d_sf36, instrument = "EQ-5D-5L"), c(0.47856, 0.53104))
  expect_equal(richardson(sf6d_sf36, instrument = "HUI3"), c(0.41153846, 0.47307692))
  expect_equal(richardson(sf6d_sf36, instrument = "15D"), c(0.707407407, 0.737037037))
  expect_equal(richardson(sf6d_sf36, instrument = "QWB"), c(0.45393258, 0.48988764))
  expect_equal(richardson(sf6d_sf36, instrument = "AQoL-8D"), c(0.432786885, 0.485245902))
})

test_that("mapping works as expected", {
  expect_equal(mapping(sf6d_sf36, "EQ-5D-5L", "gk"), c(0.62968, 0.68152))
})

test_that("mapping gives appropriate error messages", {
  expect_error(mapping(sf12), class = "SF6Dvalues_error_not_SF6D")
  expect_error(mapping(sf36), class = "SF6Dvalues_error_not_SF6D")
  expect_error(mapping(levels), class = "SF6Dvalues_error_not_SF6D")
  expect_error(mapping(1), class = "SF6Dvalues_error_not_SF6D")
  expect_error(mapping("111111"), class = "SF6Dvalues_error_not_SF6D")

  expect_error(mapping(sf6d_sf36, "A"), class = "SF6Dvalues_error_not_implemented")
  expect_error(mapping(sf6d_sf36, "EQ-5D-5L", "rowen"),
               class = "SF6Dvalues_error_incompatible_algorithm")
  expect_error(mapping(sf6d_sf36, "EQ-5D-3L", "richardson"),
               class = "SF6Dvalues_error_incompatible_algorithm")
  expect_error(mapping(sf6d_sf36, "HUI2", "richardson"),
               class = "SF6Dvalues_error_incompatible_algorithm")
  expect_error(mapping(sf6d_sf36, "HUI3", "rowen"),
               class = "SF6Dvalues_error_incompatible_algorithm")
  expect_error(mapping(sf6d_sf36, "15D", "rowen"),
               class = "SF6Dvalues_error_incompatible_algorithm")
  expect_error(mapping(sf6d_sf36, "QWB", "rowen"),
               class = "SF6Dvalues_error_incompatible_algorithm")
  expect_error(mapping(sf6d_sf36, "AQoL-8D", "rowen"),
               class = "SF6Dvalues_error_incompatible_algorithm")
})

test_that("mapping algorithms give appropriate error messages", {
  expect_error(gk(sf6d_sf12), class = "SF6Dvalues_error_incompatible_algorithm")
  expect_error(rowen(sf6d_sf12), class = "SF6Dvalues_error_incompatible_algorithm")
  expect_error(richardson(sf6d_sf12), class = "SF6Dvalues_error_incompatible_algorithm")
})
