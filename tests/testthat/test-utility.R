levels <- list(PF = 1:3, RL = 2:4, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
sf6d <- rlang::exec(SF6D, !!!levels)

test_that("uk works", {
  expect_equal(uk(sf6d), c(0.937, 0.660, 0.570))
})

test_that("sf6d_utility works", {
  expect_equal(sf6d_utility(sf6d, "uk"), c(0.937, 0.660, 0.570))
})

test_that("sf6d_utility gives appropriate error messages", {
  expect_error(sf6d_utility(1), class = "SF6Dvalues_error_not_SF6D")
  expect_error(sf6d_utility(sf6d, "nz"), class = "SF6Dvalues_error_not_implemented")
})
