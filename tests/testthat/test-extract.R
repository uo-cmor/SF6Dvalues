test_that("extract works as expected", {
  sf6d <- SF6D(1:3, 4:2, 2:4, 2:4, 2:4, 2:4)
  expect_equal(extract(sf6d, "PF"), 1:3)
  expect_equal(extract(sf6d, "RL"), 4:2)
  expect_error(extract(sf6d, "XX"), class = "SF6Dvalues_error_unknown_dimension")

  sf12 <- SF12(Q1 = 2:4, Q2 = 1:3, Q3 = 1:3, Q4 = 2:4, Q5 = 2:4, Q6 = 2:4, Q7 = 2:4, Q8 = 2:4,
               Q9 = 2:4, Q10 = 2:4, Q11 = 2:4, Q12 = 2:4)
  expect_equal(extract(sf12, "Q1"), 2:4)
  expect_equal(extract(sf12, "Q2"), 1:3)
  expect_error(extract(sf12, "XX"), class = "SF6Dvalues_error_unknown_question")

  expect_error(extract(1234, "a"), "no applicable method")
})
