test_that("SF36_scores works", {
  responses <- list(
    Q1 = 1:5, Q2 = 1:5, Q3a = c(1,1,1,1,1), Q3b = c(2,2,2,2,2), Q3c = c(3,3,3,3,3),
    Q3d = c(1,2,3,2,1), Q3e = c(3,2,1,2,3), Q3f = c(1,3,2,3,1), Q3g = c(1,1,2,2,3),
    Q3h = c(1,2,3,2,2), Q3i = c(2,2,2,1,1), Q3j = c(3,3,2,3,1), Q4a = c(1,1,1,1,1),
    Q4b = c(2,2,2,2,2), Q4c = c(1,2,2,2,2), Q4d = c(1,1,2,2,2), Q5a = c(1,1,1,1,1),
    Q5b = c(2,2,2,2,2), Q5c = c(1,1,1,2,2), Q6 = 1:5, Q7 = c(1,1,1,5,6), Q8 = c(1,2,3,1,5),
    Q9a = 1:5, Q9b = c(4,4,4,4,5), Q9c = c(3,3,4,5,5), Q9d = c(3,3,2,2,1), Q9e = c(2,2,4,4,5),
    Q9f = 1:5, Q9g = c(3,3,2,2,1), Q9h = 5:1, Q9i = c(4,3,2,1,1), Q10 = c(4,4,3,3,2), Q11a = 1:5,
    Q11b = 5:1, Q11c = c(2,3,3,4,5), Q11d = c(1,2,3,4,4)
  )
  sf36 <- rlang::exec(SF36, !!!responses)
  scores <- SF36_scores(sf36)

  expect_equal(scores$PF, c(40, 55, 55, 55, 40))
  expect_equal(scores$RP, c(25, 50, 75, 75, 75))
  expect_equal(scores$BP, c(100, 80, 70, 52, 0))
  expect_equal(scores$GH, c(45, 52, 52, 55, 65))
  expect_equal(scores$VT, c(70, 60, 35, 25, 10))
  expect_equal(scores$SF, c(87.5, 75, 50, 37.5, 12.5))
  expect_equal(scores$RE, c(100/3, 100/3, 100/3, 200/3, 200/3))
  expect_equal(scores$MH, c(36, 44, 60, 72, 88))
})

test_that("SF36_scores works with missing values", {
  responses <- list(
    Q1 = c(NA,2,3,4,5), Q2 = 1:5, Q3a = c(NA,1,NA,1,1), Q3b = c(NA,2,2,NA,2), Q3c = c(NA,3,3,3,3),
    Q3d = c(NA,2,3,2,1), Q3e = c(NA,2,1,2,3), Q3f = c(NA,3,2,NA,NA), Q3g = c(NA,1,2,NA,NA),
    Q3h = c(NA,2,3,2,NA), Q3i = c(NA,2,2,1,NA), Q3j = c(NA,3,2,3,NA), Q4a = c(1,NA,NA,NA,1),
    Q4b = c(2,NA,NA,NA,2), Q4c = c(1,NA,2,2,2), Q4d = c(1,NA,2,NA,NA), Q5a = c(NA,NA,1,1,1),
    Q5b = c(NA,NA,2,2,2), Q5c = c(NA,1,1,NA,2), Q6 = c(NA,2,3,4,5), Q7 = c(NA,1,1,5,NA),
    Q8 = c(NA,2,NA,1,5), Q9a = c(NA,2,3,4,5), Q9b = c(NA,4,4,4,5), Q9c = c(NA,3,4,5,NA),
    Q9d = c(NA,NA,2,2,1), Q9e = c(NA,2,4,4,5), Q9f = c(NA,NA,3,4,NA), Q9g = c(NA,3,2,2,1),
    Q9h = c(NA,NA,3,2,1), Q9i = c(NA,3,2,1,NA), Q10 = c(NA,NA,3,3,2), Q11a = c(NA,2,NA,4,5),
    Q11b = c(NA,4,3,2,1), Q11c = c(NA,3,NA,4,NA), Q11d = c(NA,2,3,4,4)
  )
  sf36 <- rlang::exec(SF36, !!!responses)
  scores <- SF36_scores(sf36)

  expect_equal(scores$PF, c(NA, 55, 61.111111, 50, 50))
  expect_equal(scores$RP, c(25, NA, 100, NA, 66.666667))
  expect_equal(scores$BP, c(NA, 80, 100, 52, 0))
  expect_equal(scores$GH, c(NA, 52, 53.333333, 55, 56.25))
  expect_equal(scores$VT, c(NA, 60, 35, 25, 13.3333333))
  expect_equal(scores$SF, c(NA, 75, 50, 37.5, 12.5))
  expect_equal(scores$RE, c(NA, NA, 100/3, 50, 200/3))
  expect_equal(scores$MH, c(NA, NA, 60, 72, 93.333333))
})

test_that("SF36_scores gives appropriate error messages", {
  sf12 <- SF12(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
               Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 3:1, Q7 = 1:3)
  levels <- list(PF = 1:3, RL = 2:4, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
  sf6d <- rlang::exec(SF6D, !!!levels)

  expect_error(SF36_scores(sf6d), class = "SF6Dvalues_error_incorrect_type")
  expect_error(SF36_scores(sf12), class = "SF6Dvalues_error_incorrect_type")
  expect_error(SF36_scores(levels), class = "SF6Dvalues_error_incorrect_type")
  expect_error(SF36_scores(1), class = "SF6Dvalues_error_incorrect_type")
  expect_error(SF36_scores("111111"), class = "SF6Dvalues_error_incorrect_type")
})
