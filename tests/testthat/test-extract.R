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

  sf36 <- SF36(Q1 = 1:2, Q2 = 4:5, Q3 = 1:2, Q4 = 1:2, Q5 = 1:2, Q6 = 1:2, Q7 = 1:2, Q8 = 1:2,
               Q9 = 1:2, Q10 = 1:2, Q11 = 1:2, Q12 = 1:2, Q13 = 1:2, Q14 = 1:2, Q15 = 1:2,
               Q16 = 1:2, Q17 = 1:2, Q18 = 1:2, Q19 = 1:2, Q20 = 1:2, Q21 = 1:2, Q22 = 1:2,
               Q23 = 1:2, Q24 = 1:2, Q25 = 1:2, Q26 = 1:2, Q27 = 1:2, Q28 = 1:2, Q29 = 1:2,
               Q30 = 1:2, Q31 = 1:2, Q32 = 1:2, Q33 = 1:2, Q34 = 1:2, Q35 = 1:2, Q36 = 1:2)
  expect_equal(extract(sf36, "Q1"), 1:2)
  expect_equal(extract(sf36, "Q2"), 4:5)
  expect_error(extract(sf36, "XX"), class = "SF6Dvalues_error_unknown_question")

  expect_error(extract(1234, "a"), "no applicable method")
})
