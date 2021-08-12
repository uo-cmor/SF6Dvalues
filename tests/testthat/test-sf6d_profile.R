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

## Test appropriate errors generated
test_that("sf6d_profile generates appropriate errors", {
  expect_error(sf6d_profile(questionnaire = "X"), class = "SF6Dvalues_error_invalid_version")
  expect_error(sf6d_profile(version = 3), class = "SF6Dvalues_error_invalid_version")
  expect_warning(
    sf6d_profile(Q1 = 1, Q2 = 1, Q3 = 1, Q4 = 1, Q5 = 1, Q6 = 1, Q7 = 1, Q8 = 1, Q9 = 1, Q10 = 1,
                 Q11 = 1, Q12 = 1, Q2a = 1),
    class = "SF6Dvalues_warning_too_many_SF12_questions"
  )
  expect_warning(
    sf6d_profile(Q1 = 1, Q2a = 1, Q2b = 1, Q3a = 1, Q3b = 1, Q4a = 1, Q4b = 1, Q5 = 1, Q6a = 1,
                 Q6b = 1, Q6c = 1, Q7 = 1, Q2 = 1),
    class = "SF6Dvalues_warning_too_many_SF12_questions"
  )
  expect_error(
    sf6d_profile(Q1 = 1, Q2 = 1),
    class = "SF6Dvalues_error_invalid_SF12_questions"
  )
  expect_error(
    sf6d_profile(Q1 = 4, Q2 = 4, Q3 = 4, Q4 = 4, Q5 = 4, Q6 = 5, Q7 = 4, Q8 = 4, Q9 = 4, Q10 = 4,
                 Q11 = 4, Q12 = 4),
    class = "SF6Dvalues_error_invalid_response_levels"
  )
  expect_error(
    sf6d_profile(Q1 = "OK", Q2a = "Yes, limited a lot", Q2b = "Yes, limited a lot",
                 Q3a = "All of the time", Q3b = "All of the time", Q4a = "All of the time",
                 Q4b = "All of the time", Q5 = "Not at all", Q6a = "All of the time",
                 Q6b = "All of the time", Q6c = "All of the time", Q7 = "All of the time"),
    class = "SF6Dvalues_error_invalid_response_levels"
  )
  expect_error(
    sf6d_profile(Q1 = 4, Q2 = 3, Q3 = 3, Q4 = "x", Q5 = 4, Q6 = 5, Q7 = 4, Q8 = 4, Q9 = 4, Q10 = 4,
                 Q11 = 4, Q12 = 4),
    class = "SF6Dvalues_error_invalid_responses"
  )
  expect_error(
    sf6d_profile(Q1 = "Good", Q2a = TRUE, Q2b = "Yes, limited a lot",
                 Q3a = "All of the time", Q3b = "All of the time", Q4a = "All of the time",
                 Q4b = "All of the time", Q5 = "Not at all", Q6a = "All of the time",
                 Q6b = "All of the time", Q6c = "All of the time", Q7 = "All of the time"),
    class = "SF6Dvalues_error_invalid_responses"
  )
  expect_warning(
    sf6d_profile(4, 3, 3, 4, 4, 5, 4, 4, 4, 4, 4, 4),
    class = "SF6Dvalues_warning_unnamed_SF12_questions"
  )
  expect_error(
    sf6d_profile(4, 3, 3, 4, 4, 5, 4, 4, 4, 4),
    class = "SF6Dvalues_error_unnamed_SF12_questions"
  )
})
