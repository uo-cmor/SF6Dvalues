responses <- list(Q1 = 1:2, Q2 = 1:2, Q3 = 1:2, Q4 = 1:2, Q5 = 1:2, Q6 = 1:2, Q7 = 1:2, Q8 = 1:2,
                  Q9 = 1:2, Q10 = 1:2, Q11 = 1:2, Q12 = 1:2, Q13 = 1:2, Q14 = 1:2, Q15 = 1:2,
                  Q16 = 1:2, Q17 = 1:2, Q18 = 1:2, Q19 = 1:2, Q20 = 1:2, Q21 = 1:2, Q22 = 1:2,
                  Q23 = 1:2, Q24 = 1:2, Q25 = 1:2, Q26 = 1:2, Q27 = 1:2, Q28 = 1:2, Q29 = 1:2,
                  Q30 = 1:2, Q31 = 1:2, Q32 = 1:2, Q33 = 1:2, Q34 = 1:2, Q35 = 1:2, Q36 = 1:2)

responses_alt <- list(Q1 = 1:2, Q2 = 1:2, Q3a = 1:2, Q3b = 1:2, Q3c = 1:2, Q3d = 1:2, Q3e = 1:2,
                      Q3f = 1:2, Q3g = 1:2, Q3h = 1:2, Q3i = 1:2, Q3j = 1:2, Q4a = 1:2, Q4b = 1:2,
                      Q4c = 1:2, Q4d = 1:2, Q5a = 1:2, Q5b = 1:2, Q5c = 1:2, Q6 = 1:2, Q7 = 1:2,
                      Q8 = 1:2, Q9a = 1:2, Q9b = 1:2, Q9c = 1:2, Q9d = 1:2, Q9e = 1:2, Q9f = 1:2,
                      Q9g = 1:2, Q9h = 1:2, Q9i = 1:2, Q10 = 1:2, Q11a = 1:2, Q11b = 1:2,
                      Q11c = 1:2, Q11d = 1:2)

responses_single <- list(Q1 = 4, Q2 = 1, Q3a = 1, Q3b = 1, Q3c = 2, Q3d = 1, Q3e = 1, Q3f = 3,
                         Q3g = 1, Q3h = 1, Q3i = 1, Q3j = 1, Q4a = 1, Q4b = 2, Q4c = 1, Q4d = 1,
                         Q5a = 1, Q5b = 1, Q5c = 1, Q6 = 5, Q7 = 1, Q8 = 1, Q9a = 1, Q9b = 1,
                         Q9c = 1, Q9d = 1, Q9e = 1, Q9f = 1, Q9g = 1, Q9h = 1, Q9i = 1, Q10 = 1,
                         Q11a = 1, Q11b = 1, Q11c = 1, Q11d = 1)

responses_single_text <- list(
  Q1 = "Fair", Q2 = "Much better now than one year ago", Q3 = "Yes, limited a lot",
  Q4 = "Yes, limited a lot", Q5 = "Yes, limited a little", Q6 = "Yes, limited a lot",
  Q7 = "Yes, limited a lot", Q8 = "No, not limited at all", Q9 = "Yes, limited a lot",
  Q10 = "Yes, limited a lot", Q11 = "Yes, limited a lot", Q12 = "Yes, limited a lot",
  Q13 = "Yes", Q14 = "No", Q15 = "Yes", Q16 = "Yes", Q17 = "Yes", Q18 = "Yes", Q19 = "Yes",
  Q20 = "Extremely", Q21 = "None", Q22 = "Not at all", Q23 = "All of the time",
  Q24 = "All of the time", Q25 = "All of the time", Q26 = "All of the time",
  Q27 = "All of the time", Q28 = "All of the time", Q29 = "All of the time",
  Q30 = "All of the time", Q31 = "All of the time", Q32 = "All of the time",
  Q33 = "Definitely true", Q34 = "Definitely true", Q35 = "Definitely true", Q36 = "Definitely true"
)

responses_miss <- list(Q1 = 1:2, Q2 = c(NA, NA), Q3 = 1:2, Q4 = 1:2, Q5 = c(1, NA), Q6 = 1:2,
                       Q7 = 1:2, Q8 = 1:2, Q9 = 1:2, Q10 = 1:2, Q11 = 1:2, Q12 = 1:2, Q13 = 1:2,
                       Q14 = 1:2, Q15 = 1:2, Q16 = 1:2, Q17 = 1:2, Q18 = 1:2, Q19 = 1:2, Q20 = 1:2,
                       Q21 = 1:2, Q22 = 1:2, Q23 = 1:2, Q24 = 1:2, Q25 = 1:2, Q26 = 1:2, Q27 = 1:2,
                       Q28 = 1:2, Q29 = 1:2, Q30 = 1:2, Q31 = 1:2, Q32 = 1:2, Q33 = 1:2, Q34 = 1:2,
                       Q35 = 1:2, Q36 = 1:2)

test_that("SF36() returns zero-length `SF36` vector", {
  x <- SF36()
  expect_s3_class(x, "SF6Dvalues_SF36")
  expect_length(x, 0)
  expect_equal(format(x), character())
  expect_output(print(x), "<SF36[0]>", fixed = TRUE)
})

test_that("is_SF36 works as expected", {
  expect_false(is_SF36(234))
  expect_false(is_SF36(responses))
  expect_true(is_SF36(rlang::exec(SF36, !!!responses)))
})

test_that("SF36 works as expected", {
  sf36 <- rlang::exec(SF36, !!!responses)
  sf36_alt <- rlang::exec(SF36, !!!responses_alt)

  expect_equal(field(sf36, "Q1"), 1:2)
  expect_equal(field(sf36, "Q2"), 1:2)
  expect_s3_class(sf36, "SF6Dvalues_SF36")
  expect_equal(sf36, sf36_alt)
})

test_that("format.SF6Dvalues_SF36 works as expected", {
  sf36 <- rlang::exec(SF36, !!!responses_single)

  expect_match(format(sf36), "411121131111121111151111111111111111")
})

test_that("SF36 handles character inputs", {
  sf36 <- rlang::exec(SF36, !!!responses_single_text)

  expect_s3_class(sf36, "SF6Dvalues_SF36")
  expect_equal(sf36, rlang::exec(SF36, !!!responses_single))
})

test_that("[<- can subset SF12 vectors", {
  sf36 <- rlang::exec(SF36, !!!responses)
  my_sf36 <- rlang::exec(SF36, !!!responses_single)
  sf36[2] <- my_sf36
  responses_1 <- purrr::map(responses, 1)
  sf36_1 <- rlang::exec(SF36, !!!responses_1)

  expect_equal(sf36[1], sf36_1)
  expect_equal(sf36[2], my_sf36)
})

test_that("c.SF6Dvalues_SF36 works as expected", {
  sf36 <- rlang::exec(SF36, !!!responses)
  responses_1 <- purrr::map(responses, 1)
  responses_2 <- purrr::map(responses, 2)
  sf36_1 <- rlang::exec(SF36, !!!responses_1)
  sf36_2 <- rlang::exec(SF36, !!!responses_2)
  expect_equal(c(sf36_1, sf36_2), sf36)
})

test_that("length.SF6Dvalues_SF36 works as expected", {
  sf36 <- rlang::exec(SF36, !!!responses)
  expect_length(sf36, 2)
})

test_that("SF36 handles missing values", {
  sf36 <- rlang::exec(SF36, !!!responses_miss)
  expect_s3_class(sf36, "SF6Dvalues_SF36")
  expect_equal(format(sf36), c("1.1111111111111111111111111111111111",
                               "2.22.2222222222222222222222222222222"))
})

test_that("as_SF36 works for SF36 vectors", {
  sf36 <- rlang::exec(SF36, !!!responses)
  expect_equal(as_SF36(sf36), sf36)
})

test_that("vec_ptype_full.SF6Dvalues_SF36 works", {
  expect_equal(vec_ptype_full(rlang::exec(SF36, !!!responses)), "SF36")
})

test_that("pillar_shaft.SF6Dvalues_SF36 works", {
  expect_snapshot_output(print(dplyr::tibble(sf36 = rlang::exec(SF36, !!!responses)), width = 12))
})

## Test appropriate errors generated
test_that("SF36 generates appropriate errors", {
  skip("SF-12 warnings still need to be changed for SF-36.")
  expect_warning(
    SF36(Q1 = 1, Q2 = 1, Q3 = 1, Q4 = 1, Q5 = 1, Q6 = 1, Q7 = 1, Q8 = 1, Q9 = 1, Q10 = 1, Q11 = 1,
         Q12 = 1, Q2a = 1),
    class = "SF6Dvalues_warning_too_many_questions"
  )
  expect_warning(
    SF12(Q1 = 1, Q2a = 1, Q2b = 1, Q3a = 1, Q3b = 1, Q4a = 1, Q4b = 1, Q5 = 1, Q6a = 1, Q6b = 1,
         Q6c = 1, Q7 = 1, Q2 = 1),
    class = "SF6Dvalues_warning_too_many_questions"
  )
  expect_error(
    SF12(Q1 = 1, Q2 = 1),
    class = "SF6Dvalues_error_invalid_questions"
  )
  expect_error(
    SF12(Q1 = 4, Q2 = 4, Q3 = 4, Q4 = 4, Q5 = 4, Q6 = 5, Q7 = 4, Q8 = 4, Q9 = 4, Q10 = 4, Q11 = 4,
         Q12 = 4),
    class = "SF6Dvalues_error_invalid_response_levels"
  )
  expect_error(
    SF12(Q1 = "OK", Q2a = "Yes, limited a lot", Q2b = "Yes, limited a lot",
         Q3a = "All of the time", Q3b = "All of the time", Q4a = "All of the time",
         Q4b = "All of the time", Q5 = "Not at all", Q6a = "All of the time",
         Q6b = "All of the time", Q6c = "All of the time", Q7 = "All of the time"),
    class = "SF6Dvalues_error_invalid_response_levels"
  )
  expect_error(
    SF12(Q1 = 4, Q2 = 3, Q3 = 3, Q4 = "x", Q5 = 4, Q6 = 5, Q7 = 4, Q8 = 4, Q9 = 4, Q10 = 4, Q11 = 4,
         Q12 = 4),
    class = "SF6Dvalues_error_invalid_responses"
  )
  expect_error(
    SF12(Q1 = "Good", Q2a = TRUE, Q2b = "Yes, limited a lot",
         Q3a = "All of the time", Q3b = "All of the time", Q4a = "All of the time",
         Q4b = "All of the time", Q5 = "Not at all", Q6a = "All of the time",
         Q6b = "All of the time", Q6c = "All of the time", Q7 = "All of the time"),
    class = "SF6Dvalues_error_invalid_responses"
  )
  expect_warning(
    SF12(4, 3, 3, 4, 4, 5, 4, 4, 4, 4, 4, 4),
    class = "SF6Dvalues_warning_unnamed_questions"
  )
  expect_error(
    SF12(4, 3, 3, 4, 4, 5, 4, 4, 4, 4),
    class = "SF6Dvalues_error_unnamed_questions"
  )
})
