test_that("SF12() returns zero-length `SF12` vector", {
  x <- SF12()
  expect_s3_class(x, "SF6Dvalues_SF12")
  expect_length(x, 0)
  expect_equal(format(x), character())
  expect_output(print(x), "<SF12[0]>", fixed = TRUE)
})

test_that("is_SF12 works as expected", {
  expect_false(is_SF12(234))
  expect_false(is_SF12(list(Q1 = 1:3, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
                            Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)))
  expect_true(is_SF12(SF12(Q1 = 1:3, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
                           Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)))
})

test_that("SF12 works as expected", {
  responses <- list(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
                    Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)
  sf12 <- rlang::exec(SF12, !!!responses, version = 2)

  expect_equal(field(sf12, "Q1"), 3:5)
  expect_equal(field(sf12, "Q2"), 1:3)
  expect_equal(attr(sf12, "version"), 2)
  expect_s3_class(sf12, "SF6Dvalues_SF12")
})

test_that("format.SF6Dvalues_SF12 works as expected", {
  responses <- list(Q1 = 3, Q2a = 1, Q2b = 1, Q3a = 1, Q3b = 1, Q4a = 1,
                    Q4b = 1, Q5 = 1, Q6a = 1, Q6b = 1, Q6c = 1, Q7 = 1)
  sf12 <- rlang::exec(SF12, !!!responses, version = 2)

  expect_match(format(sf12), "311111111111")
})

test_that("SF12 handles character inputs", {
  responses <- list(Q1 = "Good", Q2a = "Yes, limited a lot", Q2b = "Yes, limited a lot",
                    Q3a = "All of the time", Q3b = "All of the time", Q4a = "All of the time",
                    Q4b = "All of the time", Q5 = "Not at all", Q6a = "All of the time",
                    Q6b = "All of the time", Q6c = "All of the time", Q7 = "All of the time")
  sf12 <- rlang::exec(SF12, !!!responses, version = 2)

  expect_s3_class(sf12, "SF6Dvalues_SF12")
  expect_match(format(sf12), "311111111111")
})

test_that("[<- can subset SF12 vectors", {
  responses <- list(Q1 = 1:3, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
                    Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)
  sf12 <- rlang::exec(SF12, !!!responses, version = 2)
  my_sf12 <- SF12(Q1 = 4, Q2 = 1, Q3 = 1, Q4 = 4, Q5 = 4, Q6 = 4, Q7 = 4, Q8 = 4, Q9 = 4, Q10 = 4,
                  Q11 = 4, Q12 = 4)
  sf12[2] <- my_sf12
  sf12_1 <- SF12(Q1 = 1, Q2 = 1, Q3 = 1, Q4 = 1, Q5 = 1, Q6 = 1, Q7 = 1, Q8 = 1, Q9 = 1, Q10 = 1,
                 Q11 = 1, Q12 = 1)

  expect_equal(sf12[1], sf12_1)
  expect_equal(sf12[2], my_sf12)
})

test_that("c.SF6Dvalues_SF12 works as expected", {
  responses <- list(Q1 = 1:2, Q2a = 1:2, Q2b = 1:2, Q3a = 1:2, Q3b = 1:2, Q4a = 1:2,
                    Q4b = 1:2, Q5 = 1:2, Q6a = 1:2, Q6b = 1:2, Q6c = 1:2, Q7 = 1:2)
  sf12 <- rlang::exec(SF12, !!!responses, version = 2)
  sf12_1 <- SF12(Q1 = 1, Q2 = 1, Q3 = 1, Q4 = 1, Q5 = 1, Q6 = 1, Q7 = 1, Q8 = 1, Q9 = 1, Q10 = 1,
                 Q11 = 1, Q12 = 1)
  sf12_2 <- SF12(Q1 = 2, Q2 = 2, Q3 = 2, Q4 = 2, Q5 = 2, Q6 = 2, Q7 = 2, Q8 = 2, Q9 = 2, Q10 = 2,
                 Q11 = 2, Q12 = 2)
  expect_equal(c(sf12_1, sf12_2), sf12)
})

test_that("length.SF6Dvalues_SF12 works as expected", {
  responses <- list(Q1 = 1:2, Q2a = 1:2, Q2b = 1:2, Q3a = 1:2, Q3b = 1:2, Q4a = 1:2,
                    Q4b = 1:2, Q5 = 1:2, Q6a = 1:2, Q6b = 1:2, Q6c = 1:2, Q7 = 1:2)
  sf12 <- rlang::exec(SF12, !!!responses, version = 2)
  expect_length(sf12, 2)
})

test_that("SF12 handles missing values", {
  responses <- list(Q1 = 1:2, Q2a = c(NA, NA), Q2b = 1:2, Q3a = 1:2, Q3b = c(1, NA), Q4a = 1:2,
                    Q4b = 1:2, Q5 = 1:2, Q6a = 1:2, Q6b = 1:2, Q6c = 1:2, Q7 = 1:2)
  sf12 <- rlang::exec(SF12, !!!responses, version = 2)
  expect_s3_class(sf12, "SF6Dvalues_SF12")
  expect_equal(format(sf12), c("1.1111111111", "2.22.2222222"))
})

test_that("as_SF12 works for SF12 vectors", {
  sf12 <- SF12(Q1 = 1:3, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
               Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)
  expect_equal(as_SF12(sf12), sf12)
})

test_that("vec_ptype_full.SF6Dvalues_SF12 works", {
  expect_equal(
    vec_ptype_full(SF12(Q1 = 1:3, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
                        Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)),
    "SF12"
  )
})

## Test appropriate errors generated
test_that("SF12 generates appropriate errors", {
  expect_error(SF12(version = 3), class = "SF6Dvalues_error_invalid_version")
  expect_warning(
    SF12(Q1 = 1, Q2 = 1, Q3 = 1, Q4 = 1, Q5 = 1, Q6 = 1, Q7 = 1, Q8 = 1, Q9 = 1, Q10 = 1, Q11 = 1,
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
  ## Test version 2 works
