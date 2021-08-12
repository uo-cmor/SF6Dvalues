test_that("SF6D() returns zero-length `SF6D` vector", {
  x <- SF6D()
  expect_s3_class(x, "SF6Dvalues_SF6D")
  expect_length(x, 0)
  expect_equal(format(x), character())
  expect_output(print(x), "<SF6D[0]>", fixed = TRUE)
})

test_that("is_SF6D works as expected", {
  expect_false(is_SF6D(234))
  expect_false(is_SF6D(list(PF = 1:3, RL = 1:3, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)))
  expect_true(is_SF6D(SF6D(PF = 1:3, RL = 1:3, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)))
})

test_that("SF6D works as expected", {
  levels <- list(PF = 1:3, RL = 2:4, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
  sf6d <- rlang::exec(SF6D, !!!levels)

  expect_equal(field(sf6d, "PF"), 1:3)
  expect_equal(field(sf6d, "RL"), 2:4)
  expect_equal(attr(sf6d, "version"), "SF-12")
  expect_s3_class(sf6d, "SF6Dvalues_SF6D")
})

test_that("SF6D works for SF-36 version", {
  levels <- list(PF = 4:6, RL = 2:4, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
  sf6d <- rlang::exec(SF6D, !!!levels, version = "SF-36")

  expect_equal(field(sf6d, "PF"), 4:6)
  expect_equal(field(sf6d, "RL"), 2:4)
  expect_equal(attr(sf6d, "version"), "SF-36")
  expect_s3_class(sf6d, "SF6Dvalues_SF6D")
})

test_that("format.SF6Dvalues_SF6D works as expected", {
  levels <- list(PF = 3, RL = 1, SF = 1, PAIN = 1, MH = 1, VIT = 1)
  sf6d <- rlang::exec(SF6D, !!!levels)

  expect_match(format(sf6d), "31111")
})

test_that("[<- can subset SF6D vectors", {
  levels <- list(PF = 1:2, RL = 1:2, SF = 1:2, PAIN = 1:2, MH = 1:2, VIT = 1:2)
  sf6d <- rlang::exec(SF6D, !!!levels)
  my_sf6d <- SF6D(PF = 3, RL = 3, SF = 3, PAIN = 3, MH = 3, VIT = 3)
  sf6d[2] <- my_sf6d
  sf6d_1 <- SF6D(PF = 1, RL = 1, SF = 1, PAIN = 1, MH = 1, VIT = 1)

  expect_equal(sf6d[1], sf6d_1)
  expect_equal(sf6d[2], my_sf6d)
})

test_that("c.SF6Dvalues_SF6D works as expected", {
  levels <- list(PF = 1:2, RL = 1:2, SF = 1:2, PAIN = 1:2, MH = 1:2, VIT = 1:2)
  sf6d <- rlang::exec(SF6D, !!!levels)
  sf6d_1 <- SF6D(PF = 1, RL = 1, SF = 1, PAIN = 1, MH = 1, VIT = 1)
  sf6d_2 <- SF6D(PF = 2, RL = 2, SF = 2, PAIN = 2, MH = 2, VIT = 2)
  expect_equal(c(sf6d_1, sf6d_2), sf6d)
})

test_that("length.SF6Dvalues_SF6D works as expected", {
  levels <- list(PF = 1:2, RL = 1:2, SF = 1:2, PAIN = 1:2, MH = 1:2, VIT = 1:2)
  sf6d <- rlang::exec(SF6D, !!!levels)
  expect_length(sf6d, 2)
})

test_that("SF6D handles missing values", {
  levels <- list(PF = 1:2, RL = c(NA, NA), SF = 1:2, PAIN = 1:2, MH = c(1, NA), VIT = 1:2)
  sf6d <- rlang::exec(SF6D, !!!levels)
  expect_s3_class(sf6d, "SF6Dvalues_SF6D")
  expect_equal(format(sf6d), c("1.1111", "2.22.2"))
})

test_that("as_SF6D works as expected", {
  sf6d <- SF6D(PF = 3:1, RL = rep(4, 3), SF = 5:3, PAIN = 1:3, MH = 5:3, VIT = 1:3)
  sf12 <- SF12(Q1 = 1:3, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
               Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)
  expect_equal(as_SF6D(sf6d), sf6d)
  expect_error(as_SF6D(1), class = "vctrs_error_incompatible_type")
  expect_equal(as_SF6D(sf12), sf6d)
})

test_that("vec_ptype_full.SF6Dvalues_SF6D works", {
  expect_equal(
    vec_ptype_full(SF6D(PF = 1:3, RL = 1:3, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)),
    "SF6D"
  )
})

## Test appropriate errors generated
test_that("SF6D generates appropriate errors", {
  expect_error(SF6D(version = "x"), class = "SF6Dvalues_error_invalid_version")
  expect_error(
    SF6D(PF = 4, RL = 4, SF = 4, PAIN = 4, MH = 4, VIT = 5),
    class = "SF6Dvalues_error_invalid_dimension_levels"
  )
})
## Test version 2 works
