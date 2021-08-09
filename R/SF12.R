# Define the `SF12` S3 class
new_SF12 <- function(Q1 = integer(), Q2 = integer(), Q3 = integer(), Q4 = integer(), Q5 = integer(),
                     Q6 = integer(), Q7 = integer(), Q8 = integer(), Q9 = integer(),
                     Q10 = integer(), Q11 = integer(), Q12 = integer(), version = 2L) {
  for (arg in list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12)) {
    checkmate::assert_integer(arg)
  }
  checkmate::assert_choice(as.character(version), c("1", "2"))

  responses <- list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12)
  names(responses) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12")

  new_rcrd(responses, class = "SF6Dvalues_SF12", version = version)
}

#' `SF12` vector
#'
#' This collates SF-12 question responses into an S3 vector to allow nice print
#'     methods and type checking.
#'
#' @param ... Vectors containing SF-12 question reponses; should be named as
#'     \code{\{Q1, Q2, ..., Q12\}}, \code{\{Q1, Q2a, ..., Q7\}}, or lower case
#'     equivalents.
#' @param version Which version (1 or 2) of the SF-12 are the data obtained
#'     from?
#'
#' @export
SF12 <- function(..., version = 2L) {
  sf12_vars <- check_sf12(..., version = version)
  new_SF12(sf12_vars$Q1, sf12_vars$Q2, sf12_vars$Q3, sf12_vars$Q4, sf12_vars$Q5, sf12_vars$Q6,
           sf12_vars$Q7, sf12_vars$Q8, sf12_vars$Q9, sf12_vars$Q10, sf12_vars$Q11, sf12_vars$Q12,
           version)
}

#' Test if the object is an SF12 vector
#'
#' This function returns \code{TRUE} for SF12 vectors (or subclasses thereof)
#'     and \code{FALSE} for all other objects.
#'
#' @param x Object to test.
#'
#' @export
is_SF12 <- function(x) inherits(x, "SF6Dvalues_SF12")

#' Coerce an object to an SF12 vector
#'
#' Turns an existing object into an SF12-class vector. Currently only works for
#'     SF12 vectors -- i.e., doesn't do anything.
#'
#' @param x Object to coerce
#'
#' @export
as_SF12 <- function(x) vec_cast(x, new_SF12())

#' @export
format.SF6Dvalues_SF12 <- function(x, ...) {
  values <- vec_data(x)
  values[is.na(values)] <- cli::symbol$dot
  Reduce(paste0, values)
}

#' @export
vec_ptype_full.SF6Dvalues_SF12 <- function(x, ...) "SF12"

#' @export
vec_ptype2.SF6Dvalues_SF12.SF6Dvalues_SF12 <- function(x, y, ...) new_SF12()

#' @export
vec_cast.SF6Dvalues_SF12.SF6Dvalues_SF12 <- function(x, to, ...) x

# for compatability with the S4 system
methods::setOldClass(c("SF6Dvalues_SF12", "vctrs_vctr"))

check_sf12 <- function(..., version = 2) {
  if (!checkmate::test_choice(as.character(version), c("1", "2"))) stop_invalid_version(version)
  responses <- list(...)
  if (length(responses) == 0) {
    responses <- rep(list(integer()), 12)
    names(responses) <- paste0("Q", 1:12)
    return(responses)
  }
  questions <- toupper(names(responses))
  valid_names <- list(
    paste0("Q", 1:12), # Q1, Q2, ..., Q12
    c("Q1", "Q2A", "Q2B", "Q3A", "Q3B", "Q4A", "Q4B", "Q5", "Q6A", "Q6B", "Q6C", "Q7")
  )
  if (length(questions) == 0) {
    if (length(responses) == 12) {
      warn_unnamed_questions()
      names(responses) <- valid_names[[1]]
      return(extract_SF12_integers(responses, version))
    }
    stop_unnamed_questions(length(responses))
  }
  if (checkmate::test_names(questions, must.include = valid_names[[1]], type = "unique")) {
    if (any(!questions %in% c(valid_names[[1]])))
      warn_too_many_questions(questions, valid_names[[1]])
    names(responses) <- questions
    return(extract_SF12_integers(responses[paste0("Q", 1:12)], version))
  }
  if (checkmate::test_names(questions, must.include = valid_names[[2]], type = "unique")) {
    if (any(!questions %in% c(valid_names[[2]])))
      warn_too_many_questions(questions, valid_names[[2]])
    names(responses) <- recode_SF12_labels(questions)
    return(extract_SF12_integers(responses[paste0("Q", 1:12)], version))
  }
  stop_invalid_questions(names(responses))
}

recode_SF12_labels <- function(nm) {
  nm <- toupper(nm)
  dplyr::recode(nm,
                Q1 = "Q1", Q2A = "Q2", Q2B = "Q3", Q3A = "Q4", Q3B = "Q5", Q4A = "Q6", Q4B = "Q7",
                Q5 = "Q8", Q6A = "Q9", Q6B = "Q10", Q6C = "Q11", Q7 = "Q12")
}

extract_SF12_integers <- function(responses, version) {
  numeric_responses <- purrr::map_lgl(responses, checkmate::test_integerish)
  character_responses <- purrr::map_lgl(responses, is.character)
  if (any(numeric_responses)) {
    if (!all(numeric_responses))
      stop_invalid_responses("integer", which.min(numeric_responses),
                             typeof(responses[[which.min(numeric_responses)]]))
    validate_SF12_values(responses, "numeric", version)
    return(purrr::map(responses, as.integer))
  }
  if (any(character_responses)) {
    if (!all(character_responses))
      stop_invalid_responses("character", which.min(character_responses),
                             typeof(responses[[which.min(character_responses)]]))
    validate_SF12_values(responses, "character", version)
    return(purrr::map2(responses, sf12_response_options("character", version),
                       ~as.integer(factor(.x, levels = .y))))
  }
}

validate_SF12_values <- function(responses, format, version) {
  valid_values <- sf12_response_options(format, version)
  for (q in seq_along(responses)) {
    if (!all(responses[[q]][!is.na(responses[[q]])] %in% valid_values[[q]]))
      stop_invalid_response_levels(q, valid_values[[q]], responses[[q]])
  }
  invisible(responses)
}

sf12_response_options <- function(format, version) {
  checkmate::assert_choice(format, c("character", "numeric"))
  checkmate::assert_choice(as.character(version), c("1", "2"))
  text_responses <- switch(
    as.integer(version),
    list(
      Q1 = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
      Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
      Q4 = c("Yes", "No"),
      Q5 = c("Yes", "No"),
      Q6 = c("Yes", "No"),
      Q7 = c("Yes", "No"),
      Q8 = c("Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"),
      Q9 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
      Q10 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
      Q11 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
      Q12 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time")
    ),
    list(
      Q1 = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
      Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
      Q4 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
      Q5 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
      Q6 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
      Q7 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
      Q8 = c("Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"),
      Q9 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
      Q10 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
      Q11 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
      Q12 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time")
    )
  )
  if (format == "character") return(text_responses)
  purrr::map(text_responses, seq_along)
}
