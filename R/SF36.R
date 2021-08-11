# Define the `SF36` S3 class
new_SF36 <- function(Q1 = integer(), Q2 = integer(), Q3 = integer(), Q4 = integer(), Q5 = integer(),
                     Q6 = integer(), Q7 = integer(), Q8 = integer(), Q9 = integer(),
                     Q10 = integer(), Q11 = integer(), Q12 = integer(), Q13 = integer(),
                     Q14 = integer(), Q15 = integer(), Q16 = integer(), Q17 = integer(),
                     Q18 = integer(), Q19 = integer(), Q20 = integer(), Q21 = integer(),
                     Q22 = integer(), Q23 = integer(), Q24 = integer(), Q25 = integer(),
                     Q26 = integer(), Q27 = integer(), Q28 = integer(), Q29 = integer(),
                     Q30 = integer(), Q31 = integer(), Q32 = integer(), Q33 = integer(),
                     Q34 = integer(), Q35 = integer(), Q36 = integer()) {
  for (arg in list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18,
                   Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28, Q29, Q30, Q31, Q32, Q33, Q34,
                   Q35, Q36)) {
    checkmate::assert_integer(arg)
  }

  responses <- list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18,
                    Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28, Q29, Q30, Q31, Q32, Q33, Q34,
                    Q35, Q36)
  names(responses) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12",
                        "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23",
                        "Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33", "Q34",
                        "Q35", "Q36")

  new_rcrd(responses, class = "SF6Dvalues_SF36")
}

#' `SF36` vector
#'
#' This collates SF-36 question responses into an S3 vector to allow nice print
#'     methods and type checking.
#'
#' @param ... Vectors containing SF-36 question reponses; should be named as
#'     \code{\{Q1, Q2, ..., Q36\}}, \code{\{Q1, Q2, Q3a, ..., Q11d7\}}, or
#'     lower case equivalents.
#'
#' @export
SF36 <- function(...) {
  sf36_vars <- check_sf36(...)
  new_SF36(sf36_vars$Q1, sf36_vars$Q2, sf36_vars$Q3, sf36_vars$Q4, sf36_vars$Q5, sf36_vars$Q6,
           sf36_vars$Q7, sf36_vars$Q8, sf36_vars$Q9, sf36_vars$Q10, sf36_vars$Q11, sf36_vars$Q12,
           sf36_vars$Q13, sf36_vars$Q14, sf36_vars$Q15, sf36_vars$Q16, sf36_vars$Q17, sf36_vars$Q18,
           sf36_vars$Q19, sf36_vars$Q20, sf36_vars$Q21, sf36_vars$Q22, sf36_vars$Q23, sf36_vars$Q24,
           sf36_vars$Q25, sf36_vars$Q26, sf36_vars$Q27, sf36_vars$Q28, sf36_vars$Q29, sf36_vars$Q30,
           sf36_vars$Q31, sf36_vars$Q32, sf36_vars$Q33, sf36_vars$Q34, sf36_vars$Q35, sf36_vars$Q36)
}

#' Test if the object is an SF36 vector
#'
#' This function returns \code{TRUE} for SF36 vectors (or subclasses thereof)
#'     and \code{FALSE} for all other objects.
#'
#' @param x Object to test.
#'
#' @export
is_SF36 <- function(x) inherits(x, "SF6Dvalues_SF36")

#' Coerce an object to an SF36 vector
#'
#' Turns an existing object into an SF36-class vector. Currently only works for
#'     SF36 vectors -- i.e., doesn't do anything.
#'
#' @param x Object to coerce
#'
#' @export
as_SF36 <- function(x) vec_cast(x, new_SF36())

#' @export
format.SF6Dvalues_SF36 <- function(x, ...) {
  values <- vec_data(x)
  values[is.na(values)] <- cli::symbol$dot
  Reduce(paste0, values)
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.SF6Dvalues_SF36 <- function(x, ...) {
  out <- format(x)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 3)
}

#' @export
vec_ptype_full.SF6Dvalues_SF36 <- function(x, ...) "SF36"

#' @export
vec_ptype2.SF6Dvalues_SF36.SF6Dvalues_SF36 <- function(x, y, ...) new_SF36()

#' @export
vec_cast.SF6Dvalues_SF36.SF6Dvalues_SF36 <- function(x, to, ...) x

# for compatability with the S4 system
methods::setOldClass(c("SF6Dvalues_SF36", "vctrs_vctr"))

check_sf36 <- function(...) {
  responses <- list(...)
  if (length(responses) == 0) {
    responses <- rep(list(integer()), 36)
    names(responses) <- paste0("Q", 1:36)
    return(responses)
  }
  questions <- toupper(names(responses))
  valid_names <- list(
    paste0("Q", 1:36),
    c("Q1", "Q2", "Q3A", "Q3B", "Q3C", "Q3D", "Q3E", "Q3F", "Q3G", "Q3H", "Q3I", "Q3J", "Q4A",
      "Q4B", "Q4C", "Q4D", "Q5A", "Q5B", "Q5C", "Q6", "Q7", "Q8", "Q9A", "Q9B", "Q9C", "Q9D", "Q9E",
      "Q9F", "Q9G", "Q9H", "Q9I", "Q10", "Q11A", "Q11B", "Q11C", "Q11D")
  )
  if (length(questions) == 0) {
    if (length(responses) == 36) {
      warn_unnamed_questions()
      names(responses) <- valid_names[[1]]
      return(extract_SF36_integers(responses))
    }
    stop_unnamed_questions(length(responses))
  }
  if (checkmate::test_names(questions, must.include = valid_names[[1]], type = "unique")) {
    if (any(!questions %in% c(valid_names[[1]])))
      warn_too_many_questions(questions, valid_names[[1]])
    names(responses) <- questions
    return(extract_SF36_integers(responses[paste0("Q", 1:36)]))
  }
  if (checkmate::test_names(questions, must.include = valid_names[[2]], type = "unique")) {
    if (any(!questions %in% c(valid_names[[2]])))
      warn_too_many_questions(questions, valid_names[[2]])
    names(responses) <- recode_SF36_labels(questions)
    return(extract_SF36_integers(responses[paste0("Q", 1:36)]))
  }
  stop_invalid_questions(names(responses))
}

recode_SF36_labels <- function(nm) {
  nm <- toupper(nm)
  dplyr::recode(nm,
                Q1 = "Q1", Q2 = "Q2", Q3A = "Q3", Q3B = "Q4", Q3C = "Q5", Q3D = "Q6", Q3E = "Q7",
                Q3F = "Q8", Q3G = "Q9", Q3H = "Q10", Q3I = "Q11", Q3J = "Q12", Q4A = "Q13",
                Q4B = "Q14", Q4C = "Q15", Q4D = "Q16", Q5A = "Q17", Q5B = "Q18", Q5C = "Q19",
                Q6 = "Q20", Q7 = "Q21", Q8 = "Q22", Q9A = "Q23", Q9B = "Q24", Q9C = "Q25",
                Q9D = "Q26", Q9E = "Q27", Q9F = "Q28", Q9G = "Q29", Q9H = "Q30", Q9I = "Q31",
                Q10 = "Q32", Q11A = "Q33", Q11B = "Q34", Q11C = "Q35", Q11D = "Q36")
}

extract_SF36_integers <- function(responses) {
  numeric_responses <- purrr::map_lgl(responses, checkmate::test_integerish)
  character_responses <- purrr::map_lgl(responses, is.character)
  if (any(numeric_responses)) {
    if (!all(numeric_responses))
      stop_invalid_responses("integer", which.min(numeric_responses),
                             typeof(responses[[which.min(numeric_responses)]]))
    validate_SF36_values(responses, "numeric")
    return(purrr::map(responses, as.integer))
  }
  if (any(character_responses)) {
    if (!all(character_responses))
      stop_invalid_responses("character", which.min(character_responses),
                             typeof(responses[[which.min(character_responses)]]))
    validate_SF36_values(responses, "character")
    return(purrr::map2(responses, sf36_response_options("character"),
                       ~as.integer(factor(.x, levels = .y))))
  }
}

validate_SF36_values <- function(responses, format) {
  valid_values <- sf36_response_options(format)
  for (q in seq_along(responses)) {
    if (!all(responses[[q]][!is.na(responses[[q]])] %in% valid_values[[q]]))
      stop_invalid_response_levels(q, valid_values[[q]], responses[[q]])
  }
  invisible(responses)
}

sf36_response_options <- function(format) {
  checkmate::assert_choice(format, c("character", "numeric"))
  text_responses <- rep(
    list(
      c("Excellent", "Very good", "Good", "Fair", "Poor"),
      c("Much better now than one year ago", "Somewhat better now than one year ago",
        "About the same as one year ago", "Somewhat worse now than one year ago",
        "Much worse now than one year ago"),
      c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
      c("Yes", "No"),
      c("Not at all", "Slightly", "Moderately", "Quite a bit", "Extremely"),
      c("None", "Very mild", "Mild", "Moderate", "Severe", "Very severe"),
      c("Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"),
      c("All of the time", "Most of the time", "A good bit of the time", "Some of the time",
        "A little of the time", "None of the time"),
      c("All of the time", "Most of the time", "Some of the time", "A little of the time",
        "None of the time"),
      c("Definitely true", "Mostly true", "Don't know", "Mostly false", "Definitely false")
    ),
    c(1, 1, 10, 7, 1, 1, 1, 9, 1, 4)
  )
  names(text_responses) <- paste0("Q", 1:36)
  if (format == "character") return(text_responses)
  purrr::map(text_responses, seq_along)
}
