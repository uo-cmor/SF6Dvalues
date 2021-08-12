stop_invalid_version <- function(arg, version, values) {
  rlang::abort(class = "SF6Dvalues_error_invalid_version",
               arg = arg, version = version, values = values)
}

#' @export
conditionMessage.SF6Dvalues_error_invalid_version <- function(c) {
  glue::glue_data(
    c,
    "{usethis::ui_code(arg)} must be one of {usethis::ui_value(values)}.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {usethis::ui_value(version)}."
  )
}

warn_too_many_SF12_questions <- function(questions, valid) {
  rlang::warn(class = "SF6Dvalues_warning_too_many_SF12_questions",
              questions = questions, valid = valid)
}

#' @export
conditionMessage.SF6Dvalues_warning_too_many_SF12_questions <- function(c) {
  glue::glue_data(
    c,
    "Arguments {usethis::ui_code(questions[!questions %in% c(valid)])} ignored.\n",
    "Only SF-12 questions {usethis::ui_code(valid[1:2])}, ..., {usethis::ui_code(valid[[12]])}",
    " and optional {usethis::ui_code(c('version'))} should be provided."
  )
}

warn_too_many_SF36_questions <- function(questions, valid) {
  rlang::warn(class = "SF6Dvalues_warning_too_many_SF36_questions",
              questions = questions, valid = valid)
}

#' @export
conditionMessage.SF6Dvalues_warning_too_many_SF36_questions <- function(c) {
  glue::glue_data(
    c,
    "Arguments {usethis::ui_code(questions[!questions %in% c(valid)])} ignored.\n",
    "Only SF-36 questions {usethis::ui_code(valid[1:3])}, ..., {usethis::ui_code(valid[[12]])}",
    " and optional {usethis::ui_code(c('version'))} should be provided."
  )
}

stop_invalid_SF12_questions <- function(questions) {
  rlang::abort(class = "SF6Dvalues_error_invalid_SF12_questions", questions = questions)
}

#' @export
conditionMessage.SF6Dvalues_error_invalid_SF12_questions <- function(c) {
  glue::glue_data(
    c,
    "Either {usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q12')} or ",
    "{usethis::ui_code(c('Q1', 'Q2a'))}, ..., {usethis::ui_code('Q7')} must be supplied.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {usethis::ui_code(questions)}."
  )
}

stop_invalid_SF36_questions <- function(questions) {
  rlang::abort(class = "SF6Dvalues_error_invalid_SF36_questions", questions = questions)
}

#' @export
conditionMessage.SF6Dvalues_error_invalid_SF36_questions <- function(c) {
  glue::glue_data(
    c,
    "Either {usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q36')} or ",
    "{usethis::ui_code(c('Q1', 'Q2', 'Q3a'))}, ..., {usethis::ui_code('Q11d')} must be supplied.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {usethis::ui_code(questions)}."
  )
}

stop_invalid_responses <- function(questionnaire, should_be, which, is) {
  rlang::abort(class = "SF6Dvalues_error_invalid_responses",
               questionnaire = questionnaire, should_be = should_be, which = which, is = is)
}

#' @export
conditionMessage.SF6Dvalues_error_invalid_responses <- function(c) {
  glue::glue_data(
    c,
    "{questionnaire} response data should be supplied as {should_be} vectors.\n",
    "{crayon::red(cli::symbol$cross)} `Q{which}` is a {is} vector."
  )
}

stop_invalid_response_levels <- function(questionnaire, q, valid, actual) {
  rlang::abort(class = "SF6Dvalues_error_invalid_response_levels",
               questionnaire = questionnaire, q = q, valid = valid, actual = actual)
}

#' @export
conditionMessage.SF6Dvalues_error_invalid_response_levels <- function(c) {
  glue::glue_data(
    c,
    "Responses for {questionnaire} Q{q} should be in {{{usethis::ui_value(valid)}}}.\n",
    "{crayon::red(cli::symbol$cross)} `Q{q}` contains ",
    "{{{usethis::ui_value(sort(unique(actual)))}}}."
  )
}

warn_unnamed_SF12_questions <- function() {
  rlang::warn(class = "SF6Dvalues_warning_unnamed_SF12_questions")
}

#' @export
conditionMessage.SF6Dvalues_warning_unnamed_SF12_questions <- function(c) {
  glue::glue(
    "SF-12 question arguments are unnamed.\n",
    "{crayon::yellow(cli::symbol$info)} These are assumed to represent ",
    "{usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q12')} (in the correct order).\n",
    "{crayon::red(cli::symbol$bullet)} Provide named arguments to silence this warning message."
  )
}

stop_unnamed_SF12_questions <- function(n) {
  rlang::abort(class = "SF6Dvalues_error_unnamed_SF12_questions", n = n)
}

#' @export
conditionMessage.SF6Dvalues_error_unnamed_SF12_questions <- function(c) {
  glue::glue_data(
    c,
    "SF-12 question arguments are unnamed and cannot be matched to ",
    "{usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q12')}.\n",
    "{crayon::red(cli::symbol$cross)} {usethis::ui_value(n)} arguments provided, ",
    "{usethis::ui_value(12)} needed.\n",
    "{crayon::red(cli::symbol$bullet)} Use named arguments for all SF-12 questions."
  )
}

warn_unnamed_SF36_questions <- function() {
  rlang::warn(class = "SF6Dvalues_warning_unnamed_SF36_questions")
}

#' @export
conditionMessage.SF6Dvalues_warning_unnamed_SF36_questions <- function(c) {
  glue::glue(
    "SF-36 question arguments are unnamed.\n",
    "{crayon::yellow(cli::symbol$info)} These are assumed to represent ",
    "{usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q36')} (in the correct order).\n",
    "{crayon::red(cli::symbol$bullet)} Provide named arguments to silence this warning message."
  )
}

stop_unnamed_SF36_questions <- function(n) {
  rlang::abort(class = "SF6Dvalues_error_unnamed_SF36_questions", n = n)
}

#' @export
conditionMessage.SF6Dvalues_error_unnamed_SF36_questions <- function(c) {
  glue::glue_data(
    c,
    "SF-36 question arguments are unnamed and cannot be matched to ",
    "{usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q36')}.\n",
    "{crayon::red(cli::symbol$cross)} {usethis::ui_value(n)} arguments provided, ",
    "{usethis::ui_value(36)} needed.\n",
    "{crayon::red(cli::symbol$bullet)} Use named arguments for all SF-36 questions."
  )
}
