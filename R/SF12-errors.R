stop_invalid_version <- function(arg, version, values) {
  msg <- glue::glue(
    "{usethis::ui_code(arg)} must be one of {usethis::ui_value(values)}.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {usethis::ui_value(version)}."
  )
  rlang::abort("SF6Dvalues_error_invalid_version", message = msg,
               arg = arg, version = version, values = values)
}

warn_too_many_SF12_questions <- function(questions, valid) {
  msg <- glue::glue(
    "Arguments {usethis::ui_code(questions[!questions %in% c(valid)])} ignored.\n",
    "Only SF-12 questions {usethis::ui_code(valid[1:2])}, ..., {usethis::ui_code(valid[[12]])}",
    " and optional {usethis::ui_code(c('version'))} should be provided."
  )
  rlang::warn("SF6Dvalues_warning_too_many_SF12_questions", message = msg,
              questions = questions, valid = valid)
}

warn_too_many_SF36_questions <- function(questions, valid) {
  msg <- glue::glue(
    "Arguments {usethis::ui_code(questions[!questions %in% c(valid)])} ignored.\n",
    "Only SF-36 questions {usethis::ui_code(valid[1:3])}, ..., {usethis::ui_code(valid[[12]])}",
    " and optional {usethis::ui_code(c('version'))} should be provided."
  )

  rlang::warn("SF6Dvalues_warning_too_many_SF36_questions", message = msg,
              questions = questions, valid = valid)
}

stop_invalid_SF12_questions <- function(questions) {
  msg <- glue::glue(
    "Either {usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q12')} or ",
    "{usethis::ui_code(c('Q1', 'Q2a'))}, ..., {usethis::ui_code('Q7')} must be supplied.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {usethis::ui_code(questions)}."
  )

  rlang::abort("SF6Dvalues_error_invalid_SF12_questions", message = msg, questions = questions)
}

stop_invalid_SF36_questions <- function(questions) {
  msg <- glue::glue(
    "Either {usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q36')} or ",
    "{usethis::ui_code(c('Q1', 'Q2', 'Q3a'))}, ..., {usethis::ui_code('Q11d')} must be supplied.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {usethis::ui_code(questions)}."
  )

  rlang::abort("SF6Dvalues_error_invalid_SF36_questions", message = msg, questions = questions)
}

stop_invalid_responses <- function(questionnaire, should_be, which, is) {
  msg <- glue::glue(
    "{questionnaire} response data should be supplied as {should_be} vectors.\n",
    "{crayon::red(cli::symbol$cross)} `Q{which}` is a {is} vector."
  )

  rlang::abort("SF6Dvalues_error_invalid_responses", message = msg,
               questionnaire = questionnaire, should_be = should_be, which = which, is = is)
}

stop_invalid_response_levels <- function(questionnaire, q, valid, actual) {
  msg <- glue::glue(
    "Responses for {questionnaire} Q{q} should be in {{{usethis::ui_value(valid)}}}.\n",
    "{crayon::red(cli::symbol$cross)} `Q{q}` contains ",
    "{{{usethis::ui_value(sort(unique(actual)))}}}."
  )

  rlang::abort("SF6Dvalues_error_invalid_response_levels", message = msg,
               questionnaire = questionnaire, q = q, valid = valid, actual = actual)
}

stop_invalid_factor_levels <- function(questionnaire) {
  msg <- glue::glue("Invalid factor levels in {questionnaire} responses.")

  rlang::abort("SF6Dvalues_error_invalid_factor_levels", message = msg,
               questionnaire = questionnaire)
}

warn_invalid_factor_levels <- function(questionnaire) {
  msg <- glue::glue(
    "Unknown factor levels in {questionnaire} responses: using underlying numeric coding.\n",
    "{crayon::yellow(cli::symbol$info)} Please ensure that these codes are correct."
  )

  rlang::warn("SF6Dvalues_warning_invalid_factor_levels", message = msg,
              questionnaire = questionnaire)
}

warn_unnamed_SF12_questions <- function() {
  msg <- glue::glue(
    "SF-12 question arguments are unnamed.\n",
    "{crayon::yellow(cli::symbol$info)} These are assumed to represent ",
    "{usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q12')} (in the correct order).\n",
    "{crayon::red(cli::symbol$bullet)} Provide named arguments to silence this warning message."
  )

  rlang::warn("SF6Dvalues_warning_unnamed_SF12_questions", message = msg)
}

stop_unnamed_SF12_questions <- function(n) {
  msg <- glue::glue(
    "SF-12 question arguments are unnamed and cannot be matched to ",
    "{usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q12')}.\n",
    "{crayon::red(cli::symbol$cross)} {usethis::ui_value(n)} arguments provided, ",
    "{usethis::ui_value(12)} needed.\n",
    "{crayon::red(cli::symbol$bullet)} Use named arguments for all SF-12 questions."
  )

  rlang::abort("SF6Dvalues_error_unnamed_SF12_questions", message = msg, n = n)
}

warn_unnamed_SF36_questions <- function() {
  msg <- glue::glue(
    "SF-36 question arguments are unnamed.\n",
    "{crayon::yellow(cli::symbol$info)} These are assumed to represent ",
    "{usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q36')} (in the correct order).\n",
    "{crayon::red(cli::symbol$bullet)} Provide named arguments to silence this warning message."
  )

  rlang::warn("SF6Dvalues_warning_unnamed_SF36_questions", message = msg)
}

stop_unnamed_SF36_questions <- function(n) {
  msg <- glue::glue(
    "SF-36 question arguments are unnamed and cannot be matched to ",
    "{usethis::ui_code(c('Q1', 'Q2'))}, ..., {usethis::ui_code('Q36')}.\n",
    "{crayon::red(cli::symbol$cross)} {usethis::ui_value(n)} arguments provided, ",
    "{usethis::ui_value(36)} needed.\n",
    "{crayon::red(cli::symbol$bullet)} Use named arguments for all SF-36 questions."
  )

  rlang::abort("SF6Dvalues_error_unnamed_SF36_questions", message = msg, n = n)
}
