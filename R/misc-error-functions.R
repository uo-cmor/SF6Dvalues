stop_not_implemented <- function(arg, value) {
  msg <- glue::glue(
    "`{arg} = {usethis::ui_value(value)}` is not implemented in this version of SF6Dvalues."
  )

  rlang::abort("SF6Dvalues_error_not_implemented", message = msg, arg = arg, value = value)
}

stop_unknown_dimension <- function(d) {
  msg <- glue::glue(
    "{usethis::ui_value(d)} is not a valid SF-6D dimension.\n",
    "{crayon::red(cli::symbol$cross)} `dimension` must be one of ",
    "{{{usethis::ui_value(c('PF', 'RL', 'SF', 'PAIN', 'MH', 'VIT'))}}}."
  )

  rlang::abort("SF6Dvalues_error_unknown_dimension", message = msg, d = d)
}

stop_unknown_question <- function(d) {
  msg <- glue::glue(
    "{usethis::ui_value(d)} is not a valid SF-12 question label.\n",
    "{crayon::red(cli::symbol$cross)} `dimension` must be one of ",
    "{{{usethis::ui_value(c('Q1', 'Q2'))}, ..., {usethis::ui_value('Q12')}}}."
  )

  rlang::abort("SF6Dvalues_error_unknown_question", message = msg, d = d)
}

stop_not_SF6D <- function(type) {
  msg <- glue::glue(
    "`x` must be an SF6D vector.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {type}."
  )

  rlang::abort("SF6Dvalues_error_not_SF6D", message = msg, type = type)
}

stop_incorrect_type <- function(expected, actual) {
  msg <- glue::glue(
    "`x` must be {expected}.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {actual}."
  )

  rlang::abort("SF6Dvalues_error_incorrect_type", message = msg,
               expected = expected, actual = actual)
}

stop_version_1 <- function() {
  msg <- glue::glue(
    "Only version 2 scoring is currently implemented.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied an SF-12 version 1 vector."
  )

  rlang::abort("SF6Dvalues_error_version_1", message = msg)
}

stop_incompatible_values <- function(values, version) {
  msg <- glue::glue("The {usethis::ui_code(values)} value set is not available for the {version}.")

  rlang::abort("SF6Dvalues_error_incompatible_values", message = msg,
               values = values, version = version)
}

stop_incompatible_algorithm <- function(algorithm, from_to, instrument) {
  msg <- glue::glue(
    "The {usethis::ui_code(algorithm)} algorithm is not available for mapping {from_to} the ",
    "{instrument}."
  )

  rlang::abort("SF6Dvalues_error_incompatible_algorithm", message = msg,
               algorithm = algorithm, from_to = from_to, instrument = instrument)
}
