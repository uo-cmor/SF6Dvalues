stop_not_implemented <- function(arg, value) {
  rlang::abort(class = "SF6Dvalues_error_not_implemented", arg = arg, value = value)
}

#' @export
conditionMessage.SF6Dvalues_error_not_implemented <- function(c) {
  glue::glue_data(
    c,
    "`{arg} = {usethis::ui_value(value)}` is not implemented in this version ",
    "of SF6Dvalues."
  )
}

stop_unknown_dimension <- function(d) {
  rlang::abort(class = "SF6Dvalues_error_unknown_dimension", d = d)
}

#' @export
conditionMessage.SF6Dvalues_error_unknown_dimension <- function(c) {
  glue::glue_data(
    c,
    "{usethis::ui_value(d)} is not a valid SF-6D dimension.\n",
    "{crayon::red(cli::symbol$cross)} `dimension` must be one of ",
    "{{{usethis::ui_value(c('PF', 'RL', 'SF', 'PAIN', 'MH', 'VIT'))}}}."
  )
}

stop_unknown_question <- function(d) {
  rlang::abort(class = "SF6Dvalues_error_unknown_question", d = d)
}

#' @export
conditionMessage.SF6Dvalues_error_unknown_question <- function(c) {
  glue::glue_data(
    c,
    "{usethis::ui_value(d)} is not a valid SF-12 question label.\n",
    "{crayon::red(cli::symbol$cross)} `dimension` must be one of ",
    "{{{usethis::ui_value(c('Q1', 'Q2'))}, ..., {usethis::ui_value('Q12')}}}."
  )
}

stop_not_SF6D <- function(type) {
  rlang::abort(class = "SF6Dvalues_error_not_SF6D", type = type)
}

#' @export
conditionMessage.SF6Dvalues_error_not_SF6D <- function(c) {
  glue::glue_data(
    c,
    "`x` must be an SF6D vector.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {type}."
  )
}

stop_not_SF12 <- function(type) {
  rlang::abort(class = "SF6Dvalues_error_not_SF12", type = type)
}

#' @export
conditionMessage.SF6Dvalues_error_not_SF12 <- function(c) {
  glue::glue_data(
    c,
    "`x` must be an SF12 vector.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied {type}."
  )
}

stop_version_1 <- function() {
  rlang::abort(class = "SF6Dvalues_error_version_1")
}

#' @export
conditionMessage.SF6Dvalues_error_version_1 <- function(c) {
  glue::glue(
    "Only version 2 scoring is currently implemented.\n",
    "{crayon::red(cli::symbol$cross)} You've supplied an SF-12 version 1 ",
      "vector."
  )
}

stop_incompatible_values <- function(values, version) {
  rlang::abort(class = "SF6Dvalues_error_incompatible_values", values = values, version = version)
}

#' @export
conditionMessage.SF6Dvalues_error_incompatible_values <- function(c) {
  glue::glue_data(c,
                  "The {usethis::ui_code(values)} value set is not available for the {version}.\n")
}
