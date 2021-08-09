stop_invalid_dimension_levels <- function(d, valid, actual) {
  rlang::abort(class = "SF6Dvalues_error_invalid_dimension_levels",
               d = d, valid = valid, actual = actual)
}

#' @export
conditionMessage.SF6Dvalues_error_invalid_dimension_levels <- function(c) {
  glue::glue_data(
    c,
    "SF-6D {d} values should be in {{{usethis::ui_value(valid)}}}.\n",
    "{crayon::red(cli::symbol$cross)} `{d}` contains ",
    "{{{usethis::ui_value(sort(unique(actual)))}}}."
  )
}
