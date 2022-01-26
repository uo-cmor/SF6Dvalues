stop_invalid_dimension_levels <- function(d, valid, actual) {
  msg <- glue::glue(
    "SF-6D {d} values should be in {{{usethis::ui_value(valid)}}}.\n",
    "{crayon::red(cli::symbol$cross)} `{d}` contains ",
    "{{{usethis::ui_value(sort(unique(actual)))}}}."
  )

  rlang::abort("SF6Dvalues_error_invalid_dimension_levels", message = msg,
               d = d, valid = valid, actual = actual)
}
