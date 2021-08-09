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

