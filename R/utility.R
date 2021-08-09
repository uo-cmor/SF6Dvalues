#' SF-6D utility values
#'
#' Calculate SF-6D utility values from SF-6D profile (stored in an `SF6D`
#'     vector).
#'
#' @param x SF6D vector
#' @param values Which value set to use. Currently only 'uk' (Brazier & Roberts
#'     2004 algorithm) is implemented.
#'
#' @seealso \code{\link{sf6d_utility}} to calculate SF-6D utility values
#'     directly from SF-12/SF-36 responses; \code{\link{uk}} for the UK
#'     population value set.
#'
#' @export
utility <- function(x, values = "uk") {
  if (!is_SF6D(x)) {
    type <- if (is_SF12(x)) {
      "an SF12 vector"
    } else if (rlang::is_bare_list(x)) {
      "a list"
    } else if (rlang::is_bare_numeric(x)) {
      "a numeric vector"
    } else if (rlang::is_atomic(x)) {
      paste0("a ", typeof(x), " vector")
    } else paste0("a ", class(x)[[1]], " object")
    stop_not_SF6D(type)
  }
  if (!checkmate::test_choice(values, "uk")) stop_not_implemented("values", values)

  switch(values,
         uk = uk(x))
}


#' SF-6D utility values (direct calculation)
#'
#' Calculate SF-6D utility values from SF-12/SF-36 responses, using specified
#'     value set/algorithm.
#'
#' @param ... Vectors containing SF-12 question reponses. See
#'     \code{\link{SF12}} for details.
#' @param values Which value set to use. Currently only 'uk' (Brazier & Roberts
#'     2004 algorithm) is implemented.
#' @param questionnaire Which questionnaire ('SF-12' or 'SF-36') are the data
#'     obtained from. Only \code{questionnaire = 'SF-12'} is implemented at
#'     this stage.
#' @param version Which version (`1` or `2`) of the SF-12 are the data obtained
#'     from?
#'
#' @seealso \code{\link{utility}} to calculate SF-6D utility values from a
#'     constructed `SF6D` vector; \code{\link{uk}} for the UK population value
#'     set.
#'
#' @export
sf6d_utility <- function(..., values = "uk", questionnaire = "SF-12", version = 2L) {
  if (!checkmate::test_choice(values, "uk")) stop_not_implemented("values", values)
  if (!checkmate::test_choice(questionnaire, "SF-12"))
    stop_not_implemented("questionnaire", questionnaire)
  sf6d <- sf6d_profile(..., questionnaire = questionnaire, version = version)

  utility(sf6d, "uk")
}

#' SF-6D Brazier & Roberts UK population value set
#'
#' @param x SF6D vector
#'
#' @export
uk <- function(x) {
  PF <- extract(x, "PF")
  RL <- extract(x, "RL")
  SF <- extract(x, "SF")
  PAIN <- extract(x, "PAIN")
  MH <- extract(x, "MH")
  VIT <- extract(x, "VIT")
  MOST <- PF == 3 | RL >= 3 | SF >= 4 | PAIN >= 4 | MH >= 4 | VIT == 5

  value_set <- list(PF = c(0, 0, 0.045), RL = list(0, 0.063, 0.063, 0.063),
                    SF = c(0, 0.063, 0.066, 0.081, 0.093), PAIN = c(0, 0, 0.042, 0.077, 0.137),
                    MH = c(0, 0.059, 0.059, 0.113, 0.134), VIT = c(0, 0.078, 0.078, 0.078, 0.106),
                    MOST = 0.077)
  calculate_linear_sf6d_values(PF, RL, SF, PAIN, MH, VIT, MOST, value_set)
}

calculate_linear_sf6d_values <- function(PF, RL, SF, PAIN, MH, VIT, MOST = NULL, values) {
  tbl <- dplyr::tibble(
    PF = PF, RL = RL, SF = SF, PAIN = PAIN, MH = MH, VIT = VIT,
    MOST = {{MOST}} %||% 0,
    utility = 1 - (
      dplyr::recode(PF, !!!values$PF) +
        dplyr::recode(RL, !!!values$RL) +
        dplyr::recode(SF, !!!values$SF) +
        dplyr::recode(PAIN, !!!values$PAIN) +
        dplyr::recode(MH, !!!values$MH) +
        dplyr::recode(VIT, !!!values$VIT) +
        MOST * values$MOST
    )
  )
  tbl$utility
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
