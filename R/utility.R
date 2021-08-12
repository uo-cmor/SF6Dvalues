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
  if (!checkmate::test_choice(values, c("uk", "oz"))) stop_not_implemented("values", values)

  switch(values,
         uk = uk(x),
         oz = oz(x))
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
  if (!checkmate::test_choice(values, c("uk", "oz"))) stop_not_implemented("values", values)
  if (!checkmate::test_choice(questionnaire, c("SF-12", "SF-36")))
    stop_invalid_version("questionnaire", questionnaire, c("SF-12", "SF-36"))
  sf6d <- sf6d_profile(..., questionnaire = questionnaire, version = version)

  utility(sf6d, values)
}

#' SF-6D Brazier & Roberts UK population value set
#'
#' @param x SF6D vector
#'
#' @export
uk <- function(x) {
  version <- attr(x, "version")

  PF <- extract(x, "PF")
  RL <- extract(x, "RL")
  SF <- extract(x, "SF")
  PAIN <- extract(x, "PAIN")
  MH <- extract(x, "MH")
  VIT <- extract(x, "VIT")
  MOST <- switch(version,
                 "SF-12" = PF == 3 | RL >= 3 | SF >= 4 | PAIN >= 4 | MH >= 4 | VIT == 5,
                 "SF-36" = PF >= 4 | RL >= 3 | SF >= 4 | PAIN >= 5 | MH >= 4 | VIT >= 4)

  value_set <- switch(
    version,
    "SF-12" = list(PF = c(0, 0, 0.045), RL = list(0, 0.063, 0.063, 0.063),
                   SF = c(0, 0.063, 0.066, 0.081, 0.093), PAIN = c(0, 0, 0.042, 0.077, 0.137),
                   MH = c(0, 0.059, 0.059, 0.113, 0.134), VIT = c(0, 0.078, 0.078, 0.078, 0.106),
                   MOST = 0.077),
    "SF-36" = list(PF = c(0, 0.053, 0.011, 0.040, 0.054, 0.111), RL = list(0, 0.053, 0.055, 0.050),
                   SF = c(0, 0.055, 0.067, 0.070, 0.087),
                   PAIN = c(0, 0.047, 0.025, 0.056, 0.091, 0.167),
                   MH = c(0, 0.049, 0.042, 0.109, 0.128), VIT = c(0, 0.086, 0.061, 0.054, 0.091),
                   MOST = 0.070)
  )
  calculate_linear_sf6d_values(PF, RL, SF, PAIN, MH, VIT, MOST, value_set)
}

#' SF-6D Norman et al. Australian population value set
#'
#' @param x SF6D vector
#'
#' @export
oz <- function(x) {
  version <- attr(x, "version")
  if (version == "SF-12") stop_incompatible_values("oz", "SF-12")

  PF <- extract(x, "PF")
  RL <- extract(x, "RL")
  SF <- extract(x, "SF")
  PAIN <- extract(x, "PAIN")
  MH <- extract(x, "MH")
  VIT <- extract(x, "VIT")

  value_set <- list(PF = c(0, 0.039, 0.078, 0.133, 0.139, 0.293), RL = list(0, 0.085, 0.054, 0.106),
                    SF = c(0, 0.044, 0.044, 0.122, 0.133),
                    PAIN = c(0, 0.095, 0.187, 0.203, 0.281, 0.297),
                    MH = c(0, 0.052, 0.084, 0.185, 0.275), VIT = c(0, 0.030, 0.035, 0.220, 0.259))

  calculate_linear_sf6d_values(PF, RL, SF, PAIN, MH, VIT, values = value_set)
}

calculate_linear_sf6d_values <- function(PF, RL, SF, PAIN, MH, VIT, MOST = NULL, values) {
  tbl <- dplyr::tibble(
    PF = PF, RL = RL, SF = SF, PAIN = PAIN, MH = MH, VIT = VIT, MOST = {{MOST}} %||% 0,
    utility = 1 - (
      dplyr::recode(PF, !!!values$PF) +
        dplyr::recode(RL, !!!values$RL) +
        dplyr::recode(SF, !!!values$SF) +
        dplyr::recode(PAIN, !!!values$PAIN) +
        dplyr::recode(MH, !!!values$MH) +
        dplyr::recode(VIT, !!!values$VIT) +
        MOST * (values$MOST %||% 0)
    )
  )
  tbl$utility
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
