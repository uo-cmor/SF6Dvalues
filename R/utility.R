#' SF-6D utility values
#'
#' Calculate SF-6D utility values using specified value set/algorithm.
#'
#' @param x SF6D vector
#' @param values Which value set to use. Currently only 'uk' (Brazier & Roberts
#'     2004 algorithm) is implemented.
#'
#' @export
sf6d_utility <- function(x, values = "uk") {
  if (!is_SF6D(x)) stop_not_SF6D(x)
  if (!checkmate::test_choice(values, "uk")) stop_not_implemented("values", values)

  switch(values,
         uk = uk(x))
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
