#' SF-12 Component Summary scores
#'
#' Calculate PCS and MCS (Physical and Mental Component Summary scores) from
#'     SF-12 vector or SF12 responses.
#'
#' @param x `SF12`-class vector.
#' @param ... Vectors containing SF-12 question reponses. See
#'     \code{\link{SF12}} for details.
#' @param version Which version (`1` or `2`) of the SF-12 are the data obtained
#'     from?
#' @name SF12_scores
NULL

#' @rdname SF12_scores
#' @export
PCS <- function(x) {
  dom <- SF12_domains(x)
  PHYS <- (dom$PFz * 0.42402 + dom$RPz * 0.35119 + dom$BPz * 0.31754 + dom$GHz * 0.24954
           + dom$VTz * 0.02877 + dom$SFz * -0.00753 + dom$REz * -0.19206 + dom$MHz * -0.22069)

  50 + PHYS * 10
}

#' @rdname SF12_scores
#' @export
MCS <- function(x) {
  dom <- SF12_domains(x)
  MENT <- (dom$PFz * -0.22999 + dom$RPz * -0.12329 + dom$BPz * -0.09731 + dom$GHz * -0.01571
           + dom$VTz * 0.23534 + dom$SFz * 0.26876 + dom$REz * 0.43407 + dom$MHz * 0.48581)

  50 + MENT * 10
}

#' @rdname SF12_scores
#' @export
sf12_PCS <- function(..., version = 2L) {
  sf12 <- SF12(..., version = version)
  PCS(sf12)
}

#' @rdname SF12_scores
#' @export
sf12_MCS <- function(..., version = 2L) {
  sf12 <- SF12(..., version = version)
  MCS(sf12)
}

#' SF-12 Scale Scores
#'
#' Calculate SF-12 norm-based scale scores. This function returns a tibble
#'     containing the eight health domain scale scores from the SF-12 (on the
#'     standardised 50/10 scale).
#'
#' @param x `SF12`-class vector.
#'
#' @export
SF12_scores <- function(x) {
  dom <- SF12_domains(x)
  dplyr::tibble(PF = 50 + dom$PFz * 10, RP = 50 + dom$RPz * 10,
                BP = 50 + dom$BPz * 10, GH = 50 + dom$GHz * 10,
                VT = 50 + dom$VTz * 10, SF = 50 + dom$SFz * 10,
                RE = 50 + dom$REz * 10, MH = 50 + dom$MHz * 10)
}

SF12_domains <- function(x) {
  if (!is_SF12(x)) {
    type <- if (is_SF6D(x)) {
      "an SF6D vector"
    } else if (rlang::is_bare_list(x)) {
      "a list"
    } else if (rlang::is_bare_numeric(x)) {
      "a numeric vector"
    } else if (rlang::is_atomic(x)) {
      paste0("a ", typeof(x), " vector")
    } else paste0("a ", class(x)[[1]], " object")
    stop_not_SF12(type)
  }
  if (attr(x, "version") == 1) stop_version_1()

  PF <- (dplyr::coalesce(extract(x, "Q2"), extract(x, "Q3"))
         + dplyr::coalesce(extract(x, "Q3"), extract(x, "Q2")) - 2) / 4 * 100
  RP <- (dplyr::coalesce(extract(x, "Q4"), extract(x, "Q5"))
         + dplyr::coalesce(extract(x, "Q5"), extract(x, "Q4")) - 2) / 8 * 100
  BP <- (6 - extract(x, "Q8") - 1) / 4 * 100
  GH <- (dplyr::recode(extract(x, "Q1"), 5, 4.4, 3.4, 2, 1) - 1) / 4 * 100
  VT <- (6 - extract(x, "Q10") - 1) / 4 * 100
  SF <- (extract(x, "Q12") - 1) / 4 * 100
  RE <- (dplyr::coalesce(extract(x, "Q6"), extract(x, "Q7"))
         + dplyr::coalesce(extract(x, "Q7"), extract(x, "Q6")) - 2) / 8 * 100
  MH <- (dplyr::coalesce(6L - extract(x, "Q9"), extract(x, "Q11"))
         + dplyr::coalesce(extract(x, "Q11"), 6L - extract(x, "Q9")) - 2) / 8 * 100

  PFz <- (PF - 81.18122) / 29.10558
  RPz <- (RP - 80.52856) / 27.13526
  BPz <- (BP - 81.74015) / 24.53019
  GHz <- (GH - 72.19795) / 23.19041
  VTz <- (VT - 55.59090) / 24.84380
  SFz <- (SF - 83.73973) / 24.75775
  REz <- (RE - 86.41051) / 22.35543
  MHz <- (MH - 70.18217) / 20.50597

  list(PF = PF, RP = RP, BP = BP, GH = GH, VT = VT, SF = SF, RE = RE, MH = MH,
       PFz = PFz, RPz = RPz, BPz = BPz, GHz = GHz, VTz = VTz, SFz = SFz, REz = REz, MHz = MHz)
}
