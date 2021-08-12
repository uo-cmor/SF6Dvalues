#' SF-6D utility values
#'
#' Calculate SF-6D utility values from SF-6D profile (stored in an `SF6D`
#'     vector).
#'
#' Currently implemented value sets are the original SF-12 and SF-36 UK
#'     population SG values (Brazier et al. 2002, Brazier & Roberts 2004),
#'     available with `values = "uk"`; Australian population values estimated
#'     using a DCE-duration approach (Norman et al. 2014), with
#'     `values = "oz"`; Hong Kong population values (Lam et al. 2008),
#'     `values = "hk"`; modified UK population weights based on an episodic
#'     random utility model (ERUM; Craig 2016), `values = "erum"`; Spanish
#'     population values based on a lottery equivalent method (Abellan Perpinan
#'     et al. 2012), `values = "spain"`; and Portuguese population SG values
#'     (Ferreira et al. 2010), `values = "portugal"`.
#'
#' @param x SF6D vector
#' @param values Which value set to use. See details for currently implemented
#'     options.
#'
#' @seealso \code{\link{sf6d_utility}} to calculate SF-6D utility values
#'     directly from SF-12/SF-36 responses; \code{\link{uk}}, \code{\link{oz}},
#'     \code{\link{hk}}, \code{\link{erum}}, \code{\link{spain}},
#'     \code{\link{portugal}} for the country-specific value sets.
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
  if (!checkmate::test_choice(values, c("uk", "oz", "hk", "erum", "spain", "portugal")))
    stop_not_implemented("values", values)

  f <- get(values)
  f(x)
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
    "SF-12" = list(PF = c(0, 0, 0.045), RL = c(0, 0.063, 0.063, 0.063),
                   SF = c(0, 0.063, 0.066, 0.081, 0.093), PAIN = c(0, 0, 0.042, 0.077, 0.137),
                   MH = c(0, 0.059, 0.059, 0.113, 0.134), VIT = c(0, 0.078, 0.078, 0.078, 0.106),
                   MOST = 0.077),
    "SF-36" = list(PF = c(0, 0.053, 0.011, 0.040, 0.054, 0.111), RL = c(0, 0.053, 0.055, 0.050),
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

  value_set <- list(PF = c(0, 0.039, 0.078, 0.133, 0.139, 0.293), RL = c(0, 0.085, 0.054, 0.106),
                    SF = c(0, 0.044, 0.044, 0.122, 0.133),
                    PAIN = c(0, 0.095, 0.187, 0.203, 0.281, 0.297),
                    MH = c(0, 0.052, 0.084, 0.185, 0.275), VIT = c(0, 0.030, 0.035, 0.220, 0.259))

  calculate_linear_sf6d_values(PF, RL, SF, PAIN, MH, VIT, values = value_set)
}

#' SF-6D Lam et al. Hong Kong population value set
#'
#' @param x SF6D vector
#'
#' @export
hk <- function(x) {
  version <- attr(x, "version")
  if (version == "SF-12") stop_incompatible_values("hk", "SF-12")

  PF <- extract(x, "PF")
  RL <- extract(x, "RL")
  SF <- extract(x, "SF")
  PAIN <- extract(x, "PAIN")
  MH <- extract(x, "MH")
  VIT <- extract(x, "VIT")

  value_set <- list(PF = c(0, 0.060, 0.073, 0.099, 0.157, 0.232), RL = c(0, 0.065, 0.053, 0.067),
                    SF = c(0, 0.052, 0.036, 0.113, 0.131),
                    PAIN = c(0, 0.075, 0.068, 0.082, 0.103, 0.183),
                    MH = c(0, 0.069, 0.037, 0.172, 0.098), VIT = c(0, 0.026, 0.031, 0.060, 0.137))

  calculate_linear_sf6d_values(PF, RL, SF, PAIN, MH, VIT, values = value_set)
}

#' SF-6D modified UK population value set (Episodic Random Utility Model)
#'
#' @param x SF6D vector
#'
#' @export
erum <- function(x) {
  version <- attr(x, "version")
  if (version == "SF-12") stop_incompatible_values("erum", "SF-12")

  PF <- extract(x, "PF")
  RL <- extract(x, "RL")
  SF <- extract(x, "SF")
  PAIN <- extract(x, "PAIN")
  MH <- extract(x, "MH")
  VIT <- extract(x, "VIT")

  value_set <- list(PF = cumsum(c(0, 0.071, 0, 0.016, 0, 0.151)),
                    RL = cumsum(c(0, 0.071, 0, 0.014)),
                    SF = cumsum(c(0, 0.088, 0.006, 0.004, 0.056)),
                    PAIN = cumsum(c(0, 0.063, 0, 0.024, 0.061, 0.100)),
                    MH = cumsum(c(0, 0.076, 0.007, 0.055, 0.047)),
                    VIT = cumsum(c(0, 0.071, 0, 0, 0.048)))

  calculate_linear_sf6d_values(PF, RL, SF, PAIN, MH, VIT, values = value_set)
}

#' SF-6D Abellan Perpinan et al. Spanish population value set
#'
#' @param x SF6D vector
#'
#' @export
spain <- function(x) {
  version <- attr(x, "version")
  if (version == "SF-12") stop_incompatible_values("spain", "SF-12")

  PF <- extract(x, "PF")
  RL <- extract(x, "RL")
  SF <- extract(x, "SF")
  PAIN <- extract(x, "PAIN")
  MH <- extract(x, "MH")
  VIT <- extract(x, "VIT")

  value_set <- list(PF = c(0, 0.015, 0.034, 0.090, 0.111, 0.338), RL = c(0, 0.014, 0.038, 0.070),
                    SF = c(0, 0.037, 0.060, 0.203, 0.208),
                    PAIN = c(0, 0.018, 0.034, 0.198, 0.202, 0.318),
                    MH = c(0, 0.066, 0.078, 0.096, 0.224), VIT = c(0, 0.058, 0.121, 0.157, 0.199))

  calculate_linear_sf6d_values(PF, RL, SF, PAIN, MH, VIT, values = value_set)
}

#' SF-6D Ferreira et al. Portuguese population value set
#'
#' @param x SF6D vector
#'
#' @export
portugal <- function(x) {
  version <- attr(x, "version")
  if (version == "SF-12") stop_incompatible_values("portugal", "SF-12")

  PF <- extract(x, "PF")
  RL <- extract(x, "RL")
  SF <- extract(x, "SF")
  PAIN <- extract(x, "PAIN")
  MH <- extract(x, "MH")
  VIT <- extract(x, "VIT")

  value_set <- list(PF = c(0, 0.029, 0.029, 0.047, 0.050, 0.207), RL = c(0, 0.012, 0.012, 0.061),
                    SF = c(0, 0.025, 0.025, 0.051, 0.075),
                    PAIN = c(0, 0, 0, 0.049, 0.049, 0.087),
                    MH = c(0, 0.038, 0.038, 0.066, 0.100), VIT = c(0, 0.040, 0.040, 0.041, 0.092))

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
