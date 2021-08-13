#' Mapping to predict other utility instrument value sets from the SF-6D
#'
#' Calculate predicted utility values based on other utility instruments from
#'     the SF-6D, based on published mapping algorithms.
#'
#' The currently implemented mapping algorithms are:
#'
#' Gamst-Klaussen et al. 2016 ("gk"): non-linear mapping to EQ-5D-5L.
#'
#' Rowen et al. 2012 ("rowen'): linear mapping via VAS to EQ-5D-3L & HUI2.
#'
#' Richardson et al. 2015 ("richardson"): symmetrical geometric mean square
#'     regression mapping to EQ-5D-5L, HUI3, 15D, QWB, and AQoL-8D.
#'
#' @param x `SF6D` vector
#' @param instrument Which instrument to map to (currently "EQ-5D-5L",
#'     "EQ-5D-3L", "HUI2", "HUI3", "15D", "QWB", and "AQoL-8D" are
#'     implemented).
#' @param algorithm Which published mapping algorithm to use. See Details for
#'     the available algorithms for each target instrument.
mapping <- function(x, instrument, algorithm) {
  if (!is_SF6D(x)) {
    type <- if (is_SF12(x)) {
      "an SF12 vector"
    } else if (is_SF36(x)) {
      "an SF36 vector"
    } else if (rlang::is_bare_list(x)) {
      "a list"
    } else if (rlang::is_bare_numeric(x)) {
      "a numeric vector"
    } else if (rlang::is_atomic(x)) {
      paste0("a ", typeof(x), " vector")
    } else paste0("a ", class(x)[[1]], " object")
    stop_not_SF6D(type)
  }
  if (!checkmate::test_choice(instrument,
                              c("EQ-5D-5L", "EQ-5D-3L", "HUI2", "HUI3", "15D", "QWB", "AQoL-8D")))
    stop_not_implemented("instrument", instrument)
  switch(
    instrument,
    "EQ-5D-5L" = if (!checkmate::test_choice(algorithm, c("gk", "richardson")))
      stop_incompatible_algorithm(algorithm, "to", "EQ-5D-5L"),
    "EQ-5D-3L" = if (!checkmate::test_choice(algorithm, c("rowen")))
      stop_incompatible_algorithm(algorithm, "to", "EQ-5D-3L"),
    "HUI2" = if (!checkmate::test_choice(algorithm, c("rowen")))
      stop_incompatible_algorithm(algorithm, "to", "HUI2"),
    "HUI3" = if (!checkmate::test_choice(algorithm, c("richardson")))
      stop_incompatible_algorithm(algorithm, "to", "HUI3"),
    "15D" = if (!checkmate::test_choice(algorithm, c("richardson")))
      stop_incompatible_algorithm(algorithm, "to", "15D"),
    "QWB" = if (!checkmate::test_choice(algorithm, c("richardson")))
      stop_incompatible_algorithm(algorithm, "to", "QWB"),
    "AQoL-8D" = if (!checkmate::test_choice(algorithm, c("richardson")))
      stop_incompatible_algorithm(algorithm, "to", "AQoL-8D"),
  )

  f <- get(algorithm)
  f(x, instrument)
}

gk <- function(x, instrument) {
  checkmate::assert_class(x, "SF6Dvalues_SF6D")
  if (!checkmate::test_string(attr(x, "version"), pattern = "SF-36"))
    stop_incompatible_algorithm("gk", "from", "SF-6D(SF-12)")
  sf6d_utility <- utility(x)
  dplyr::case_when(
    sf6d_utility >= 0.87 ~ 1 - (1 - sf6d_utility) * 0.35,
    sf6d_utility >= 0.69 ~ 0.95 - (0.87 - sf6d_utility) * 0.58,
    sf6d_utility >= 0.54 ~ 0.85 - (0.69 - sf6d_utility) * 1.62,
    sf6d_utility >= 0.36 ~ 0.60 - (0.54 - sf6d_utility) * 2.19,
    TRUE ~ 0.22 - (0.36 - sf6d_utility) * 1.04
  )
}

rowen <- function(x, instrument) {
  checkmate::assert_class(x, "SF6Dvalues_SF6D")
  if (!checkmate::test_string(attr(x, "version"), pattern = "SF-36"))
    stop_incompatible_algorithm("rowen", "from", "SF-6D(SF-12)")
  sf6d_utility <- utility(x)

  switch(instrument,
         "EQ-5D-3L" = 1.571 * sf6d_utility - 0.633,
         HUI2 = 1.117 * sf6d_utility - 0.073)
}

richardson <- function(x, instrument) {
  checkmate::assert_class(x, "SF6Dvalues_SF6D")
  if (!checkmate::test_string(attr(x, "version"), pattern = "SF-36"))
    stop_incompatible_algorithm("richardson", "from", "SF-6D(SF-12)")
  sf6d_utility <- utility(x)

  value <- switch(
    instrument,
    "EQ-5D-5L" = 1.64 * sf6d_utility - 0.43,
    HUI3 = (sf6d_utility - 0.34) / 0.52,
    "15D" = (sf6d_utility + 0.21) / 1.08,
    "QWB" = (sf6d_utility - 0.15) / 0.89,
    "AQoL-8D" = {
      value <- (sf6d_utility - 0.29) / 0.61
      value[value < 0.09] <- 0.09
      value
    }
  )
  value[value > 1] <- 1

  value
}

