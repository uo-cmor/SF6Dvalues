#' SF-36 Scale Scores
#'
#' Calculate SF-36 health domain scale scores. This function returns a tibble
#'     containing the eight health domain scale scores from the SF-36 (on the
#'     standardised 0--100 scale).
#'
#' @param x `SF36`-class vector.
#'
#' @export
SF36_scores <- function(x) {
  if (!is_SF36(x)) {
    type <- if (is_SF6D(x)) {
      "an SF6D vector"
    } else if (is_SF12(x)) {
      "an SF12 vector"
    } else if (rlang::is_bare_list(x)) {
      "a list"
    } else if (rlang::is_bare_numeric(x)) {
      "a numeric vector"
    } else if (rlang::is_atomic(x)) {
      paste0("a ", typeof(x), " vector")
    } else paste0("a ", class(x)[[1]], " object")
    stop_incorrect_type("an SF36 vector", type)
  }

  n <- length(x)

  PF <- nPF <- rep(0, n)
  for (i in 3:12) {
    PF <- PF + dplyr::coalesce(extract(x, paste0("Q", i)), 0)
    nPF <- nPF + !is.na(extract(x, paste0("Q", i)))
  }
  PF <- dplyr::if_else(nPF >= 5, PF / nPF * 10, NA_real_)

  RP <- nRP <- rep(0, n)
  for (i in 13:16) {
    RP <- RP + dplyr::coalesce(extract(x, paste0("Q", i)), 0)
    nRP <- nRP + !is.na(extract(x, paste0("Q", i)))
  }
  RP <- dplyr::if_else(nRP >= 2, RP / nRP * 4, NA_real_)

  BP <- dplyr::if_else(
    !is.na(extract(x, "Q21")),
    dplyr::recode(extract(x, "Q21"), 6, 5.4, 4.2, 3.1, 2.2, 1)
    + dplyr::coalesce(6 - extract(x, "Q22") + (extract(x, "Q22") == 1 & extract(x, "Q21") == 1),
                      dplyr::recode(extract(x, "Q21"), 6, 5.4, 4.2, 3.1, 2.2, 1)),
    dplyr::recode(extract(x, "Q22"), 6, 4.75, 3.5, 2.25, 1) * 2
  )

  GH <- dplyr::coalesce(dplyr::recode(extract(x, "Q1"), 5, 4.4, 3.4, 2, 1), 0)
  nGH <- as.integer(!is.na(extract(x, "Q1")))
  for (i in c("Q33", "Q35")) {
    GH <- GH + dplyr::coalesce(extract(x, i), 0)
    nGH <- nGH + !is.na(extract(x, i))
  }
  for (i in c("Q34", "Q36")) {
    GH <- GH + (6 - dplyr::coalesce(extract(x, i), 0))
    nGH <- nGH + !is.na(extract(x, i))
  }
  GH <- dplyr::if_else(nGH >= 3, GH / nGH * 5, NA_real_)

  VT <- nVT <- rep(0, n)
  for (i in c("Q23", "Q27")) {
    VT <- VT + (7 - dplyr::coalesce(extract(x, i), 0))
    nVT <- nVT + !is.na(extract(x, i))
  }
  for (i in c("Q29", "Q31")) {
    VT <- VT + dplyr::coalesce(extract(x, i), 0)
    nVT <- nVT + !is.na(extract(x, i))
  }
  VT <- dplyr::if_else(nVT >= 2, VT / nVT * 4, NA_real_)

  SF <- (dplyr::coalesce(6L - extract(x, "Q20"), extract(x, "Q32"))
         + dplyr::coalesce(extract(x, "Q32"), 6L - extract(x, "Q20")))

  RE <- nRE <- rep(0, n)
  for (i in 17:19) {
    RE <- RE + dplyr::coalesce(extract(x, paste0("Q", i)), 0)
    nRE <- nRE + !is.na(extract(x, paste0("Q", i)))
  }
  RE <- dplyr::if_else(nRE >= 2, RE / nRE * 3, NA_real_)

  MH <- nMH <- rep(0, n)
  for (i in c("Q24", "Q25", "Q28")) {
    MH <- MH + dplyr::coalesce(extract(x, i), 0)
    nMH <- nMH + !is.na(extract(x, i))
  }
  for (i in c("Q26", "Q30")) {
    MH <- MH + (7 - dplyr::coalesce(extract(x, i), 0))
    nMH <- nMH + !is.na(extract(x, i))
  }
  MH <- dplyr::if_else(nMH >= 3, MH / nMH * 5, NA_real_)

  PF <- (PF - 10) / 20 * 100
  RP <- (RP - 4) / 4 * 100
  BP <- (BP - 2) / 10 * 100
  GH <- (GH - 5) / 20 * 100
  VT <- (VT - 4) / 20 * 100
  SF <- (SF - 2) / 8 * 100
  RE <- (RE - 3) / 3 * 100
  MH <- (MH - 5) / 25 * 100

  dplyr::tibble(PF, RP, BP, GH, VT, SF, RE, MH)
}
