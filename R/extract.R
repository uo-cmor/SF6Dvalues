#' Extract dimensions/questions from SF-6D/SF-12 vector
#'
#' @param x SF6D or SF12 vector
#' @param dimension Character string. Names of SF-6D dimension ("PF", "RL",
#'     "SF", "PAIN", "MH", or "VIT") or SF-12 question ("Q1", "Q2", ..., "Q12")
#'     to extract.
#'
#' @export
extract <- function(x, dimension) {
  UseMethod("extract")
}

#' @export
extract.SF6Dvalues_SF6D <- function(x, dimension) {
  if (!dimension %in% c("PF", "RL", "SF", "PAIN", "MH", "VIT")) stop_unknown_dimension(dimension)
  field(x, dimension)
}

#' @export
extract.SF6Dvalues_SF12 <- function(x, dimension) {
  if (!dimension %in% paste0("Q", 1:12)) stop_unknown_question(dimension)
  field(x, dimension)
}

#' @export
extract.SF6Dvalues_SF36 <- function(x, dimension) {
  if (!dimension %in% paste0("Q", 1:36)) stop_unknown_question(dimension)
  field(x, dimension)
}
