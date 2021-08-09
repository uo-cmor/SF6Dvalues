# Define the `SF6D` S3 class
new_SF6D <- function(PF = integer(), RL = integer(), SF = integer(), PAIN = integer(), MH = integer(),
                     VIT = integer(), version = "SF-12") {
  for (arg in list(PF, RL, SF, PAIN, MH, VIT)) checkmate::assert_integer(arg)
  checkmate::assert_choice(version, c("SF-12", "SF-36"))

  responses <- list(PF = PF, RL = RL, SF = SF, PAIN = PAIN, MH = MH, VIT = VIT)

  new_rcrd(responses, class = "SF6Dvalues_SF6D", version = version)
}

#' `SF6D` vector
#'
#' This collates SF-6D dimension scores into an S3 vector to allow nice print
#'     methods and type checking.
#'
#' @param PF,RL,SF,PAIN,MH,VIT Vectors containing SF-6D dimension scores.
#' @param version Which questionnaire ("SF-12" or "SF-36") are the data derived
#'     from?
#'
#' @export
SF6D <- function(PF = integer(), RL = integer(), SF = integer(), PAIN = integer(), MH = integer(),
                 VIT = integer(), version = "SF-12") {
  if (!checkmate::test_choice(version, "SF-12")) stop_not_implemented("version", version)
  validate_SF6D_values(PF, RL, SF, PAIN, MH, VIT, version)
  PF <- as.integer(PF)
  RL <- as.integer(RL)
  SF <- as.integer(SF)
  PAIN <- as.integer(PAIN)
  MH <- as.integer(MH)
  VIT <- as.integer(VIT)
  new_SF6D(PF, RL, SF, PAIN, MH, VIT, version)
}

#' Test if the object is an SF6D vector
#'
#' This function returns \code{TRUE} for SF6D vectors (or subclasses thereof)
#'     and \code{FALSE} for all other objects.
#'
#' @param x Object to test.
#'
#' @export
is_SF6D <- function(x) inherits(x, "SF6Dvalues_SF6D")

#' Coerce an object to an SF6D vector
#'
#' Turns an existing object into an SF6D-class vector. Currently only works for
#'     SF6D vectors -- i.e., doesn't do anything.
#'
#' @param x Object to coerce
#'
#' @export
as_SF6D <- function(x) {
  UseMethod("as_SF6D")
}

#' @export
as_SF6D.default <- function(x) vec_cast(x, new_SF6D())

#' @export
as_SF6D.SF6Dvalues_SF12 <- function(x) {
  version <- attr(x, "version")

  PF <- 4L - field(x, "Q2")
  RLp <- switch(version, 2L - field(x, "Q5"), as.integer(field(x, "Q5") < 5))
  RLe <- switch(version, 2L - field(x, "Q6"), as.integer(field(x, "Q6") < 5))
  RL <- 1L + RLp + 2L * RLe
  SF <- switch(version, 6L - (field(x, "Q12") - (field(x, "Q12") > 2)), 6L - field(x, "Q12"))
  PAIN <- field(x, "Q8")
  MH <- switch(version, 6L - (field(x, "Q11") - (field(x, "Q11") > 2)), 6L - field(x, "Q11"))
  VIT <- switch(version, field(x, "Q10") - (field(x, "Q10") > 2), field(x, "Q10"))

  new_SF6D(PF, RL, SF, PAIN, MH, VIT, version = "SF-12")
}

#' @export
format.SF6Dvalues_SF6D <- function(x, ...) {
  values <- vec_data(x)
  values[is.na(values)] <- cli::symbol$dot
  Reduce(paste0, values)
}

#' @export
vec_ptype_full.SF6Dvalues_SF6D <- function(x, ...) "SF6D"

#' @export
vec_ptype2.SF6Dvalues_SF6D.SF6Dvalues_SF6D <- function(x, y, ...) new_SF6D()

#' @export
vec_cast.SF6Dvalues_SF6D.SF6Dvalues_SF6D <- function(x, to, ...) x

# for compatability with the S4 system
methods::setOldClass(c("SF6Dvalues_SF6D", "vctrs_vctr"))

validate_SF6D_values <- function(PF, RL, SF, PAIN, MH, VIT, version) {
  valid_values <- sf6d_dimension_levels(version)
  for (d in c("PF", "RL", "SF", "PAIN", "MH", "VIT")) {
    value <- get(d)
    if (!all(value[!is.na(value)] %in% valid_values[[d]]))
      stop_invalid_dimension_levels(d, valid_values[[d]], value)
  }
  invisible(NULL)
}

sf6d_dimension_levels <- function(version) {
  checkmate::assert_choice(version, c("SF-12", "SF-36"))
  switch(
    version,
    "SF-12" = list(PF = 1:3, RL = 1:4, SF = 1:5, PAIN = 1:5, MH = 1:5, VIT = 1:5)
  )
}
