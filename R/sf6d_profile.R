#' Generate SF-6D profile from SF-12/SF-36 questionnaire
#'
#' Construct an SF-6D profile from SF-12 (to be implemented: SF-36)
#'     question-level responses. This is a simple wrapper around \code{SF12}
#'     and \code{as_SF6D}.
#'
#' @param ... Vectors containing SF-12 question reponses. See
#'     \code{\link{SF12}} for details.
#' @param questionnaire Which questionnaire ('SF-12' or 'SF-36') are the data
#'     obtained from. Only \code{questionnaire = 'SF-12'} is implemented at
#'     this stage.
#' @param version Which version (`1` or `2`) of the SF-12 are the data obtained
#'     from?
#'
#' @export
sf6d_profile <- function(..., questionnaire = "SF-12", version = 2L) {
  if (!checkmate::test_choice(questionnaire, "SF-12"))
    stop_not_implemented("questionnaire", questionnaire)
  sf12 <- SF12(..., version = version)
  as_SF6D(sf12)
}
