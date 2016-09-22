#' logit
#'
#' computes the logit (or its inverse) on a numerical vector
#'
#' @param x numerical
#' @return numerical
#'
#' @author Martin Schmettow
#' @export


logit <-
  function(x) log(x/(1-x))

#' @rdname logit
#' @export

inv_logit <-
  function(x) plogis(x)


#' exp_law
#'
#' the exponential law of learning in previous experience parametrization
#'
#' @param pexp virtual previous experience
#' @param rate rate of learning [-1;1]
#' @param asym asymptote of maximum performance
#' @param N number of sessions
#' @return numerical
#'
#' @author Martin Schmettow
#' @export


exp_law <-
  function(pexp, rate, asym, N) asym * (1 + exp(-rate * (N + pexp)))

