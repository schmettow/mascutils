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


#' mode
#'
#' estimation of the mode. Uses mofdeest::shorth
#' Will be replaced by an own implementation to work around the
#' odd dependencies of modeest (genefilter fro BioConductor).
#'
#' @param x data
#' @return numerical
#'
#' @author Martin Schmettow
#' @export

mode <- function(x) {
  modeest::shorth(x)
  # binning <-
  #   tibble(x = x) %>%
  #   mutate(bin = x%/%bin_width) %>%
  #   group_by(bin) %>%
  #   summarize(N = n()) %>%
  #   ungroup() %>%
  #   filter(N == max(N))
  #
  # binning$bin + bin_width/2
}



# beta_mean <- function(shape1, shape2)
#   shape1/(shape1 + shape2)
#
# beta_mean(2, 4)
# beta_sd(2, 4)
#
# beta_sd   <- function(shape1, shape2)
#   sqrt((shape1 * shape2/
#           (shape1 + shape2)^2 * (shape1 + shape2 + 1)))
#
# beta_shape1 <- function(mean, sd){
#   out <- c()
# }
#
