#' simulate rating scale data
#'
#' using a scaled and binned logitnorm distribution
#'
#' @param N number of simulations
#' @param mu mean of underlying normal
#' @param sd sd of underlying normal
#' @param ends vector of two endpoints c(0,5)
#' @param bin whether to bin to integer values
#' @return numerical
#'
#' Simulates rating scale responses using rlogitnorm.
#' Note that mu and sd are for the underlying normal distribution.
#'
#' @author Martin Schmettow
#' @export

rrating_scale <- function(N, mu, sd, ends = c(1, 5), bin = TRUE){
  out <- Leibniz:::rlogitnorm(N, mu, sd) * ends[2] + ends[1]
  if(bin) out <- as.integer(out)
  out
}
