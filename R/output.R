#' tibble class for observations
#'
#' A class for tibbles with observations providing a compact output
#'
#' @usage as_tbl_obs(x)
#' @param x a data frame
#' @return tbl_obs
#'
#' The constructor makes any data.frame or tibble class tbl_obs.
#' The provided print method randomly selects a small number of
#' observations and puts it into a knitr::kable, which looks nice
#' on both, console and knitr output.
#'
#' @author Martin Schmettow
#' @importFrom knitr knit_print
#' @export

as_tbl_obs <- function(x) UseMethod("as_tbl_obs", x)

#' @rdname as_tbl_obs
#' @export

as_tbl_obs.tbl_df <- function(x) {
  x <- mutate(x, Obs = row_number()) %>%
    mascutils::go_first(Obs)
  class(x) <- c("tbl_obs", class(x))
  x
}

#' @rdname as_tbl_obs
#' @export

as_tbl_obs.data.frame <- function(x) {
  x <- tibble::as_tibble(x) %>%
    as_tbl_obs.tbl_df(x)
  class(x) <- c("tbl_obs", class(x))
  x
}

#' @rdname as_tbl_obs
#' @export

print.tbl_obs <- function(x, ...) {
  n <- min(8, nrow(x))
  tab <- dplyr::sample_n(x, n)
  if("Obs" %in% colnames(tab)) tab <- dplyr::arrange(tab, Obs)
  if("Part" %in% colnames(tab)) tab <- dplyr::arrange(tab, Part)
  print(tab)
  invisible(tab)
}

#' @rdname as_tbl_obs
#' @export

knit_print.tbl_obs <- function(x, ...) {
  cap <- stringr::str_c("showing ", n, " of ", nrow(x), " observations")
  print.tbl_obs(x, ...)
  tab <- knitr::kable(tab, align = "c", format = "html") %>%
    kableExtra::kable_styling(full_width = F) %>%
    kableExtra::add_footnote(c(cap), notation = "symbol")
  print(tab)
  invisible(tab)
}

