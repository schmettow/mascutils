
#' appending objects to an Rdata file
#'
#' works like save(), but adds objects, instead of overwriting
#' This function is slow for large objects. For discussion see
#' https://stackoverflow.com/questions/11813096/updating-an-existing-rdata-file
#'
#' @param ... factors
#' @return data_frame
#'
#' @author Martin Schmettow
#' @export

resave <- function(..., list = character(), file) {
  e <- new.env()
  load(file, e)
  list <- union(list, as.character(substitute((...)))[-1L])
  copyEnv(parent.frame(), e, list)
  save(list = ls(e, all.names=TRUE), envir = e, file = file)
}
