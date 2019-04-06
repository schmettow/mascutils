
#' dplyrified expand.grid
#'
#' works like expand.grid, but returns a tibble data_frame
#'
#' @param ... factors
#' @return data_frame
#'
#' @author Martin Schmettow
#' @export


expand_grid <-
  function(...)
    expand.grid(stringsAsFactors = F, ...) %>% tibble::as_tibble()


#' Moving column to first position
#'
#' The referred column becomes the first, keeping the order of the others
#'
#' @param D data frame
#' @param ... formulas, like in select, but with ~
#' @return data frame
#'
#' go_first and go_arrange expect a column specification, similar to
#' dplyr::select, but as a formula
#'
#' @examples
#' D <- data_frame(x = 1:3, y = 6:4, z = c(8,9,7))
#' go_first(D, ~y)
#' go_first(D, ~y:z)
#' go_arrange(D, ~y)
#'
#'
#'
#' @author Martin Schmettow
#' @export


go_first <-
  function(D, ...){
    class <- class(D)
    cols <- quos(...)
    df1 <- dplyr::select(D, !!!cols)
    out <- mascutils::left_union(df1, D)
    class(out) <- class
    out
  }


#' @rdname go_first
#' @export

go_arrange <-
  function(D, ...){
    class <- class(D)
    cols <- quos(...)
    df1 <- dplyr::select(D, !!!cols)
    out <- mascutils::left_union(df1, D) %>%
      dplyr::arrange_(names(df1))
    class(out) <- class
    out
  }



#' adding columns that don't exist yet
#'
#'
#' @param df1 master data frame, where columns are kept
#' @param df2 data frame where additional columns are taken from
#' @return data frame
#'
#'
#'
#' @author Martin Schmettow
#' @export

left_union <-
  function(df1, df2){
    class <- class(df1)
    if(nrow(df1) != nrow(df2)) stop("dataframes have different number of rows")
    added_cols <- setdiff(names(df2), names(df1))
    out <- dplyr::bind_cols(df1, df2[,added_cols])
    class(out) <- class
    out
  }


#' update columns conditionally
#'
#'
#' @param D data frame
#' @param filter predicate function (like dplyr::filter)
#' @param ... expressions
#' @return data frame
#'
#' Applies mutations to the filtered group, only.
#'
#' @examples
#' D <- tribble(~group, ~value,  1, 4, 1, 9, 2, -4, 2, -9)
#'
#' D %>% mutate(value = if_else(group == 1, sqrt(value), value))
#' ## Produces NaNs, because sqrt() is evaluated before selection
#'
#' D %>% mutate_by(group == 1, value = sqrt(value))
#' ## sqrt() is only evaluated
#'
#' @author Martin Schmettow
#' @export



update_by <-
  function(D, by, ...){
    flt <- enquo(by)
    args <- enquos(...)

    missing_cols <- dplyr::setdiff(names(args), names(D))
    if (length(missing_cols > 0)) stop(paste0("\nColumn does not exist: ",
                                              missing_cols))

    D <- dplyr::mutate(D, .tmp_idx = dplyr::row_number())

    mut <- D %>%
      dplyr::filter(!!flt) %>%
      dplyr::mutate(!!!args)

    rmn <- D %>%
      dplyr::filter(!(!!flt))

    bind_rows(mut, rmn) %>%
      dplyr::arrange(.tmp_idx) %>%
      dplyr::select(-.tmp_idx)
  }





#' Adding z-scores
#'
#' The (simple) range of columns are z-transformed and added to the data frame (foo_z)
#'
#' @param D data frame
#' @param ... dplyr::select range of variables
#' @return data frame
#'
#'
#' @author Martin Schmettow
#' @import dplyr
#' @import tidyr
#' @export


z_score <- function(D,  ...){
  col_spec <- quos(...)
  df_z <- dplyr::select(D, !!!col_spec) %>%
    transmute_all(z)
  names(df_z) <- stringr::str_c("z", names(df_z), sep = "")
  bind_cols(D, df_z)
}

z <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)

## TODO:
# check out dplyr:::select_.data.frame for how to use dplyrs column expansion



#' Removing completely unmeasured variables
#'
#' all columns that are completely NA are discarded
#'
#' @param D data frame
#' @return data frame
#'
#'
#' @author Martin Schmettow
#' @export

discard_all_na <-
  function(D){
    filter = which(plyr::aaply(as.matrix(D), 2, any_not_na))
    var_non_na = colnames(D)[filter]
    out = select(D, one_of(var_non_na))
    out
  }

all_na <-
  function(x) all(is.na(x))

any_not_na <-
  function(x) !all_na(x)


#' Removing redundant variables
#'
#' all variables that do not vary are discarded
#'
#' @param D data frame
#' @return data frame
#'
#'
#' @author Martin Schmettow
#' @export




discard_redundant <- function(D){
  if(nrow(D) < 2) return(D)

  a <- as.matrix(D)
  nonred <- plyr::aaply(a, 2, function(v) length(unique(v)) > 1)
  D[, nonred]
}

