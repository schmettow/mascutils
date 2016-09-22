
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
    expand.grid(stringsAsFactors = F, ...) %>% tibble::as_data_frame()


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
    args <- list(...)
    df1 <- dplyr::select_(D, .dots = args)
    mascutils::left_union(df1, D)
  }


#' @rdname go_first
#' @export

go_arrange <-
  function(D, ...){
    args <- list(...)
    df1 <- select_(D, .dots = args)
    D <- mascutils::left_union(df1, D)
    dplyr::arrange_(D, .dots = args)
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
    if(nrow(df1) != nrow(df2)) stop("dataframes have different number of rows")
    added_cols <- setdiff(names(df2), names(df1))
    dplyr::bind_cols(df1, df2[,added_cols])
  }







#' Adding z-scores
#'
#' The (simple) range of columns are z-transformed and added to the data frame (foo_z)
#'
#' @param D data frame
#' @param range expression
#' @param na.rm ignoring missing values (TRUE)
#' @param add adding z scores (TRUE)
#' @return data frame
#'
#'
#' @author Martin Schmettow
#' @import dplyr
#' @import tidyr
#' @export

## TODO: change to f_eval style and formulas

z_score <-
  function(D, range, na.rm = T, add = T){
    range <- substitute(range) ## catching column specification, changing context to local
    out <-
      D %>%
      select(eval(range)) %>%
      mutate(ID = row_number()) %>%
      gather(var, val, na.rm = F, -ID) %>%
      mutate(z_var = stringr::str_c("z", var, sep = "")) %>%
      group_by(var) %>%
      mutate(m = mean(val, na.rm = na.rm),
             s = sd(val, na.rm = na.rm),
             z_val = (val - m)/s) %>%
      ungroup() %>%
      select(ID, z_var, z_val) %>%
      spread(z_var, z_val) %>%
      select(-ID)
    if(add) out <- bind_cols(D, out)
    class(out) <- class(D)
    out
  }

## TODO:
# check out dplyr:::select_.data.frame for how to use dplyrs column expansion



#' Removing unmeasured variables
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

