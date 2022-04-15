#' Make Call Frame
#'
#' This function will make a call frame given a list of named vectors. This
#' function will use the purrr::cross_df function to create a dataframe of
#' every possible call combination determined by the named lists.
#'
#' @param ... a named list with vectors
#'
#' @details if you provide the name of a variable using the | sybmol, then
#' this variable will be using in the combination then split into two separate
#' variables afterwards
#'
#' @export

make_cf <- function(...) {
  dots <- list(...)
  cf <- purrr::cross_df(dots)
  cf <- data.table::as.data.table(cf)
  bar_nms <- get_bar(cf)
  cf <- split_bar(cf, bar_nms)
  cf
}

get_bar <- function(cf) {
  bar <- grepl(".*\\|.*", x = names(cf))
  bar_nms <- names(cf)[bar]
  bar_nms
}

split_bar <- function(cf, bar_nms) {
  nms <- bar_nms
  for (nm in nms) {
    data.table::set(cf,
      i = NULL,
      j = unlist(strsplit(nm, "\\|")),
      value = data.table::tstrsplit(cf[[nm]], "\\|")
    )
  }
  cf
}

