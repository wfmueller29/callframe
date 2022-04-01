
make_cf <- function(..., .action = NULL, .type = NULL) {
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
    set(cf,
      i = NULL,
      j = unlist(strsplit(nm, "\\|")),
      value = data.table::tstrsplit(cf[[nm]], "\\|")
    )
  }
  cf
}

