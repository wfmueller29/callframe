#' rowwise map callframe
#'
#' Given a call frame, the user can map any function.
#'
#'

pmap_cf <- function(cf, fun, type, pkgs = NULL, seed = TRUE) {
  safe_quiet_fun <- purrr::safely(purrr::quietly(fun))
  mos <- list()
  for (i in seq_len(nrow(cf))) {
    row <- cf[i, ]
    row <- as.list(coerce_cf(row, type = type))
    call <- as.call(c(str2lang("safe_quiet_fun"), row))
    f_call <- as.call(c(
      str2lang("future::future"),
      list(call),
      list(packages = pkgs),
      list(seed = seed)
    ))
    mos[[i]] <- eval(f_call)
  }
  mos <- lapply(mos, FUN = future::value)
}

coerce_cf <- function(cf_row, type) {
  cf_row <- as.list(cf_row)

  # get names of args from type and cf_row and match them
  nms <- names(cf_row)
  mtch_nms <- names(cf_row)[nms %in% names(type)]

  # select columns for dt cf_row
  cf_row <- cf_row[mtch_nms]
  # Create formula columns
  form_args <- names(type)[type == "form"]
  for (arg in form_args) {
    cf_row[[arg]] <- as.formula(cf_row[[arg]])
  }

  # Create symbol columns
  sym_args <- names(type)[type == "sym"]
  for (arg in sym_args) {
    cf_row[[arg]] <- as.symbol(cf_row[[arg]])
  }

  # Create object columns
  obj_args <- names(type)[type == "obj"]
  for (arg in obj_args) {
    cf_row[[arg]] <- eval(as.symbol(cf_row[[arg]]))
  }

  cf_row
}
