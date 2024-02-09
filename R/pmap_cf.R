#' Rowise Map Call Frame
#'
#' Given a call frame, the user can map any function.
#'
#' @param cf the call frame that we are going to map
#' @param fun the function that we are going to apply to each row of the call
#' frame
#' @param type a named character vector where the names are the names of the
#' columns within the cf that are to be included as arguments to fun. Options
#' for this parameter are c("sym", "form", "chr", "bool", "num")
#' @param safe_quiet a boolean that specifies if fun should be wrapped by
#' the safely and quietly arguements in the purrr package
#' @param pkgs an Optional argumenet where the user can explicitly tell the
#' future package which packages are required to run the fun arguments. This
#' may not be necessary, however if errors are occuring, this may be why.
#' @param seed if your function requires seeding this argument must be set
#' to true by default.
#' @param tictoc boolean specifying if you would like to use the package
#' 'tictoc' to time each call
#' @param progress boolean specifying if you would liek to use the package
#' 'progress' to provide progress bar updates
#'
#' @author William Mueller
#'
#' @export

pmap_cf <- function(cf,
                    fun,
                    type,
                    safe_quiet = TRUE,
                    pkgs = NULL,
                    seed = TRUE,
                    tictoc = TRUE,
                    progress = TRUE) {
  pkgs <- update_pkgs(pkgs, tictoc, progress, safe_quiet)

  fun <- update_safe_quiet(fun, safe_quiet)

  mos <- list()
  if (tictoc) tictoc::tic(msg = "Executing callframe")
  if (progress) pb <- progress::progress_bar$new(total = nrow(cf))
  for (i in seq_len(nrow(cf))) {
    row <- cf[i, ]
    row <- as.list(coerce_cf(row, type = type))
    call <- as.call(c(str2lang("fun"), row))
    exp <- create_expression(call, tictoc, progress)
    mos[[i]] <- future::future(eval(exp),
      packages = pkgs, seed = seed, substitute = FALSE
    )
  }
  mos <- lapply(mos, FUN = future::value)
  if (tictoc) tictoc::toc()

  mos
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
    cf_row[[arg]] <- stats::as.formula(cf_row[[arg]])
  }

  # Create symbol columns
  sym_args <- names(type)[type == "sym"]
  for (arg in sym_args) {
    cf_row[[arg]] <- as.symbol(cf_row[[arg]])
  }

  # Create numeric columns
  num_args <- names(type)[type == "num"]
  for (arg in num_args) {
    cf_row[[arg]] <- as.numeric(cf_row[[arg]])
  }

  # Create boolean columns
  bool_args <- names(type)[type == "bool"]
  for (arg in bool_args) {
    cf_row[[arg]] <- as.logical(cf_row[[arg]])
  }

  # Create object columns
  obj_args <- names(type)[type == "obj"]
  for (arg in obj_args) {
    cf_row[[arg]] <- eval(as.symbol(cf_row[[arg]]))
  }

  cf_row
}

update_pkgs <- function(pkgs, tictoc, progress, safe_quiet) {
  if (tictoc) {
    pkgs <- c(pkgs, "tictoc")
  }

  if (progress) {
    pkgs <- c(pkgs, "progress")
  }

  if (safe_quiet) {
    pkgs <- c(pkgs, "purrr")
  }

  pkgs
}

update_safe_quiet <- function(fun, safe_quiet) {
  if (safe_quiet) {
    fun <- purrr::safely(purrr::quietly(fun))
  } else {
    fun <- fun
  }
  fun
}

create_expression <- function(call, tictoc, progress) {
  if (tictoc & progress) {
    exp <- expression(
      tictoc::tic(msg = paste("Call", i, sep = " ")),
      mo <- eval(call),
      tictoc::toc(),
      pb$tick(),
      mo
    )
  } else if (tictoc) {
    exp <- expression(
      tictoc::tic(msg = paste("Call", i, sep = " ")),
      mo <- eval(call),
      tictoc::toc(),
      mo
    )
  } else if (progress) {
    exp <- expression(
      mo <- eval(call),
      pb$tick(),
      mo
    )
  } else {
    exp <- expression(eval(call))
  }
  exp
}
