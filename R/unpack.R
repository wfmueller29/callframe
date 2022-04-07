#' Unpack output from pmap call
#'
#' When running pmap we have the option to run it safely and quietly. If so,
#' the output from each call is nested in a list that also contains error
#' messages, warning messages, echoed output, and what was returned from our
#' function call.
#'
#' @param outputs a list of safely and quielty packaged outputs
#'
#' @return a list of lists such that each element of the parent list contains
#' all ouput from each function call and each element of the inner list
#' contains the various outputs (warnings, errors, messages, results, ect.)
#' from that particular call
#' 
#' @export

unpack <- function(outputs) {
  outputs_unlist <- lapply(outputs, unlist)

  outputs_names_unique <- unique(c(unlist(lapply(outputs_unlist, names))))

  outputs_unlist_complete <- list()
  i <- 1
  for (output in outputs_unlist) {
    for (nm in outputs_names_unique) {
      if (nm %in% names(output)) {
        outputs_unlist_complete[[nm]][[i]] <- output[[nm]]
      } else {
        outputs_unlist_complete[[nm]][[i]] <- NA
      }
    }
    i <- i + 1
  }

  unpacked_outputs <- outputs_unlist_complete

  unpacked_outputs
}

