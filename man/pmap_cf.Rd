% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmap_cf.R
\name{pmap_cf}
\alias{pmap_cf}
\title{Rowise Map Call Frame}
\usage{
pmap_cf(
  cf,
  fun,
  type,
  safe_quiet = TRUE,
  pkgs = NULL,
  seed = TRUE,
  tictoc = TRUE,
  progress = TRUE
)
}
\arguments{
\item{cf}{the call frame that we are going to map}

\item{fun}{the function that we are going to apply to each row of the call
frame}

\item{type}{a named character vector where the names are the names of the
columns within the cf that are to be included as arguments to fun. Options
for this parameter are c("sym", "form", "chr", "bool", "num")}

\item{safe_quiet}{a boolean that specifies if fun should be wrapped by
the safely and quietly arguements in the purrr package}

\item{pkgs}{an Optional argumenet where the user can explicitly tell the
future package which packages are required to run the fun arguments. This
may not be necessary, however if errors are occuring, this may be why.}

\item{seed}{if your function requires seeding this argument must be set
to true by default.}

\item{tictoc}{boolean specifying if you would like to use the package
'tictoc' to time each call}

\item{progress}{boolean specifying if you would liek to use the package
'progress' to provide progress bar updates}
}
\description{
Given a call frame, the user can map any function.
}
\author{
William Mueller
}
