% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Media_Mix.R
\name{Media_Mix}
\alias{Media_Mix}
\title{Media Mix Optimization}
\usage{
Media_Mix(
  mod_obj,
  total_budget = NULL,
  incremental = 1e+05,
  interactive = FALSE
)
}
\arguments{
\item{mod_obj}{- model object}

\item{total_budget}{- total budget}

\item{incremental}{- incremental step size}

\item{interactive}{- should the user be asked for Min and Max budget input. (only set TRUE when in an interactive session)}
}
\value{
mod_obj
}
\description{
This function implements media mix optimization.
}
