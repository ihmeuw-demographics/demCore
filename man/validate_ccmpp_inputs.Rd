% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccmpp.R
\name{validate_ccmpp_inputs}
\alias{validate_ccmpp_inputs}
\title{Validate the ccmpp input arguments}
\usage{
validate_ccmpp_inputs(inputs, settings, value_col)
}
\arguments{
\item{inputs}{[\code{list()}]\cr
[\code{data.table()}] for each ccmpp input. Requires 'srb', 'asfr', 'baseline';
mortality estimates provided as just 'survival' or two of 'mx', 'ax', and
'qx'; and migration estimates provided as just 'net_migration' or both
'immigration' and 'emigration'. See \strong{Section: Inputs} for more
information on each of the required inputs.}

\item{settings}{[\code{list()}]\cr
Named list of settings for running \code{\link[=ccmpp]{ccmpp()}} with. See
\strong{Section: Settings} for more information on each of the required
settings.}

\item{value_col}{[\code{character(1)}]\cr
Name of the column containing the value of interest in each of the
\code{inputs}. Default is 'value'.}
}
\value{
Invisibly returns \code{inputs} but throws error if not formatted
correctly.
}
\description{
Validate the ccmpp input arguments
}
\keyword{internal}
