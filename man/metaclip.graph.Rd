% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaclip.graph.R
\name{metaclip.graph}
\alias{metaclip.graph}
\title{Metaclip launcher}
\usage{
metaclip.graph(package, version, fun = NULL, arg.list = NULL,
  output = NULL)
}
\arguments{
\item{package}{Character string indicating the name of the package}

\item{version}{Character string indicating the package version. This can be obtained via \code{\link[utils]{packageVersion}},
but note that the value of the argument should always refer to the version actually used for the output generation,
 and not the currently installed version (that may differ).}

\item{fun}{Character string indicating the name of the function called}

\item{arg.list}{A list of arguments passed to \code{fun}}

\item{output}{The output returned by \code{fun} with the specified arguments}
}
\value{
A directed graph of class \code{\link[igraph]{igraph}}
}
\description{
Launch the metaclip encoding for a given process
}
\author{
D. San Martin, J. Bedia
}
