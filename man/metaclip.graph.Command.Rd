% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaclip.graph.Command.R
\name{metaclip.graph.Command}
\alias{metaclip.graph.Command}
\title{Directed metadata graph construction for Package/Command/Argument provenance description}
\usage{
metaclip.graph.Command(graph, package, version, fun, arg.list, origin.node.name)
}
\arguments{
\item{graph}{An igraph-class graph}

\item{package}{Character string. Package name}

\item{version}{Character string. Package version}

\item{fun}{Character string. Function name}

\item{arg.list}{Argument list, passed by the upper level function.}

\item{origin.node.name}{Name of the origin node of the previously existing graph.}
}
\description{
Build a directed metadata graph describing R calls generating outputs
}
\details{
This function takes as reference the semantics defined in the Seasonal Forecast Verification ontology
defined in the Metaclip Framework (\url{http://metaclip.predictia.es/}).
}
\references{
\itemize{
\item \href{https://docs.google.com/presentation/d/1CQyxVIj501N7VylMR9i_T_XwFDId6MDNvpPnuaLXgnI/present#slide=id.g1f8b6ebe3d_0_196}{Visual schema of the Command Step Definition}
}
}
\seealso{
Other transformation: \code{\link{metaclipR.Aggregation}},
  \code{\link{metaclipR.AnomalyCalculation}},
  \code{\link{metaclipR.Climatology}},
  \code{\link{metaclipR.Dataset}},
  \code{\link{metaclipR.Ensemble}},
  \code{\link{metaclipR.Regridding}},
  \code{\link{metaclipR.etccdi}}
}
\author{
D. San Martín, J. Bedia
}
\keyword{internal}
