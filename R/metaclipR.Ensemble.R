##     metaclipR.Ensemble Construct a METACLIP representation of the ds:Ensemble class
##
##     Copyright (C) 2018 Predictia (http://www.predictia.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Construct a METACLIP representation of the ds:Ensemble class
#' @description Build a directed metadata graph describing an Ensemble transformation from two or more
#' ds:Steps
#' @param package package name. Default to \code{"transformeR"}
#' @param version Package version string.
#' @param fun function name. Default to \code{"bindGrid.member"})
#' @param graph.list A \code{metaclipR} object data list, each element being the
#' graph defining each ensemble member
#' @param output Optional. The output R object name, as character string
#' @param arg.list Argument list. See details
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://metaclip.predictia.es/}).
#' 
#' \strong{Argument list}
#' 
#' The following (minimal) list of arguments is required to define an ETCCDI climate index transformation:
#' \itemize{
#' \item \code{index.code}
#' }
#' 
#' Further optional arguments can be passed to \code{arg.list} for a more detailed description of the command call.
#' The different arguments are explained in the the help page of \code{\link[climate4R.climdex]{climdexGrid}}. 
#' 
#' @references 
#' 
#' \href{https://github.com/Predictia/metaclip}{METACLIP Overview}
#' 
#' \href{http://www.meteo.unican.es/en/climate4r}{Climate4R page at University of Cantabria}
#' 
#' @family transformation
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia

metaclipR.Ensemble <- function(package = "transformeR",
                               version = "1.3.3",
                               output = NULL,
                               fun = "bindGrid.member",
                               arg.list = NULL,
                               graph.list) {
    if (length(graph.list) < 2) {
        stop("The input must be a list of at least two metaclipR graphs", call. = FALSE)
    }
    for (i in 1:length(graph.list)) {
        if (class(graph.list[[i]]$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")    
    }
    # Ensemble node
    graph <- make_empty_graph()
    nodename <- paste0("Ensemble.", randomName()) 
    graph <- my_add_vertices(graph,
                             nv = 1,
                             name = nodename,
                             label = "Multi-model Ensemble",
                             className = paste0("ds:Ensemble"))
    for (i in 1:length(graph.list)) {
        graph <- my_union_graph(graph.list[[i]]$graph, graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, graph.list[[i]]$parentnodename),
                             getNodeIndexbyName(graph, nodename)),
                           label = "ds:wasEnsembleMember")
    }
    # Package/Command/Argument metadata ---------------------------------------
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                    origin.node.name = nodename)
    return(list("graph" = graph, "parentnodename" = nodename))
}
