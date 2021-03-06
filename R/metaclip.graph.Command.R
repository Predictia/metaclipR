##     metaclip.graph.Command Construct a directed graph for a veriApply output metadata encoding
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

#' @title Directed metadata graph construction for Package/Command/Argument provenance description
#' @description Build a directed metadata graph describing R calls generating outputs
#' @param graph An igraph-class graph
#' @param package Character string. Package name
#' @param version Character string. Package version
#' @param fun Character string. Function name
#' @param arg.list Argument list, passed by the upper level function.
#' @param origin.node.name Name of the origin node of the previously existing graph.
#' @family transformation
#' @details This function takes as reference the semantics defined in the Seasonal Forecast Verification ontology
#' defined in the Metaclip Framework (\url{http://metaclip.predictia.es/}).
#' @references \itemize{
#' \item \href{https://docs.google.com/presentation/d/1CQyxVIj501N7VylMR9i_T_XwFDId6MDNvpPnuaLXgnI/present#slide=id.g1f8b6ebe3d_0_196}{Visual schema of the Command Step Definition}
#' }
#' @keywords internal
#' @importFrom igraph add_vertices add_edges 
#' @author D. San Martín, J. Bedia

metaclip.graph.Command <- function(graph, package, version, fun, arg.list, origin.node.name) {
    if (class(graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    pkgVersionCheck(package, version)
    if (!is.null(fun)) {
        cmd.node.name <- paste("Command", fun, randomName(), sep = ".")
        graph <- add_vertices(graph, 1, 
                              name = cmd.node.name, 
                              className = "ds:Command",
                              label = fun,
                              attr = list("prov:value" = fun))
        # Add the edge linking the verification step with the command 
        for (i in 1:length(origin.node.name)) {
            graph <- add_edges(graph, 
                               c(getNodeIndexbyName(graph, origin.node.name[i]),
                                 getNodeIndexbyName(graph, cmd.node.name)),
                               label = "ds:hadCommandCall")    
        }
        # Package ----------------------
        pkg.node.name <- ifelse(package %in% suppressMessages(knownClassIndividuals("Package")),
                                paste0("ds:", package), paste0("Package.", randomName()))
        graph <- my_add_vertices(graph = graph,
                                 nv = 1,
                                 name = pkg.node.name, 
                                 className = "ds:Package",
                                 label = package)
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, cmd.node.name),
                             getNodeIndexbyName(graph, pkg.node.name)),
                           label = "ds:fromPackage")
        # Argument and ArgumentValue
        arg.names <- names(arg.list)
        if (!is.null(arg.names)) {
            for (i in 1:length(arg.list)) {
                arg.node.name <- paste("Argument", arg.names[i], randomName(), sep = ".")
                graph <- add_vertices(graph, 1,
                                      name = arg.node.name, 
                                      className = "ds:Argument",
                                      label = arg.names[i],
                                      attr = list("prov:value" = arg.names[i]))
                graph <- add_edges(graph, 
                                   c(getNodeIndexbyName(graph, cmd.node.name),
                                     getNodeIndexbyName(graph, arg.node.name)),
                                   label = "ds:usedArgument")
                if (length(arg.list[[i]]) > 0) {
                    for (j in 1:length(arg.list[[i]])) {
                        argval.node.name <- paste0("ArgumentValue.", arg.names[i], ".", j, ".", randomName())
                        arg.val <- if (is.list(arg.list[[i]])) {
                            arg.list[[i]][[j]]
                        } else {
                            arg.list[[i]][j]
                        }
                        if (is.function(arg.val)) {
                            arg.val <- paste(deparse(arg.val), collapse = "")
                            arg.val <- gsub("\"","'", arg.val)
                        }
                        tipo <- as.character(typeof(arg.val))
                        arg.val <- gsub("\n", "-", arg.val)
                        if (is.null(arg.val) | length(arg.val) == 0) arg.val <- "NULL"
                        graph <- add_vertices(graph, 1, name = argval.node.name, 
                                              className = "ds:ArgumentValue",
                                              label = arg.val,
                                              attr = list("prov:value" = arg.val,
                                                          "ds:withDataType" = tipo))
                        ### Revisar este nodo de donde a donde va el valor del argumento
                        graph <- add_edges(graph,
                                           c(getNodeIndexbyName(graph, arg.node.name),
                                             getNodeIndexbyName(graph, argval.node.name)),
                                           label = "ds:hadArgumentValue")
                    }
                } else {
                    argval.node.name <- paste("ArgumentValue", arg.names[i], randomName(), sep = ".")
                    graph <- add_vertices(graph, 1, name = argval.node.name, 
                                          className = "ds:ArgumentValue",
                                          label = "NULL",
                                          attr = list("prov:value" = "NULL",
                                                      "ds:withDataType" = typeof(arg.list[[i]])))
                    graph <- add_edges(graph,
                                       c(getNodeIndexbyName(graph, arg.node.name),
                                         getNodeIndexbyName(graph, argval.node.name)),
                                       label = "ds:hasArgumentValue")
                }
            }
        }
    }
    return(graph)
}
