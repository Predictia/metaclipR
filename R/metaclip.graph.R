##     metaclip.graph Construct a directed graph for metadata encoding
##
##     Copyright (C) 2017 Predictia (http://www.predictia.es)
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

#' @title Metaclip launcher
#' @description Launch the metaclip encoding for a given process
#' @param package Character string indicating the name of the package
#' @param version Character string indicating the package version. This can be obtained via \code{\link[utils]{packageVersion}},
#' but note that the value of the argument should always refer to the version actually used for the output generation,
#'  and not the currently installed version (that may differ).
#' @param fun Character string indicating the name of the function called 
#' @param arg.list A list of arguments passed to \code{fun}
#' @param output The output returned by \code{fun} with the specified arguments  
#' @return A directed graph of class \code{\link[igraph]{igraph}}
#' @author D. San Martin, J. Bedia
#' @export

metaclip.graph <- function(package, version, fun = NULL, arg.list = NULL, output = NULL) {
    ver <- package_version(version)
    if (package == "easyVerification") {
        if (ver != package_version("0.4.1")) {
            stop("'easyVerification' package version 0.4.1 is required", call. = FALSE)
        }
        graph <- metaclipR.easyVerification(version = version, package = package, fun = "veriApply", arg.list = arg.list)
    }
    else if (package == "transformeR") {
        if (ver < package_version("1.1.0") | ver > package_version("1.1.1")) {
            stop("'transformeR' package version >= 1.1.0 and <= 1.1.1 is required", call. = FALSE)
        }
        # graph <- metaclip.graph.transformeR(version, arg.list, output)
    } else if (package == "loadeR") {
        if (ver < package_version("1.1.0") | ver > package_version("1.1.1")) {
            stop("'loadeR' package version >= 1.1.0 and <= 1.1.1 is required", call. = FALSE)
        }
        graph <- metaclipR.DatasetSubset(output)
    }
    #     
    # } else if (package == "visualizeR") {
    #     
    # }
    return(graph)
}



