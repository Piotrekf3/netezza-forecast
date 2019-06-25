# 
# Copyright (c) 2010, 2014, IBM Corp. All rights reserved. 
# 		
# This program is free software: you can redistribute it and/or modify 
# it under the terms of the GNU General Public License as published by 
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version. 
#
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU General Public License for more details. 
#
# You should have received a copy of the GNU General Public License 
# along with this program. If not, see <http://www.gnu.org/licenses/>. 
# 


# nz.data.frame non-core methods
#


setMethod("mean", signature(x="nz.data.frame"),
	function(x) {
		query <- paste(paste("AVG(\"", x@cols, "\") AS ", x@cols, collapse=","),
				" FROM ", nzdf.qualified.name(x))
		if (nchar(x@where)) {
			query <- paste(query, " WHERE ", x@where)
		}
		return(nzQuery ("SELECT ", query))
	}
)


setMethod("min", signature(x="nz.data.frame"),
	function(x) {
		query <- paste(paste("MIN(\"", x@cols, "\") AS ", x@cols, collapse=","),
				" FROM ", nzdf.qualified.name(x))
		if (nchar(x@where)) {
			query <- paste(query, " WHERE ", x@where)
		}
		return(nzQuery ("SELECT ", query))
	}
)


setMethod("max", signature(x="nz.data.frame"),
	function(x) {
		query <- paste(paste("MAX(\"", x@cols, "\") AS ", x@cols, collapse=","),
			" FROM ", nzdf.qualified.name(x))
		if (nchar(x@where)) {
			query <- paste(query, " WHERE ", x@where)
		}
		return(nzQuery ("SELECT ",query))
	}
)


# ---------------------------------------------------------------------

hist.nz.data.frame <- function (x, breaks="Sturges",...) {
	if (!is.nz.data.frame(x))
		stop("x is not a proper object")
	if (length(x@cols) > 1)
		stop("there can be only one column selected")

	# we can do smth with breaks if we want it run on SPUs
	# pass the rest of the arguments
	x <- materialize(x)[[1]]
	invisible(hist(x, breaks=breaks,...))
}

setMethod("hist", signature(x="nz.data.frame"), hist.nz.data.frame)

# ---------------------------------------------------------------------

cov.nz.data.frame <- function (x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman")) {
	if (length(x@cols) < 2)
		stop("not enough data columns to calculate covariance")
	return(cov(as.data.frame(x)))
}

setMethod("cov", signature(x="nz.data.frame"), cov.nz.data.frame)
setMethod("cov", signature(y="nz.data.frame"), cov.nz.data.frame)
setMethod("cov", signature(x="nz.data.frame", y="nz.data.frame"), cov.nz.data.frame)

