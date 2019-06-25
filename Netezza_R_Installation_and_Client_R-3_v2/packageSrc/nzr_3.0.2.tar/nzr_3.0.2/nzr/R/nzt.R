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
# 

nzt <- function(x, col.names=NULL) {
	n <- names(x) <- toupper(names(x))
	m <- c("COLUMNID", "VALUE")
	if (length(intersect(n, m)) != length(m))
		stop("input data frame does not have required columns")

	cn <- max(x$COLUMNID) + 1

	if (is.null(col.names))
		col.names <- paste("X", seq(cn), sep="")
	else if (length(col.names) != cn)
		stop("names vector length different than `x` columns number")

	ret <- data.frame(matrix(NA, nrow(x)/cn, cn))
	names(ret) <- col.names

	for (i in seq(cn)) {
		ret[,i] <- x$VALUE[x$COLUMNID == (i-1)]
		# convert strings to numeric
		if (length(grep("([0-9]*\\.[0-9]+|[0-9]+)", ret[,i])) == length(ret[,i]))
			ret[,i] <- type.convert(as.character(ret[,i]))
	}

	return(ret)
}
