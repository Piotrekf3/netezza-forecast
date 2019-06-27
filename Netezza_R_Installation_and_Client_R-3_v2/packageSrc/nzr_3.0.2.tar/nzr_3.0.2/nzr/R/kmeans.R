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

# Plain R implementation of the k-means algorithm.
#
# Since centroids may be returned in any order, to find theirs best
# permutation (in case when 'iris' is the input table) you may run:
#
# centers <- nzkmeans ('iris', .....)
#
# # all possible centroids permutations
# prms <- matrix(c(1,2,3,1,3,2,2,1,3,2,3,1,3,2,1,3,1,2), 6, 3, byrow=TRUE)
#
# # chooses best centroid for the given input row
# bestC <- function(x, centers) {
#    X <- x
#    which.min(apply(centers, 1, function(x) dist(rbind(x,X))))
# }
#
# # computes the mismatch error for the whole table
# err <- function(x) {
#   appl <- apply (iris, 1, bestC, centers=centers[x,])
#   sum(abs(appl-as.integer(iris$Species)))
# }
#
# finalError <- min(apply(prms, 1, err))
#
#

nzkmeans <- function(x, k, iter.max=10) {
	if ((!is.character(x) || !nzExistTable(x)) && !is.nz.data.frame(x))
		stop("table name or nz.data.frame has to be given")

	k <- as.integer(k)
	if (k < 1)
		stop("a positive integer number of centroids has to be given")

	if (is.character(x))
		x <- nz.data.frame(x)

	tmpname <- paste("kmeans", x@table, sep="_")
	tmp <- nzApply(x, 0, function(x, k) as.integer(ceiling(runif(1, 0, k))),
			output.name=tmpname, output.signature=list(CENTER=NZ.INT32),
			clear.existing=TRUE, cappend=seq_along(x@cols), k=k)

	# pick the closest center
	chooseCenter <- function (x, centers) {
		X <- x
		C <- which.min(apply(centers, 1, function(x) dist(rbind(x,X)) ))
		return(as.integer(C))
	}

	# calculating groups centers
	init <- function(ncol) { store(rep(0,ncol+1)) }
	accum <- function(ncol) {
		x <- restore()
		for(i in seq(ncol))
			x[i] <- x[i] + getInputColumn(i-1)
		x[ncol+1] <- x[ncol+1] + 1
		store(as.numeric(x))
	}
	merge <- function(ncol) {
		store(restore()+as.numeric(restore(NZ.INPUT)))
	}
	final <- function(ncol) {
		x <- restore(NZ.INPUT)
		store(x[seq(ncol)]/x[ncol+1])
	}

	# these columns represent centers
	idx     <- seq(length(x@cols)) + 1
	centers <- matrix(0, k, length(x@cols))

	for (i in seq(iter.max)) {
		retc <- nzGroupedApply(tmp, init, accum, merge, final, "CENTER", ncol=length(x@cols)+1)

		for (i in seq(length(retc)))
			centers[i,] <- retc[[i]]$value[[1]][idx]

		nzApply(x, 0, chooseCenter, centers=centers,
			output.name=tmpname, output.signature=list(CENTER=NZ.INT32),
			clear.existing="truncate", cappend=seq_along(x@cols))
	}

	nzDeleteTable(tmpname)

	return(centers)
}
