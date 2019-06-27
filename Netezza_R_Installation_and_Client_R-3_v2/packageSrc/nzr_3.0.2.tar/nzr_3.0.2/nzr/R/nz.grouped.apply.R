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
# init <- function() { store(rep(0,5)) }
# 
# accum <- function() {
#   x <- c()
#   for(i in seq(4))
#     x[i] <- getInputColumn(i-1)
#   x[5] <- 1
#   store(restore()+as.numeric(x))
# }
# 
# merge <- function() {
#   store(restore()+as.numeric(restore(NZ.INPUT)))
# }
# 
# final <- function() {
#   x <- restore(NZ.INPUT)
#   store(x)
#   store(x[1:4]/x[5])
# }
# 
# r <- nzGroupedApply (x, init=init, accum=accum, merge=merge, final=final, groupBy='Species')
#
# correct values are:
#  virginica  : 6.588 2.974 5.552 2.026
#  versicolor : 5.936 2.770 4.260 1.326
#  setosa     : 5.006 3.428 1.462 0.246
#

nzGroupedApply <- function(X, init=function(){}, accum=function(){}, merge=function(){}, final=function(){}, groupBy=NULL, ...) {

	#  MAKE SURE WE HAVE A nz.data.frame.
	if (!is.nz.data.frame(X))
		stop("X is not a nz.data.frame.")

	# prepare functions
	funcs <- list(init=init, accum=accum, merge=merge, final=final)
	for (n in names(funcs)) {
		if (!is.function(funcs[[n]]))
			stop(n, "is not a function")
		else
			environment(funcs[[n]]) <- emptyenv()
	}

	funcs$args <- list(...)

	gpby <- ifelse(is.null(groupBy), '', paste(paste('\"',groupBy,'\"',sep=""), collapse=","))

	fname <- nz.placefile(output.name="aggregation_%sid", data=rawToChar(serialize(funcs, NULL, ascii=TRUE)))
	res   <- nzQuery("SELECT ",
				# columns
				"nzr..r_uda(", paste(paste('\"',X@cols,'\"',sep=""), collapse=","), ")", ifelse(nchar(gpby), paste(',',gpby), ''),
				# from
				" FROM ", nzdf.qualified.name(X),
				# where
				ifelse(nchar(X@where), paste(" WHERE", X@where), ""), " ",
				# group by
				ifelse(is.null(groupBy), "", paste("GROUP BY", gpby)))

	rlist <- list()
	for (r in seq(nrow(res)))
		if (is.character(res[r,1]))
			rlist[[r]] <- list(value=unserialize(charToRaw(res[r,1])), group=res[r,2:ncol(res)])
		else
			rlist[[r]] <- list(value=res[r,1], group=res[r,2:ncol(res)])

	return(rlist)
}

