% 
% Copyright (c) 2010, 2011, IBM Corp. All rights reserved. 
% 		
% This program is free software: you can redistribute it and/or modify 
% it under the terms of the GNU General Public License as published by 
% the Free Software Foundation, either version 3 of the License, or 
% (at your option) any later version. 
%
% This program is distributed in the hope that it will be useful, 
% but WITHOUT ANY WARRANTY; without even the implied warranty of 
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
% GNU General Public License for more details. 
%
% You should have received a copy of the GNU General Public License 
% along with this program. If not, see <http://www.gnu.org/licenses/>. 
% 
% 
\name{nzGroupedApply}
\alias{nzGroupedApply}
\title{Aggregate grouped data}
\description{
  This function is a client-side interface to the Netezza \emph{User
  Defined Aggregate} (UDA) interface, which provides state-aware data
  aggregation. The data can be grouped by an arbitrary subset of the
  input table columns.
}
\usage{
nzGroupedApply(X, init, accum, merge, final, groupBy, ...)
}
\arguments{
  \item{X}{The input \code{nz.data.frame}.}
  \item{init}{Initialize state}
  \item{accum}{Accumulate rows}
  \item{merge}{Merge intermediate results}
  \item{final}{Calculate final results}
  \item{groupBy}{Grouping columns}
  \item{\dots}{These arguments are passed to each of the above functions.}
}
\details{
  This function has the same semantics as UDA (User-Defined Aggregate)
  interface present in Netezza. The four functions passed in as the
  arguments are called when respective UDA methods would be called.

  When compared with \code{nzRun}, the aggregation server-side API has two
  additional functions: \code{getOutput} which enables reading the
  output/state values, and \code{getState} which returns the current
  state identifier. There are also four state identifiers:
  \code{NZ.INIT}, \code{NZ.ACCUM}, \code{NZ.MERGE}, and \code{NZ.FINAL}.

  In the UDA, instead of regular input and output, you can access the 
  input state variables (the \emph{initialization} state), the input row
  and input state variables (the \emph{accumulate} state), or the
  input/output state variables (the \emph{merge} and \emph{final result}
  states). In each state the same functions are used to access the data:
  \code{getInputColumn} for the input data/state, and \code{setOutput},
  \code{setOutputNull}, and \code{getOutput} for the output state.

  In the \emph{initialization} state there is no input and the state
  variables are accessed via the output API .
  In the \emph{accumulate} state, the input row is accessed via the input
  API, and the state variables are accessed via the output API.
  In the \emph{merge} and \emph{final result} states, the input and
  output state variables are accessed via the input and output API,
  respectively.

  Since the state and output structures defined in R can take any arbitrary
  from and are defined dynamically, they are stored as \code{VARCHAR}s. This
  means that the state must be serialized prior to being stored, as do the
  output variables.
  Serialization and deserialization is hidden in two functions present in
  the server-side API: \code{store} and \code{restore}. The \code{store}
  function accepts any number of variables of any type and stores them in
  a \code{list}, which is then serialized. The \code{restore} function
  reads the serialized state and deserializes it. If there is only one
  element in the \code{list}, it returns this element directly, instead
  of having it wrapped in a \code{list} object.
}
\value{
  A \code{list} with one entry for each group. Each entry consists of a
  \code{value}, which carries the result of invocation of the \code{final}
  function and a \code{group}, which stores the values of columns used in
  the \code{groupBy} clause.
}
\seealso{\code{\link{nzt}}, \code{\link{nz.data.frame}}}
\examples{
nzConnectDSN('NZSQL')
d <- as.nz.data.frame(iris, 'iris2', clear.existing=TRUE, case.sensitive=T)
init <- function() { store(rep(0,5)) }

accum <- function() {
	x <- c()
	for(i in seq(4))
		x[i] <- getInputColumn(i-1)
	x[5] <- 1
	store(restore()+as.numeric(x))
}

merge <- function() {
	store(restore()+as.numeric(restore(NZ.INPUT)))
}

final <- function() {
	x <- restore(NZ.INPUT)
	store(x)
	store(x[1:4]/x[5])
}

r <- nzGroupedApply (d, init=init, accum=accum, merge=merge,
       final=final, groupBy='Species')

# correct values are (and the actual output should be similar to):
#  virginica  : 6.588 2.974 5.552 2.026
#  versicolor : 5.936 2.770 4.260 1.326
#  setosa     : 5.006 3.428 1.462 0.246

`nzkmeans` <- function(x, k, iter.max=10) {
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
                retc <- nzGroupedApply(tmp, init, accum, merge, final, "center", ncol=length(x@cols)+1)

                for (i in seq(length(retc)))
                        centers[i,] <- retc[[i]]$value[[1]][idx]

                nzApply(x, 0, chooseCenter, centers=centers,
                        output.name=tmpname, output.signature=list(CENTER=NZ.INT32),
                        clear.existing="truncate", cappend=seq_along(x@cols))
        }

        nzDeleteTable(tmpname)

        return(centers)
}

nzDisconnect()
}
\keyword{manip}
