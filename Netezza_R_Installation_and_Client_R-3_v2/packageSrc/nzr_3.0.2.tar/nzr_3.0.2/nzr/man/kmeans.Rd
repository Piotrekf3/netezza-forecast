% 
% Copyright (c) 2010, 2014, IBM Corp. All rights reserved. 
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
\name{nzkmeans}
\alias{nzkmeans}
\title{Perform k-means clustering}
\description{
  This function is an R implementation of the k-means clustering algorithm.
  It should be considered rather an example of using \code{nzGroupedApply},
  than a mature clustering implementation.
}
\usage{
nzkmeans (x, k, iter.max=10)
}
\arguments{
	\item{x}{Object of \code{nz.data.frame} class or a string containing
	  the table name.}
	\item{k}{The number of groups to be created.}
	\item{iter.max}{Maximal number of iterations.}
}
\details{
  This algorithm runs on Netezza, where it creates a temporary table
  with centroid assignments. A matrix with all calculated cluster means
  (centroids) is returned.

  This function is intended as an example of using the UDA R interface,
  rather than a full-fledged clustering algorithm implementation.
}
\value{
	A matrix containing one centroid in each row.
}
\examples{
# connect etc.
nzConnectDSN('NZSQL')

x<-as.nz.data.frame(iris,'iris2',clear.existing=T,case.sensitive=T)
centers <- nzkmeans(x[,1:4], 3, 50)

# all possible centroids permutations
prms <- matrix(c(1,2,3,1,3,2,2,1,3,2,3,1,3,2,1,3,1,2), 6, 3, byrow=TRUE)

# chooses best centroid for the given input row
bestC <- function(x, centers) {
	X <- x
	which.min(apply(centers, 1, function(x) dist(rbind(x,X))))
}

# computes the mismatch error for the whole table
err <- function(x) {
	appl <- apply (iris[,1:4], 1, bestC, centers=centers[x,])
	sum(abs(appl-as.integer(iris$Species)))
}

# should be around 17 - which means around 10\% error
finalError <- min(apply(prms, 1, err))

nzDisconnect()
}
