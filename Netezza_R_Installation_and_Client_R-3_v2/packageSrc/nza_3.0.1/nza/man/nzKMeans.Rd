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
\name{nzKMeans}
\Rdversion{1.0}
\alias{nzKMeans}
\alias{predict.nzKMeans}

\title{Wrapper for KMeans cluster algorithm}
\description{
	This function is a wrapper for the \code{nza..KMEANS} stored procedure.
	All computations are performed on the NPS, the results are converted to a R object.
	
	The fitted model may be stored in an object of class \code{kmeans}.
	Overloaded functions such as \code{print} from the package \code{stats} and \code{plot} 
	from this package may be used.
	
	A local function that computes the K-Means clusters is \code{stats::kmeans}.
}
\usage{
nzKMeans(
	data,
	k=2,
	maxiter=10,
	distance="euclidean",
	outtable = NULL,
	id="id", 
	getLabels=F,
	randseed=1234,
	format=c("kmeans", "raw"),
	model=NULL,
	dropAfter=F,
	...
)

\method{print}{nzKMeans}(object, ...)
\method{predict}{nzKMeans}(object, newdata, id="id", ...)
}
\arguments{
	\item{data}{A \code{nz.data.frame} storing the data.}
	\item{k}{The number of clusters.}
	\item{maxiter}{The maximum number of iterations to be used for the k-means algorithm.}
	\item{distance}{The distance function to be used. Possible values are \code{"canberra"}, \code{"euclidean"},\code{"manhattan"}, and \code{"maximum"}.}
	\item{outtable}{The name of the output table containing the results of the operation. Will be set automatically if not specified.}
	\item{id}{The name of the column that contains unique IDs.}
	\item{getLabels}{Denotes whether to add a cluster label to the output table, i.e. whether to download the cluster assignment for every item in \code{data}.}
	\item{randseed}{The seed for the random number generator.}
	\item{format}{Denotes the output format. Either \code{"kmeans"} for \code{kmeans} like output or \code{"raw"} for downloading all results as \code{data.frame}s.}
	\item{model}{The modelname. Will be set automatically if not specified.}
	\item{dropAfter}{Whether to drop the results after downloading them.}
	\item{object}{Fitted model of class \code{nzKMeans}.}
	\item{newdata}{A pointer to the nz.data.frame table that stores the new data.}
	\item{...}{Further arguments that will be passed to other functions. See details.}
}
\details{
	The function \code{nzKMeans} will pass the \code{...} argument to the stored procedure \code{nza..KMEANS}.
	The function \code{print.nzKMeans} will pass the \code{...} argument to \code{print}.
	The function \code{predict.nzKMeans} will pass the \code{...} argument to the stored procedure \code{nza..PREDICT_KMEANS}.
	
	The function {predict.nzKMeans} is an overloaded wrapper for \code{nza..PREDICT_KMEANS}.
	There is a plot function for objects of class \code{nzKMeans} available. Check section 'See also'.
	
	For more details on the KMeans algorithm and requirements to the \code{data}, please refer to
	the documentation of \code{nza..KMEANS} in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
}
\value{
	The function \code{nzKMeans} will return an object of class \code{kmeans} if \code{format} was \code{"kmeans"}.
	If \code{format} was \code{"raw"} it will return a list of \code{data.frame}s.

	The function \code{predict} will return a \code{nz.data.frame} that stores the prediction results in a
	\code{ID, CLUSTER_ID, DISTANCE} format.
}
\seealso{\code{\link{plot.nzKMeans}}, \code{\link{nzKMeans}}}

\examples{\dontrun{
	library(nza) 
	
	# NZSQL - ODBC connection id, has to be defined locally
	# on your system using "ODBC_Source_Administrator"
	# nzConnectDSN('NZSQL')

	nziris <- nz.data.frame("iris") 
	t1 <- nzKMeans(nziris, id="ID") 
	t2 <- nzKMeans(nziris, k=10, id="ID") 
	t3 <- nzKMeans(nziris, k=10, distance="manhattan", id="ID") 
	t4 <- nzKMeans(nziris, k=10, id="ID", getLabels=T) 
	preds <- predict(t1, nziris, id="ID")
	head(preds)
	
	# nzDisconnect()
}}
