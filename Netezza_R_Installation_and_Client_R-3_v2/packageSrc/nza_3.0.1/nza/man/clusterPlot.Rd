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
\name{clusterPlot} 
\Rdversion{1.0}
\alias{plot.nzKMeans}
\alias{plot.nzTwoStep}
 
\title{Plots for clustering algorithms}
\description{
	This function enables pair plots of
	results from clustering algorithms using the \code{lattice} package.
}
\usage{
plot.nzKMeans <- function (
		clusterModel,
		n.nearest=10,
		n.farest=10,
		select=NULL,
		points.pch=NULL,
		points.cex=NULL,
		points.col=NULL,
		cluster.names=NULL,
		cluster.pch=NULL,
		cluster.cex=NULL,
		cluster.col=NULL,
		...
)

plot.nzTwoStep <- plot.nzKMeans
}
\arguments{
	\item{clusterModel}{Result of either nzKMeans or nzTwoStep.}
	\item{n.nearest}{Number of points that are close to the centroids. See details.}
	\item{n.farest}{Number of points that are far from the centroids. See details.}
	\item{select}{Select several columns to display. See details.}
	\item{...}{See lattice::xyplot documentation.}
}
\details{
	If clusters are stored in a table (i.e. \code{getLabels} was \code{FALSE} when computing \code{clusterModel}),
	this function will fetch the \code{n.nearest} elements with the lowest and \code{n.farest} elements with the highest
	distance to the centroid from \bold{each} cluster. If one of the values equals -1, all elements will be displayed.

	The \code{select} argument should be formatted such that it can be passed to the \code{subset} command.
	It will use it as \code{select} argument. When passing numbers in order to address columns, please note,
	that the \code{id} column will already have been removed from the list of columns and that the order of columns
	will equal to the order of columns in the original data.
}
\note{
	Please also note, that when calling this from inside functions, you need to call \code{print(plot(clusterModel))}
	in order to see the plots. See \code{lattice} documentation / FAQ.
}

\examples{\dontrun{
	require(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	nz.data <- nz.data.frame("iris")

	result1 <- nzKMeans(nz.data, k=3, id="ID", getLabels=F)
	result2 <- nzTwoStep(nz.data, id="ID", getLabels=T)
	
	plot(result1, n.nearest=-1)
	plot(result2, select= 2:3)
	
	# nzDisconnect() 
}
}