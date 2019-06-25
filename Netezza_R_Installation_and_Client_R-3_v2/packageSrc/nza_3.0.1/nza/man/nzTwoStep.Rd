%
% Copyright (c) 2011, 2012, Revolution Analytics. All rights reserved.
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

\name{nzTwoStep}
\Rdversion{1.0}
\alias{nzTwoStep}
\alias{print.nzTwoStep}
\alias{predict.nzTwoStep}

\title{Wrapper for TwoStep cluster algorithm}
\description{
	This function is a wrapper for the \code{nza..TWOSTEP} stored procedure.
	All computations are performed on the NPS, the results are converted to a R object.
	
	The fitted model may be stored in an object of class \code{nzTwoStep}.
	Overloaded functions such as \code{print} and \code{plot} may be used.
}

\usage{
nzTwoStep(
	data,
	outtable=NULL,
	id="id",
	distance="loglikelihood",
	k=0,
	maxk=20,
	statistics="none",
	bins=10,
	randseed=12345,
	distancethreshold=0,
	distancethresholdfactor=2,
	epsilon=0,
	nodecapacity=6,
	leafcapacity=8,
	maxleaves=1000,
	outlierfraction=0,
	model=NULL,
	dropAfter=F,
	getLabels=F,
	format="cluster",
	...
)

\method{print}{nzTwoStep}(object)
\method{predict}{nzTwoStep}(object, newdata, id="id", ...)
}
\arguments{
	\item{data}{Input table name as \code{string} or \code{nz.data.frame} object containing the data.}
	\item{outtable}{Table name for table where item-cluster assignments are saved. Will be determined automatically if not specified.}
	\item{id}{The name of the column that contains unique IDs.}
	\item{model}{Name for the model. Will be created automatically unless specified otherwise.}
	\item{dropAfter}{Whether to drop the results after fetching them.}
	\item{getLabels}{If \code{TRUE}, cluster information will be downloaded and returned as \code{named int[]} instead of \code{nz.data.frame}s.}
	\item{format}{Specified output format. Either \code{"cluster"} for object of class \code{"nzTwoStep"} or \code{"raw"} for a list of \code{data.frame}s.}
	\item{object}{Object of class \code{nzTowStep}}
	\item{newdata}{New data as \code{nz.data.frame} for prediction}
	\item{...
	\itemize{
			\item distance
			\item k
			\item maxk
			\item statistics
			\item bins
			\item randseed
			\item distancethreshold
			\item distancethresholdfactor
			\item epsilon
			\item nodecapacity
			\item leafcapacity
			\item maxleaves
			\item outlierfraction
		}
	}{
		The function \code{nzTwoStep} will pass these arguments to \code{nza..TWOSTEP}.
		The function \code{predict} will pass these arguments to \code{nza..PREDICT_TWOSTEP}.
	}
}
\details{
	The function {predict.nzTwoStep} is an overloaded wrapper for \code{nza..PREDICT_TWOSTEP}.
	There is a plot function for objects of class \code{nzTwoStep} available. Check section 'See also'.
	
	For more details on the TwoStep algorithm and requirements to the \code{data}, please refer to
	the documentation of \code{nza..TWOSTEP} in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
}
\value{
	The function \code{nzTwoStep} will return an object of class \code{nzTwoStep} for which overloaded \code{print} and
	\code{predict} functions are available.
	The function \code{predict} return a \code{nz.data.frame} that stores the predicted values.
}
\seealso{\code{\link{plot.nzTwoStep}}, \code{\link{nzKMeans}}}

\examples{\dontrun{
	library(nza) 
	
	# NZSQL - ODBC connection id, has to be defined locally
	# on your system using "ODBC_Source_Administrator"
	# nzConnectDSN('NZSQL')

	nziris <- nz.data.frame("iris") 
	t1 <- nzTwoStep(nziris, id="ID")
	print(t1)
	str(t1$cluster) # a nz.data.frame
	
	t2 <- nzTwoStep(nziris, id="ID", getLabels=T) 
	str(t2$cluster) # named int[]
	
	t3 <- nzTwoStep(nziris, id="ID", format="raw")
	names(t3)
	
	preds <- predict(t1, nziris, id="ID")
	head(preds)
	
	# nzDisconnect()
}}
