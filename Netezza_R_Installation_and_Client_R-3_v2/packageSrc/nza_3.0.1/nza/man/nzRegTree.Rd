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
\name{nzRegTree}
\Rdversion{1.0}
\alias{nzRegTree}
\alias{plot.nzRegTree}
\alias{print.nzRegTree}
\alias{predict.nzRegTree}

\title{Wrapper for regression tree algorithm (RegTree)}
\description{
	This function is a wrapper for the \code{nza..REGTREE} stored procedure.
	All computations are performed on the NPS, the results are converted to a R object.
	
	The fitted classifier may be stored in an object that imitates \code{tree} or \code{rpart} objects.
	Overloaded functions such as \code{print} and \code{plot} can be used for these objects.
	
	A local way of computing regression trees may be found in \code{tree::tree} or \code{rpart::rpart}.
}
\usage{
nzRegTree(
		form,
		data,
		minimprove=0.1,
		maxdepth=62,
		minsplit=2,
		id="id",
		valtable=NULL,
		model=NULL,
		format="tree",
		dropAfter=F,
		...
)

\method{plot}{nzRegTree}(object, ...)
\method{print}{nzRegTree}(object, ...)
\method{predict}{nzRegTree}(object, newdata, id="id", ...)
}
\arguments{
	\item{form}{A \code{formula} that describes the model to fit.}
	\item{data}{A \code{nz.data.frame} object.}
	\item{minimprove}{The minimum improvement of the split evaluation measure.}
	\item{maxdepth}{The maximum depth of the tree.}
	\item{minsplit}{The minimum number of instances that can be split.}
	\item{id}{The name of the column that contains unique IDs.}
	\item{model}{Name for the model. Will be created automatically unless specified otherwise.}
	\item{format}{If \code{format} is \code{"tree"} or \code{"rpart"}, the result will be a object of class \code{tree} or \code{rpart}.
					If \code{format} is \code{"raw"}, the result will be \code{list} of \code{data.frame}s.}
	\item{dropAfter}{Whether to drop the results after downloading them as specified in \code{format}.}
	\item{object}{Fitted model of either class \code{tree} or \code{rpart}.}
	\item{newdata}{A \code{nz.data.frame} object that stores the data to be predicted.}
	\item{...}{Further arguments that will be passed to other functions. See details.}
}
\details{
	The function \code{nzRegTree} will pass the \code{...} argument to \code{nza..REGTREE}.
	The function \code{plot} will pass the \code{...} argument to \code{plot}.
	The function \code{print} will pass the \code{...} argument to \code{print}.
	The function \code{predict} will pass the \code{...} argument to \code{nza..PREDICT_REGTREE}.
	
	The function {predict.nzRegTree} is an overloaded wrapper for \code{nza..PREDICT_REGTREE}.
	
	For more details on the REGTREE algorithm and requirements to the \code{data}, please refer to
	the documentation of \code{nza..REGTREE} in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
	
}
\value{
	The function \code{nzRegTree} returns an object of either class \code{tree} or \code{rpart} or a 'raw' \code{list}
	or \code{data.frame}s as specified in \code{format}.

	The function \code{predict} returns a \code{nz.data.frame} that stores the prediction results.	
}
\seealso{\code{\link{nzDecTree}}}

\examples{\dontrun{
	library(nza) 
 
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL')
	
	weather <- nz.data.frame("weatherr") 
	rtModel <- nzRegTree(GRADE~., data=weather, id="INSTANCE", minimprove=0.1, minsplit=2, maxdepth=4) 
	plot(rtModel) 
	print(rtModel)
	
	rtPred <- predict(rtModel, weather, id="INSTANCE")
	head(rtPred)
}}
