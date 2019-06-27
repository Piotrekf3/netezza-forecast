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
\name{nzDecTree}
\Rdversion{1.0}
\alias{nzDecTree}
\alias{plot.nzDecTree}
\alias{print.nzDecTree}
\alias{predict.nzDecTree}

\title{Wrapper for decision tree algorithm (DecTree)}
\description{
	This function is a wrapper for the \code{nza..DECTREE} stored procedure.
	All computations are performed on the NPS, the results are converted to a R object.	
	
	The fitted classifier may be stored in an object that imitates \code{tree} or \code{rpart} objects.
	Overloaded functions such as \code{print} and \code{plot} can be used for these objects.
	
	A local way of computing decision trees may be found in \code{tree::tree} or \code{rpart::rpart}.
}
\usage{
nzDecTree(
	form,
	data,
	minsplit=1000,
	maxdepth=62,
	id="id",
	qmeasure="wAcc",
	minimprove=0.01,
	eval="entropy",
	valtable=NULL,
	model=NULL
	format="tree",
	...
)

\method{plot}{nzDecTree}(object, ...)
\method{print}{nzDecTree}(object, ...)
\method{predict}{nzDecTree}(object, newdata, id="id", ...)
}
\arguments{
	\item{form}{A \code{formula} object that describes the model to fit.}
	\item{data}{A \code{nz.data.frame} object that is a pointer to a table on the NPS where the variables called in \code{form} are stored.}
	\item{minsplit}{The minimum number of instances that can be split.}
  	\item{maxdepth}{The maximum depth of the tree.}
	\item{id}{The column in the table containing unique IDs.}
	\item{qmeasure}{The stop criterion for tree building.}
	\item{minimprove}{The minimum improvement.}
	\item{eval}{Evaluation measure used as splitting criteria.}
	\item{valtable}{Table used for tree trimming, will use the table from \code{data} unless specified otherwise.}
	\item{model}{Name for the model. Will be created automatically unless specified otherwise.}
	\item{format}{Desired output format of the tree, either "tree" or "rpart".}
	\item{object}{A model of the \code{nzDecTree} class.}
	\item{newdata}{A pointer to a \code{nz.data.frame} table containing new data.}
	\item{...}{Further arguments that will be passed to the stored procedure.}
}
\details{
	This function will return an object of class \code{nzDecTree}, that my be
	converted to an object of either class 'tree' or 'rpart' by changing it's 'class' attribute,
	depending on the selected \code{format}.
	
	In both cases printing/plotting methods already exist and are overloaded. They will be called
	by \code{plot.nzDecTree} and \code{print.nzDecTree}.
	
	The function {predict.nzDecTree} is an overloaded wrapper for \code{nza..PREDICT_DECTREE}.
	
	The function \code{nzDecTree} will pass the \code{...} argument to \code{nza..DECTREE}.
	The function \code{plot.nzDecTree} will pass the \code{...} argument to \code{plot}.
	The function \code{print.nzDecTree} will pass the \code{...} argument to \code{print}.
	The function \code{predict.nzDecTree} will pass the \code{...} argument to \code{nza..PREDICT_DECTREE}.
	
	For more details on the DECTREE algorithm and requirements to the \code{data}, please refer to
	the documentation of \code{nza..DECTREE} in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
}
\value{
	The function \code{nzDecTree} returns a \code{nzDecTree} object.
	The function \code{predict.nzDecTree} returns a \code{nz.data.frame}.
}
\seealso{\code{\link{nzRegTree}}}

\examples{\dontrun{
	library(nza)
	 
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	adult <- nz.data.frame("adult") 
	tr <- nzDecTree(INCOME~WORKCLASS+EDUCATION+EDUCATION_NUM+RACE+CAPITAL_GAIN+AGE, data=adult, minsplit=50, maxdepth=10, id="ID")
	preds <- predict(tr, adult, "ID")
	plot(tr)
	print(tr)
	head(preds)
	
	# nzDisconnect()
}}
