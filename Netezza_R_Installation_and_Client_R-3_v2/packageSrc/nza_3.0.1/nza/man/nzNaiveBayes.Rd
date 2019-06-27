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
\name{nzNaiveBayes}
\alias{nzNaiveBayes}
\alias{predict.nzNaiveBayes}
\title{Wrapper for nzNaiveBayes}

\description{
	This function is a wrapper for the \code{nza..NAIVEBAYES} stored procedure.
	All computations are performed on the NPS, the results are converted to a R object.
	
	The fitted classifier is stored in an object of the class \code{naiveBayes} from
	\code{e1071} package, that provides a local Naive Bayes implementation in \code{e1071::naiveBayes}.
}
\usage{
nzNaiveBayes <- function(
	form,
	data,
	id="id",
	relative=F,
	model=NULL,
	format="naiveBayes",
	dropAfter=F,
	...)

\method{predict}{nzNaiveBayes}(object, newdata, id="id", ...)
}
\arguments{
	\item{form}{A \code{formula} object that describes the model to fit.}
	\item{data}{A \code{nz.data.frame} object.}
	\item{id}{The name of the column that contains unique IDs.}
	\item{relative}{Denotes whether values should be returned as absolute or relative values.}
	\item{model}{Name for the model. Will be created automatically unless specified otherwise.}
	\item{format}{Desired output format of the tree, either \code{"naiveBayes"} or \code{"raw"}.}
	\item{dropAfter}{Whether to drop all created tables after all results have been fetched.}
	\item{object}{Fitted model of the nzNaiveBayes class.}
	\item{newdata}{A pointer to the nz.data.frame table.}
	\item{...}{Further to be passed to other functions. See details.}
}
\details{
	The function \code{nzNaiveBayes} will pass \code{...} to the stored procedure \code{nza..NAIVEBAYES}.
	The function \code{predict} will pass \code{...} to the stored procedure \code{nza..PREDICT_NAIVEBAYES}.
	
	The function {predict.nzNaiveBayes} is an overloaded wrapper for \code{nza..PREDICT_NAIVEBAYES}.
	
	For more details on the Naive Bayes algorithm and requirements to the \code{data}, please refer to
	the documentation of \code{nza..NAIVABAYES} in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
}
\value{
	The function \code{nzNaiveBayes} returns an object of class \code{nzNaiveBayes} if \code{format} is \code{"naiveBayes"}
	or a \code{list} of \code{data.frame}s if format is	\code{"raw"}.
	
	The function \code{predict} returns a \code{nz.data.frame} that stores the prediction results.
}
\examples{\dontrun{
	library(nza) 
 
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL')
	
	adult.train <- nz.data.frame("adult_train")
	adult.test <- nz.data.frame("adult_test")
	
	nbModel <- nzNaiveBayes(INCOME~WORKCLASS+EDUCATION+RACE+SEX+INCOME, adult.train, id="ID", relative=T)
	print(nbModel)
	
	nbRaw <- nzNaiveBayes(INCOME~WORKCLASS+EDUCATION+RACE+SEX+INCOME, adult.train, id="ID", relative=T, format="raw")
	names(nbRaw)
	
	nbPred <- predict(nbModel, adult.test, id="ID")
	head(nbPred)
}}
