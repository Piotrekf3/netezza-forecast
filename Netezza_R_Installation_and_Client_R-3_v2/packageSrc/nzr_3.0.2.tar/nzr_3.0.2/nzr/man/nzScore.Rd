% 
% Copyright (c) 2013, 2014, IBM Corp. All rights reserved. 
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
\name{nzScore}
\alias{nzScore}

\title{Score an R model}
\description{
  This function scores a local R model on new data in the database.
}
\usage{
nzScore(nzdf, model, output.name, idCol, output.signature=NULL, clear.existing=T, requiredPkg=NULL, batchSize=10000, ...);

}
\arguments{
  \item{nzdf}{The table containing the new data to score.}
  \item{model}{The model to apply.}
  \item{output.name}{The resulting table.}
  \item{idCol}{The name of the column containing the row ids.}
  \item{output.signature}{The signature of the resulting table.}
  \item{clear.existing}{Clear if exists.}
  \item{requiredPkg}{Package that needs to be loaded to score the model.}
  \item{batchSize}{Numbers of rows to score at once.}
}
\details{

	\code{nzScore} allows to score local R objects on new data. It returns a table with an ID column,
	a prediction column and, optionally, a columns for standard error. The user needs to provide
	an output signature compatible with the model type predicted. The \code{requiredPkg} parameter might
	be NULL if no extra packages need to be loaded. For some model type, e.g. tree, \code{nzScore} will
	set this parameter automatically if it is NULL. 
}
\value{
  A \code{nz.data.frame} object pointing to the scored table. 

}

\examples{

nzdf <- nz.data.frame('IRIS')
irisnz <- as.data.frame(nzdf)

lmiris <- lm('SEPALLENGTH~SEPALWIDTH',irisnz)

sc <- nzScore(nzdf,lmiris,output.name='IRISSCNEW2','ID',output.signature=list(ID=NZ.INT32,PRED=NZ.DOUBLE,SE=NZ.DOUBLE),clear.existing=T,se.fit=T)


}
