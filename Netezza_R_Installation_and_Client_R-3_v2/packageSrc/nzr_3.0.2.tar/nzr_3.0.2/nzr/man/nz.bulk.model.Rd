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
\name{nzBulkModel}
\alias{nzBulkModel}

\title{Create a set of models in database using an index column.}
\description{
  This function creates a set of models and stores them in a 
  data base table. The original table is split using an index
  column and for each group, a user defined modeling function
  is applied.
}
\usage{
nzBulkModel(nzdf,INDEX,modelingFun, output.name, clear.existing=FALSE, indexType="character");
}

\arguments{
  \item{nzdf}{a nz.data.frame to which to apply the function}
  \item{INDEX}{a column for grouping}
  \item{modelingFun}{a user defined function to create models that takes a single parameter of type data.frame}
  \item{output.name}{name of the output table}
  \item{clear.existing}{drop the output table in case it exists}
  \item{indexType}{the type of index column to be used, either character or integer}
  
}
\details{
	Please not that the sub data set associated with each index should not be too large as it is loaded into
	main memory before processing.	

}
\value{
  An \code{nz.list} object.
}

\examples{
nzConnectDSN('NZSQL')

#Build three models according to the three classes of the IRIS data set
nzl <- nzBulkModel(nz.data.frame('IRIS'),'CLASS',function(df){lm('sepallength~sepalwidth',data=df);}, output.name="IRISBULK");

#Retrieve the model for group setosa
lm1 <- nzl["setosa"];

nzDisconnect()
}
