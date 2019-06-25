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
\name{nzSingleModel}
\alias{nzSingleModel}


\title{Create a model on a whole table in the data base}
\description{
  This function creates a single model or object on a whole
  table that is passed as data frame. 
}
\usage{
nzSingleModel(nzdf,modelingFun,force=F);

}
\arguments{
  \item{nzdf}{a nz.data.frame to which to apply the function}
  \item{modelingFun}{a user defined function to create models that takes a single parameter of type data.frame}
  \item{force}{if true, the size check of the table is omitted}
}
\details{

	Note that this function can only be applied to moderate size tables, as all data is loaded onto a single node and processed in-memory.
	The default is to throw an error if this function is applied to tables with more than 100k rows. This behaviour can be
	overwritten with the force option.

}
\value{
  The object returned by the user defined modeling function.

}

\examples{
nzConnectDSN('NZSQL')

#Build a linear model on the whole table IRIS, without loading the data to the client
lm1 <- nzSingleModel(nz.data.frame("IRIS"),function(df){lm('sepallength~sepalwidth',data=df);});

nzDisconnect()
}
