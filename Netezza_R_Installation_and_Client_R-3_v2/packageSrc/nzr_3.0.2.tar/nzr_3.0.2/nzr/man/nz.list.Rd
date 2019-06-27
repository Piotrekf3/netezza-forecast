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
\name{nz.list}
\alias{nz.list}
\title{Create a nz.list}
\description{
  This function creates a pointer to a table located on Netezza. The pointer
  can later be used to store and retrieve R objects using the basic 
  list operators. 
}
\usage{
nz.list(tableName, createTable=FALSE,indexType=NULL)

}
\arguments{
  \item{tableName}{Name of a table in the current database on the Netezza appliance.}
  \item{createTable}{if TRUE, create the table if it does not exist}
  \item{indexType}{if createTable=TRUE, defines the type to use of the index column, which can be "integer" or "character"}
}
\details{

	nz.list provides a simple remote list. Objects are written and retrieved using basic list operators.
	

}
\value{
  A \code{nz.list} object.

}

\examples{
nzConnectDSN('NZSQL')

#Create nz.list
nzl <- nz.list('MYOBJS')

#Store obj with ID 5
nzl['5'] <- 1:10000

#Retrieve obj with ID 5
x <- nzl['5']

#Delete obj with ID 5
nzl['5'] <- NULL;

#List all keys
names(nzl)

#Number of entries
length(nzl)

#Append one list to another, thus all key/value pairs from the 
#second list are added persistently to the first one
#attention: no check for duplicates is performed currently

nzl <- nz.list('MYOBJS')
nzl2 <- nz.list('MYOBJS2')

nzAppend(nzl,nzl2);

#Deletes the list from the data base
nzDeleteList(nzl);


nzDisconnect()
}
