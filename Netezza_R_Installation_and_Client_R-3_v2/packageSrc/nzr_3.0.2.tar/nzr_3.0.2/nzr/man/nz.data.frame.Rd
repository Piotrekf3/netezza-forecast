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
\name{nz.data.frame}
\alias{nz.data.frame}
\alias{is.nz.data.frame}
\title{Create a nz.data.frame}
\description{
  This function creates a pointer to a table located on Netezza. The pointer
  can later be used to run data transformations using the \code{nzApply} or
  \code{nzRun} functions or for data mining algorithms. It does not store
  any data in local memory, but aggregates metadata used to determine the
  exact table subset (columns - \code{SELECT} clause; and/or rows - 
  \code{WHERE} clause)  that the user code is run on. 
}
\usage{
nz.data.frame(table)

as.data.frame(x, row.names=NULL, optional=FALSE, max.rows=NULL, order.by=TRUE, ...)

is.nz.data.frame(x)
}
\arguments{
  \item{table}{Name of a table available in the current database on the Netezza appliance.}
  \item{x}{An \code{nz.data.frame} object.}
  \item{max.rows}{Maximal rows number to be transfered to the client.}
  \item{order.by}{Denotes whether ordering should be used in the operation
    (using the \code{rowid} column).}
  \item{row.names}{Not used; included for compatibility.}
  \item{optional}{Not used; included for compatibility.}
}
\details{

  The argument table must be a valid NPS table name and the table must exist.
  
  If the database is schema enabled, the schema can be specified along with
  the table name using the default SQL notation (MYSCHEMA.MYTABLE). If no
  schema is specified, the current schema is used.
  
  If the data base is not schema enabled, any specified schema will be ignored.  	
  
  If schema or table are set in quotes, they will be treated case sensitive otherwise
  they are automatically converted to the default schema of the database. Columns
  are always treated case sensitive. 
  
  The table meta data is read from Netezza. 
  A subset of columns and/or rows may be specified
  using the indexing operator \code{[]} (which is translated to the
  \code{SELECT} clause for columns and/or the \code{WHERE} clause for
  rows). Note that columns are treated case sensitive.

  One limitation is that rows cannot be selected using their numbers
  since continuous row numbering is not used on the NPS. Instead, you
  must specify value-based conditions, for example \code{d[,1] > 10}
  which means \dQuote{all rows where the value of the first column is
  greater than 10}. The \code{$} operator may be also used to select
  a \code{nz.data.frame} column.

  \code{is.nz.data.frame} checks if the given object's class is \code{nz.data.frame}.

}
\value{
  A \code{nz.data.frame} object.

  \code{is.nz.data.frame} returns a logical value.
}
\seealso{\code{\link{nzRun}}, \code{\link{nzApply}},
 \code{\link{as.nz.data.frame}}, \code{\link{nzConnect}},
 \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}},
 \code{\link{length}}, \code{\link{head}}, \code{\link{tail}}}
\examples{
nzConnectDSN('NZSQL')
netezza.iris <- as.nz.data.frame(iris, 'iris2', clear.existing=TRUE,case.sensitive=T)

d <- nz.data.frame('iris2',case.sensitive=T) # creating a frame basing on a Netezza table
regular.df <- as.data.frame(d) # now the same as iris
another.df <- as.data.frame(d[d[,1] > 5, 1]) # a subset of iris

#Table only, in a schema enabled database, the current schema is added automatically 
nzdf <- nz.data.frame('IRIS');

#Schema and table in a schema enabled database
nzdf2 <- nz.data.frame('A.IRIS');

#Schema name is case sensitive
nzdf3 <- nz.data.frame('"Ax".IRIS');


nzDisconnect()
}
