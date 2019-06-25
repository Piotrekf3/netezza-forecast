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
\name{nzApply}
\alias{nzApply}
\title{Apply a function to data}
\description{
  This function applies a user-provided function to each row of a given
  distributed data frame, that is an \code{nz.data.frame}. For each
  processed row it expects at most one result row (a \code{vector} or a
  \code{list}) which is inserted into the output table (returned as
  a \code{nz.data.frame}, if \code{output.name} is provided), or
  returned as a regular \code{data.frame}.
}
\usage{
nzApply(X, MARGIN, FUN, output.name = NULL, output.signature = NULL,
  clear.existing = FALSE, case.sensitive=FALSE, output.distributeOn=NULL, cappend = NULL, ...)
}
\arguments{
  \item{X}{Input data frame.}
  \item{MARGIN}{Currently not used, a NULL value should be passed here.}
  \item{FUN}{A user-defined function object.}
  \item{output.name}{Optional: name of the output table created on Netezza.}
  \item{output.signature}{Optional: data types for the output table columns;
    if not provided, a generic (sparse) table is created.}
  \item{clear.existing}{Denotes whether the output table, if it currently
    exists, should be deleted. If set to \code{TRUE}, and the Netezza table
	specified by the argument exists, that table is dropped.}
  \item{case.sensitive}{Denotes if the output column names should be case-sensitive or not.}
  \item{output.distributeOn}{Optional: Column(s) along which to distribute the resulting table}
  \item{chunkSize}{Optional: the number of rows to process at once.}
  \item{\dots}{Additional arguments to be passed to \code{fun}.}
  
}
\details{
  \code{FUN} is a user-provided function object, which accepts at
  least one parameter \code{x} of class \code{list}
  (\code{FUN <- function(x){...}}). This argument at each invocation
  of \code{FUN} contains only one row of input data.
  
  The behavior of parameter \code{x} depends on whether individual rows are
  processed or rows in larger chunks. With a chunk size larger 1,
  rows are processed in batches which is more efficient, however,
  all types are casted to the most general one, for instance character.
  
  If \code{chunkSize=1} then rows are processed individually and types
  are preserved.
  
  The elements of \code{x} do not have names and must be accessed by
  their indices: \code{x[[i]]} where \code{i} takes values from
  \code{0} to \code{inputColumnCount()-1}.

  If \code{output.name} is not provided, then the results are returned
  to the client, and no table is created on Netezza.
  The \code{output.signature} parameter may be used to set the desired
  output column names and types, avoiding a sparse table as output; if
  \code{output.signature} is provided, \code{FUN} must return values
  conforming to these types.
  
  The option \code{cappend} present in earlier version of this function is
  no longer supported. To achieve similar behavior, add the input
  columns to the output of the user function.
  
}
\value{
  An object the \code{nz.data.frame} class.
}
\seealso{\code{\link{nzRun}},
         \code{\link{nz.data.frame}}, \code{\link{output.signature}}}
\examples{
# Given that some_table exists on NPS, and that it has at least one
# (first) column of type `double'.

nzConnectDSN('NZSQL')
if (nzExistTable('iris')) { d<-nz.data.frame('iris') } else { d<-as.nz.data.frame(iris) }

f <- function(x) { return(sqrt(x[[1]])) }
r <- nzApply(d[,1], NULL, f, output.name='apply_output',
       output.signature=list(squareroot=NZ.DOUBLE), clear.existing=TRUE)

# this exists also as a overloaded apply method and the following
# returns the save result
r <- apply(d[,1], NULL, f, output.name='apply_output',
       output.signature=list(squareroot=NZ.DOUBLE), clear.existing=TRUE)

nzDisconnect()
}
\keyword{manip}
