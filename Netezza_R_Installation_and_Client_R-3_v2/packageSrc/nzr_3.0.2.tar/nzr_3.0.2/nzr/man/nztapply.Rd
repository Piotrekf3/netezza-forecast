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
\name{nzTApply}
\alias{nzTApply}
\title{Apply a function to table data}
\description{
  This function applies a user-provided function to a number of subsets
  of a given distributed data frame (\code{nz.data.frame}). The subsets
  are determined by a specified index column. The results from applying
  the functions is put into a Netezza table or a client-side data frame.
}
\usage{
nzTApply(X, INDEX, FUN = NULL, output.name = NULL,
         output.signature = NULL, clear.existing = FALSE, case.sensitive = FALSE,output.distributeOn=NULL,
         debugger.mode = FALSE, ..., simplify = TRUE)
}
\arguments{
  \item{X}{Input data frame.}
  \item{INDEX}{Column used to determine the input data frame subsets.}
  \item{FUN}{User-defined function.}
  \item{output.name}{Name of the output table created on Netezza.}
  \item{output.signature}{Data types for output table columns; if not
    provided, a generic sparse table is created. See
	\code{\link{output.signature}} for details.}
  \item{clear.existing}{Denotes whether the output table, if it
    currently exists, should be deleted. If \code{TRUE}, the output
	table is deleted.}
  \item{case.sensitive}{Denotes if the output columns should be case-sensitive or not.}
  \item{output.distributeOn}{Optional: Column(s) along which to distribute the resulting table}
  \item{debugger.mode}{Denotes whether the function should run in
    debug mode. If \code{TRUE}, \code{nzTApply} works in the debug mode
	(see \emph{Details}).}
  \item{\dots}{These arguments are passed to \code{FUN}.}
  \item{simplify}{Not implemented, included for compatibility only.}
}
\details{
  \code{INDEX} can be supplied as a \code{character} string whose value
  does not have to be present among columns of \code{X}, an integer not
  greater than the number of columns of \code{X}, or a one-column
  \code{nz.data.frame} whose column name again does not have to be
  present among \code{X}'s columns. If the \code{INDEX} column is not
  selected in \code{X}, then it is not passed to \code{FUN}. However, it
  must be present in the SQL table on the server side so that the input
  data can be grouped based on that column.

  Note that the last output column is always the group identifier value,
  which follows the data type of the \code{INDEX} input column. This must
  be taken into account when the \code{output.signature} is specified.
  
  The user-defined function \code{FUN} can return a scalar value or a row.
  It receives a subset of the input data in the form of a \code{data.frame},
  with column names in lower case.

  If \code{output.name} is not provided, then no table is created on
  Netezza. The \code{output.signature} parameter may be used to avoid
  receiving a sparse table and set the desired output columns types; if
  provided, \code{FUN} must return values that can be cast to
  its types. If \code{output.name} is given, then \code{output.signature}
  must be provided.

  \emph{Debug Mode}

  If \code{FUN} is causing errors, the debugger mode can be used to
  investigate the error conditions. If \code{debugger.mode} is set to
  \code{TRUE}, then the result table is not stored on Netezza, mirroring
  the functionality when \code{output.name=FALSE}. Instead, for every
  group a diagnostic test is called, and the environment for the first
  group that causes an error is sent to the local R client and opened
  in the R \code{debugger()} function.
  
  For example, consider following R code:
\preformatted{
nziris <- as.nz.data.frame(iris, clear.existing=TRUE)
FUN5 <- function(x) {
  if(min(x[,1]) < 4.5) cov(0) else min(x[,1])
}
nzTApply(nziris, 5, FUN5, debugger.mode=T)
} 

  In this example there are three groups and one of them produces an
  error. In the debug mode \code{nzTApply} returns a group processing
  summary, which is presented in a table with three columns: the outcome
  or error description, the type of outcome or \code{try-error} in the
  case of an error, and the group name for which the given result is
  returned:
\preformatted{
 Found 1 error 
                                         values      type      group
1                                          101   integer  virginica
2 supply both 'x' and 'y' or a matrix-like 'x' try-error     setosa
3                                           51   integer versicolor
} 

  Then, for the first group causing problems, a dumped environment is
  sent from a remote SPU to the R client process and opened in the R
  debugger. Choose environment number 11 and investigate the value of
  the \code{args} variable, which contains the input data frame and the
  input \code{FUN} function object.
  NOTE: Only expert users should consider opening other environment than
  the environment no 11.
}
\value{
  If \code{output.name} is not provided, an object of class
  \code{data.frame} is returned. It has the same columns as the sequences
  returned by \code{FUN}. If the number of returned columns matches the
  number input columns, then both have the same column names, that is,
  the input column names are copied into the output \code{data.frame}.
  
  Note that \code{FUN} function is expected to return a single value, a vector or data frame. 
  Complex types like lists or S4 classes are not allowed.
  
  If \code{output.name} is provided, an object of class \code{nz.data.frame}
  is returned.
}
\seealso{\code{\link{nzt}},
  \code{\link{nz.data.frame}}, \code{\link{output.signature}},
  \code{\link{nzApply}}}
\examples{
#nzConnectDSN('NZSQL')

d <- as.nz.data.frame(iris, table="iris2",clear.existing=TRUE)
r <- nzTApply(d[,1], 'Species', mean)

# the following lines do the same
nzTApply(d, d[,5], mean)
nzTApply(d, 'Species', mean)
nzTApply(d, 5, mean)

# this function is also registered with setMethod
tapply(d, d[,5], mean)
tapply(d, 'Species', mean)
tapply(d, 5, mean)

# Output signature - last column is the group's INDEX value.
# Notice, that the output signature specifies five output column,
# while the input nz.data.frame contains only four!
nzTApply(d[,1:4], 'Species', function(x)mean(x),
  output.signature=list(x=NZ.DOUBLE,y=NZ.DOUBLE,z=NZ.DOUBLE,
                        v=NZ.DOUBLE,g=list(NZ.VARIABLE,10)))

\dontrun{
# Given that an 'adult' table exists on Netezza.
nzadult <- nz.data.frame("adult")

fun <- function(x) {
 for (i in 1:10^2)
    a <- rnorm(10^5)
 mean(x)
}

df <- nzTApply(nzadult[,c(2,4)], nzadult[,11], fun, output.name='tapply_output_tmp',
      output.signature=list(AGE=NZ.DOUBLE, FNLWGT=NZ.DOUBLE, SEX=list(NZ.VARIABLE,10)))
df
# prints: SELECT  AGE,FNLWGT,SEX  FROM  tapply_output_tmp

df2 <- nzTApply(nzadult[,c(2,4)], nzadult[,11], fun)
df2
#        AGE   FNLWGT    SEX
# 1 36.85823 185746.3 Female
# 2 39.43355 191771.4   Male

# Debug mode.
nziris <- nz.data.frame("iris")
FUN5 <- function(x) if(min(x[,1]) < 4.5) cov(0) else min(x[,1])
nzTApply(nziris, 5, FUN5, debugger.mode=TRUE)
}

nzDisconnect()
}
\keyword{manip}
