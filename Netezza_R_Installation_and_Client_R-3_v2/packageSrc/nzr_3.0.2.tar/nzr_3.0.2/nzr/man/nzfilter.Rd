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
\name{nzFilter}
\alias{nzFilter}
\title{Filter table contents}
\description{
  This function filters the contents of the table referenced by the
  \code{nz.data.frame} object based on a user-provided filter function.
  Only those rows for which \code{fun} returns \code{TRUE} are put into
  the output table/\code{data.frame}.
}
\usage{
nzFilter(x, fun, output.name = NULL, clear.existing = FALSE, case.sensitive = FALSE, ...)
}
\arguments{
  \item{x}{Input data source, an \code{nz.data.frame} object.}
  \item{fun}{User-provided filtering function.}
  \item{output.name}{Netezza output table name.}
  \item{clear.existing}{Denotes whether the output table, if it
    already exists, is dropped.}
  \item{\dots}{Additional parameters passed to \code{fun}.}
}
\details{
  The user-provided function \code{fun} takes one argument, which is the
  current row passed as a \code{list}. It returns only one value that
  is coerced to a logical vector. If \code{TRUE} is returned, a row is
  copied into the output table; if \code{FALSE}, it is skipped.
}
\value{
  Object of the \code{nz.data.frame} class.
}
\seealso{\code{\link{nz.data.frame}},\code{\link{nzConnectDSN}},
		 \code{\link{nzApply}}}
\examples{
nzConnectDSN('NZSQL')
d <- as.nz.data.frame(iris, '"iris2"',clear.existing=TRUE)
f <- nzFilter(d, function(x) { return (runif(1) > 0.5) } )
nzDisconnect()
}
\keyword{manip}
