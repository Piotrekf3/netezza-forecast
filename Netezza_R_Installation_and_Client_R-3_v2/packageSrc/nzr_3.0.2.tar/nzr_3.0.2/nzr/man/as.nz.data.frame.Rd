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
\name{as.nz.data.frame}
\alias{as.nz.data.frame}
\alias{as.nz.data.frame.data.frame}
\title{Coerce to a nz.data.frame}
\description{
  This function creates a \code{nz.data.frame} from a different R object.
}
\usage{
as.nz.data.frame(x, table = NULL, distributeon = NULL,
    clear.existing = FALSE, fast = TRUE,  case.sensitive=FALSE)
}
\arguments{
  \item{x}{The object to be coerced to \code{nz.data.frame}.}
  \item{table}{Optional table name; if not specified, the function
    selects a name, defaulting to the \code{x} object name, if possible.}
  \item{distributeon}{Optional column name; the Netezza-side data
    distribution is based on the specified column.}
  \item{clear.existing}{Denotes whether the output table, if it currently
    exists, should be deleted. If set to \code{TRUE}, and the Netezza table
	specified by the table argument exists, that table is dropped.}
  \item{fast}{Denotes whether fast upload mode should be used. A value of
    \code{TRUE} enables fast upload mode.}
   \item{case.sensitive}{Denotes whether the supplied table name should be treated
    as case-sensitive.}
}
\details{
  This function creates a Netezza table and inserts the data passed
  as \code{x} into the newly created table.

  When \emph{fast mode} is enabled, data is uploaded via raw network
  connection, which typically speeds up the process since it bypasses
  the RODBC/ODBC stack. 
}
\value{
  Object of the \code{nz.data.frame} class pointing to the newly created
  table.
}
\seealso{ \code{\link{as.data.frame}} }
\examples{
nzConnectDSN('NZSQL')
d <- as.nz.data.frame(iris, 'iris2',clear.existing=TRUE, fast=TRUE) # use fast mode
#It will error out (iris2 already exists)
#d2 <- as.nz.data.frame(iris,'iris2')
nzDisconnect()
}
