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
\name{nzNoDSlices}
\alias{nzNoDSlices}
\alias{nzDualTable}
\alias{nzDualDSliceTable}
\alias{nzDeleteTable}
\alias{nzTruncateTable}
\alias{nzExistTable}
\alias{nzTableDef}
\alias{nzGetValidTableName}
\alias{nzDBName}
\title{Other tools}
\description{
  This is a set of functions designed to simplify working with
  \emph{nzr} and Netezza.
}
\usage{
nzNoDSlices()

nzDualDSliceTable()

nzDualTable()

nzExistTable(table)

nzDeleteTable(table)

nzTruncateTable(table)

nzTableDef(nzdf, case.sensitive=NULL, collapse=TRUE)

nzGetValidTableName(prefix = "data_frame_")

nzDBName()

nzIsView(x, case.sensitive=NULL)
}
\arguments{
  \item{table}{A valid name of a Netezza table; in the case of the
    \code{nzDeleteTable} function and the \code{nzTruncateTable}
	function, the table must exist.}
  \item{x}{An object of class \code{nz.data.frame} or \code{character}.}
  \item{nzdf}{An object of \code{nz.data.frame} class.}
  \item{prefix}{A prefix of a table name.}
  \item{collapse}{Return a string with the table definition (if \code{TRUE})
    or a \code{data.frame} (if \code{FALSE}).}
}
\value{
  The \code{nzNoDSlices} function returns the number of data slices.
  
  The \code{nzDualDSliceTable} function returns a \code{nz.data.frame}
  pointing to the \code{_V_DUAL_DSLICE} table, which has exactly one row
  on each data slice.

  The \code{nzDualTable} function returns a \code{nz.data.frame} pointing
  to the \code{_V_DUAL} table, which has exactly one row on the host.

  The \code{nzExistTable} function returns a logical value indicating
  whether the given table exists on Netezza.

  The \code{nzDeleteTable} and \code{nzTruncateTable} functions can be
  used to drop the specified table or to remove all of its rows. Row
  removal is the preferred method when the table is to be filled with
  new contents.
  
  The \code{nzTableDef} function returns a string with a SQL table
  definition.
  
  The \code{nzGetValidTableName} function returns a string representing
  unique table name.

  The \code{nzDBName} function returns the current database name.

  The \code{nzIsView} function returns \code{TRUE} if the object specified
  by \code{x} is a view, or \code{FALSE} if it is a table.
}
\examples{
nzConnectDSN('NZSQL')

d <- nzDualDSliceTable()
if (nrow(d) != nzNoDSlices())
	warning("these values should be equal")

nzDisconnect()
}
