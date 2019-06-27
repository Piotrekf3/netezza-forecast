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
\name{nzQuery}
\alias{nzQuery}
\alias{nzScalarQuery}
\title{Run an SQL query on Netezza}
\description{
These functions allow you to run any SQL query on Netezza and fetch the
results into a client-side \code{data.frame}.
}
\usage{
nzQuery(..., as.is=TRUE)

nzScalarQuery(..., as.is=TRUE)
}
\arguments{
  \item{\dots}{Any number of query parts which are passed to \code{paste()}.}
  \item{as.is}{Denotes whether R should leave the result columns
    \emph{as-is}, that is, without type conversions, or run the default
	RODBC type conversions (see \code{sqlGetResults} and \code{type.convert}).}
}
\details{
All parts of the input query are concatenated with \code{paste(..., sep="")}
and the result is passed to Netezza.
}
\value{
  The \code{nzQuery} function returns a \code{data.frame} with the
  query results.

  The \code{nzScalarQuery} function returns the query result coerced
  to a single scalar value.
}
\seealso{\code{\link{nzConnect}},\code{\link{nzScalarQuery}},\code{\link{nzt}}}
\examples{
nzConnectDSN('NZSQL')
t <- nzQuery("SELECT COUNT(*) FROM _V_DUAL_DSLICE")
if (t != nzNoDSlices())
	warning("these should be equal")
nzDisconnect()
}
