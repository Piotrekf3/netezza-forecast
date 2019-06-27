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
\name{nzStats}
\alias{nzStats}
\title{Return runtime and configuration information}
\description{
This function returns runtime and configuration information gathered
from the Netezza \emph{nzstats} command-line utility with the
\code{subcmd} parameter equal \code{show}. It takes the same \code{type}
argument as the \code{nzstate} utility and returns its output for
appropriate values.
}
\usage{
nzStats(type = "system", user = "admin", password = "password", print = TRUE)
}
\arguments{
  \item{type}{The type of information to be returned.}
  \item{user}{The user ID of the user requesting the query. The default value
  of \code{"admin"} should be used unless your system is specially configured.}
  \item{password}{The password associated with \code{user}. The default value
  of \code{"password"} should be used unless your system is specially
  configured.}
  \item{print}{Denotes whether to print the statistics or return then as
  a \code{character vector}.}
}
\details{
The following values are accepted for the \code{type} parameter:
\code{"dbms"}, \code{"system"}, \code{"database"},
\code{"host"}, \code{"hostCpu"}, \code{"hostFileSystem"}, \code{"hostIf"},
\code{"hostMgmtChan"}, \code{"hostNet"}, \code{"hwMgmtChan"},
\code{"query"}, \code{"queryHist"}, \code{"spu"}, \code{"spuPartition"},
\code{"table"}, \code{"tableDataSlice"}, \code{"reclaim"}.
}
\value{
A \code{character vector} is returned if \code{print = FALSE}.
}
\seealso{\code{\link{nzConnect}}, \code{\link{nzConnectDSN}}}
\examples{
nzConnectDSN('NZSQL')
nzStats()
nzDisconnect()
}
