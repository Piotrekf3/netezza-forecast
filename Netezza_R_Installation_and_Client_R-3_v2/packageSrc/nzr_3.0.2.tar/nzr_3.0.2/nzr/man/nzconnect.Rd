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
\name{nzConnect}
\alias{nzConnect}
\alias{nzConnectDSN}
\alias{nzDisconnect}
\alias{nzCheckConnection}
\alias{nzIsConnected}
\alias{nzinit}
\title{Connection management}
\description{
  This functions connects you to Netezza using directly entered
  credentials or a pre-defined DSN (Database Source Name) string.
}
\usage{
nzConnect(user, password, machine, database, force=FALSE,
  queryTimeout=0, loginTimeout=0, verbose=TRUE)

nzConnectDSN(dsn, force=FALSE, verbose=TRUE)
nzDisconnect()
nzCheckConnection()
nzIsConnected()
}
\arguments{
  \item{user}{A string value specifying the database user name.}
  \item{password}{A string value specifying the password for the database user.}
  \item{machine}{A string value specifying the name or IP address of the Netezza appliance.}
  \item{database}{A string value specifying the database name.}
  \item{queryTimeout}{The query timeout; the default value is 0, meaning no timeout.}
  \item{loginTimeout}{The login timeout; the default value is 0, meaning no timeout.}
  \item{dsn}{The DSN string as defined in the local ODBC configuration.}
  \item{force}{Open a new connection, even if a previous connection is still open.}
  \item{verbose}{Show verbose connection info.}
}
\details{
  Either the \code{nzConnectDSN()} or \code{nzConnect()} function must
  be called before any other \emph{nzr} functions can be called. When
  called, the \code{nzConnect}* functions set a hidden variable that is
  checked by other \emph{nzr} functions before they execute. The
  \code{nzDisconnect()} function disconnects the client from the server.
  The \code{nzCheckConnection()} and \code{nzIsConnected()} functions can
  be used to check if connection currently exists. 

  During the connection setup a number of operations are performed, including
  assigning the Netezza data types identifiers; see \code{\link{output.signature}}
  for details.

  The \code{nzinit()} function is a convenient tool for opening a
  connection. It checks for the existence of the \code{$HOME/.nzrrc}
  file, and if it exists reads the contents of the file by running
  \code{source()}. If this file defines, at a minimum, the \code{user},
  \code{pwd}, \code{host}, and \code{db} objects, their values are
  passed to \code{nzConnect}. Creation of such a file simplifies the
  connection to the default host.
}
\value{
  No value is returned.
}
\seealso{\code{\link{nzNoDSlices}}, \code{\link{nzDualDSliceTable}},
         \code{\link{nzStats}}, \code{\link{output.signature}}}
\examples{
\dontrun{
# Will connect to a Netezza database
nzConnect('miami', 'AZ4GH', '172.0.0.123', 'miamidb')
nzConnectDSN('MIAMI')
}
}
