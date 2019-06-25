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
\name{nzDebug}
\alias{nzDebug}
\title{Toggle debugging}
\description{
	This function switches debugging on or off.
}
\usage{
nzDebug(onoff = TRUE)
}
\arguments{
	\item{onoff}{Turn debugging on (\code{TRUE}) or off (\code{FALSE}).}
}
\details{
  Some functions, such as \code{nzQuery()}, available in this package,
  can print additional debug information when called, after this function
  is invoked with \code{onoff == TRUE}. This flag sets a global variable
  \code{.nzDebug}, whose value is equal to the \code{onoff} parameter’s value.

  If debug is enabled prior to establishing a connection, the DSN is
  extended with \code{DebugLogging=1} and \code{DebugOnTheFly=1}, which
  enables ODBC debug logging. By default, debug logging is placed in the
  \code{/tmp} directory.
}
\examples{
\dontrun{
# Given that some_table exists on NPS, and that it has at least one
# (first) column of type `double'.

nzConnectDSN('NZSQL')
nzDebug(TRUE)		# Turns on debug messages.
# .... do some operations using nzr ....
nzDebug(FALSE)		# Turns off debug messages.

nzDisconnect()
}
}
