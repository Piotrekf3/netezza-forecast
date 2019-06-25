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
\name{nzShowTables}
\alias{nzShowTables}
\title{Return a list of tables}
\description{
  Returns a data frame containing names of tables available in the
  currently used database.
}
\usage{
nzShowTables()
}
\details{
  Running this function has the same effect as running the command
  \code{\\d} in the \emph{nzsql} tool.
}
\value{
  Data frame with columns: Name, Type, Owner.
}
\examples{
nzConnectDSN('NZSQL')
nzShowTables()
nzDisconnect()

# Sample output:
#
#   Name  Type Owner
# 1 IRIS TABLE ADMIN
# 2   T1 TABLE ADMIN
}
