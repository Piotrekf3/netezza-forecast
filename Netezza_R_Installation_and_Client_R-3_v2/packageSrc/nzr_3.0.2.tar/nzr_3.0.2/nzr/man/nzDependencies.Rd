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
\name{nzDependencies}
\alias{nzDependencies}
\title{Check dependencies}
\description{
  This function checks versions of selected installed Netezza cartridges
  (\code{*.nzc} files), as well as the installed version of R, on the
  Host and SPUs.
}
\usage{
nzDependencies(types=c("r_ae","r"))
}
\arguments{
	\item{types}{A vector of names of cartridges, by default contains two values: "r_ae" and "r"}
}
\details{
	This function prints out information about versions of installed cartridges. 
	It is also called when connecting to a machine to check the versions of R
	and \emph{nzr}-related cartridges, installed on the Netezza appliance.
}
\examples{
nzConnectDSN('NZSQL')

nzDependencies()

nzDependencies(c("system_ae", "r", "analytics_data"))

nzDisconnect()
}
