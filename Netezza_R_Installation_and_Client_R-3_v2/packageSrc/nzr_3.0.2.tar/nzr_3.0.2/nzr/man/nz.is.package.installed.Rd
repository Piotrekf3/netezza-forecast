% 
% Copyright (c) 2010, 2011, IBM Corp. All rights reserved. 
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
\name{nzIsPackageInstalled}
\alias{nzIsPackageInstalled}
\title{Check package status}
\description{
  Checks whether a package is installed on the Netezza Host and SPUs.
}
\usage{
nzIsPackageInstalled(package)
}
\arguments{
	\item{package}{Name of the package to be checked.}
}
\details{
  If the package is found on the Host or SPUs a message is printed
  on the screen.
}
\value{
  \code{TRUE} if the package has been found in given locations,
  \code{FALSE} otherwise.
}
\seealso{\code{\link{nzInstallPackages}}}
\examples{
nzConnectDSN('NZSQL')
nzIsPackageInstalled(tree)
nzDisconnect()
}
