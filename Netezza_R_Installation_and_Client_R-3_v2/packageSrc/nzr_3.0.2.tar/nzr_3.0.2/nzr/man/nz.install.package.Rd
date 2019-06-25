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
\name{nzInstallPackages}
\alias{nzInstallPackages}
\title{Perform package installation}
\description{
  Sends a given CRAN package to Netezza and performs the installation
  of that package.
}
\usage{
nzInstallPackages(pkg, installOnSpus = TRUE)
}
\arguments{
  \item{pkg}{The location of the package, which can either be a local
    file path or a web address. If a web address, the string must begin
	with \code{http://}}
  \item{installOnSpus}{Determines whether the package is to be installed
    on the SPUs. If \code{FALSE}, no installation takes place.}
}
\details{
  If the \code{pkg} parameter value starts with \code{http://} it is
  assumed to be a web address.

  After sending the file it is installed on Netezza, which involves
  compiling the C/Fortran code. When installation is completed, the
  installation log is displayed on the screen.
}
\value{No value is returned.}
\seealso{\code{\link{nzConnectDSN}},\code{\link{nzApply}}}
\examples{
\dontrun{
nzConnectDSN('NZSQL')
nzInstallPackages('http://cran.r-project.org/src/contrib/tree_1.0-29.tar.gz')
nzDisconnect()
}
}
