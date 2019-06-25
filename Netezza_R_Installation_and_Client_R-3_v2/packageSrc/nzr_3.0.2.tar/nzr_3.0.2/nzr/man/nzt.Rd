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
\name{nzt}
\alias{nzt}
\title{Transpose a nz.data.frame}
\description{
  This function tranposes a \code{data.frame} returned from Netezza that
  has \code{COLUMNID} and \code{VALUE} columns into a standard
  \code{data.frame}.
}
\usage{
nzt(x, col.names = NULL)
}
\arguments{
  \item{x}{A \code{data.frame} returned from Netezza}
  \item{col.names}{If not provided, names in the form of \code{Xn}
    are used.}
}
\details{
  This function can be used to transpose \code{data.frame}s that cannot
  or should not be transposed on the server side. 
}
\value{
  A transposed \code{data.frame}.
}
\examples{
nzConnectDSN('NZSQL')

d     <- nz.data.frame("iris")
npsdf <- nzApply(d, NULL, function(x) length(x) )
df    <- nzt(npsdf)

nzDisconnect()
}
