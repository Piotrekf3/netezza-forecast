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
\name{nzCreateView}
\alias{nzCreateView}
\alias{nzDropView}


\title{Create/drop a view}
\description{
  These functions allow you to create a view from a \code{nz.data.frame}
  and then to drop the view. 
}
\usage{
nzCreateView(x,simplified=F) 
nzDropView(v) 
}
\arguments{
  \item{x}{\code{nz.data.frame} object.}
  \item{v}{Name of the view.}
  \item{simplified}{Denotes whether NUMERIC and DECIMAL columns should be casted to DOUBLE, e.g. for use with nzTApply.}
}
\details{
  The \code{nzCreateView()} function creates a view from a
  \code{nz.data.frame}. The function returns the view name.

  The \code{nzDropView()} function drops the view specified
  by its name.
}
\examples{
nzConnectDSN('NZSQL')
d <- as.nz.data.frame(iris,'iris2',clear.existing=T,case.sensitive=T)
vname <- nzCreateView(d)
nzDropView(vname)
nzDisconnect()
}
