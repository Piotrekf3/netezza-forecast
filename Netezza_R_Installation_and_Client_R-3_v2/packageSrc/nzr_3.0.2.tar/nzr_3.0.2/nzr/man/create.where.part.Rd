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
\name{nzCreateWherePart}
\alias{nzCreateWherePart}
\title{Produce a SQL WHERE clause}
\description{
This function produces the \code{WHERE} clause of a SQL query when
operating on a \code{nz.data.frame}s.
}
\usage{
nzCreateWherePart(obj, value, operator)
}
\arguments{
  \item{obj}{An object of class \code{nz.data.frame}.}
  \item{value}{The columns of the object passed in the
    \code{obj} argument are compared to this value. Note that you might need to quote the value.}
  \item{operator}{Comparison operator}
}
\details{
  This function is typically used in a manner internal to the code.
  It is exposed publically to allow it to be used in packages built
  on top of \emph{nzr}.
}
\value{
  Object of class \code{nz.data.frame.rows}.
}
\seealso{\code{\link{nz.data.frame}}}
