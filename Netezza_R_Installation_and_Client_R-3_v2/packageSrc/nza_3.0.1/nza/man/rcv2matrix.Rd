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
\name{nzSparse2matrix} 
\Rdversion{1.0}
\alias{nzSparse2matrix}
 
\title{Conversion from RCV to matrix}
\description{This function converts a \code{data.frame} or \code{nz.data.frame} with row-column-value format to the standard R matrix format.}
\usage{
	nzSparse2matrix(tab, with.names = TRUE)
}
\arguments{
    \item{tab}{The \code{data.frame} or \code{nz.data.frame} object.}
	\item{with.names}{Boolean flag indicating whether names should be accompanied or not.}
}
\value{The procedure returns the data from \code{tab} as a matrix.}
\details{
	This function and all computations will be performed locally. Nothing will be executed on the NPS side.
	If \code{tab} is a \code{nz.data.frame}, all data will be downloaded.
}


\examples{\dontrun{
	require(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	# create own sample data
	mat <- matrix(c(1,2,3,1,2,3,4,4,4),3,3)
	nzmat <- as.nz.data.frame(as.data.frame(mat))
	res <- nzSparse2matrix(nzmat, with.names=FALSE)
	# res equals diag(4,3) 
	
	# use CURVES data from NZA
	data <- nz.data.frame("curves")
	res <- nzSparse2matrix(data)
	
	# nzDisconnect() 
}}
