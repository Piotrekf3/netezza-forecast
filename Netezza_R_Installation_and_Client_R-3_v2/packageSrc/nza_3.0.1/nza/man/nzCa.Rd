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
\name{nzCa} 
\Rdversion{1.0}
\alias{nzCa}
 
\title{Wrapper for correspondence analysis (CA)}
\description{
	This procedure computes the contingency table \code{contTab}
	and calls \code{ca(contTab)} from package \code{ca}.
	
	A local way perform correspondence analysis may be found in \code{ca::ca}.
}
\usage{nzCa(form, nzdf)}
\arguments{
    \item{form}{A \code{formula} with two right hand side variables and no left hand side variables.}
    \item{nzdf}{A \code{nz.data.frame} that stores the data to be mined.}
    }
\value{Object of class \code{ca}.}
\seealso{\code{\link{nzTable}}}

\examples{\dontrun{
	library(nza)

	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	adult <- nz.data.frame("adult") 
	nzCa(~EDUCATION+OCCUPATION, adult)
	 
	#nzDisconnect() 
}}
 
