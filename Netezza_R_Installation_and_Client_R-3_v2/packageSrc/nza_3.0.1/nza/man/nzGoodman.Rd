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
\name{nzGoodman} 
\Rdversion{1.0}
\alias{nzGoodman}
 
\title{Goodman and Kruskal Gamma statistic}
\description{
	This function implements the Goodman and Kruskal Gamma statistic.
}
\usage{
	nzGoodman(form, nzdf, ...)
}
\arguments{
    \item{form}{A \code{formula} with two variables on the right hand side.}
    \item{nzdf}{A \code{nz.data.frame} object storing the data to be mined.}
    \item{...}{Currently unused.}
}
\value{This procedure returns the Goodman gamma statistic as \code{numeric} value.}
\details{
	The Goodman-Kruskal gamma statistic is an easily-interpreted measure of rank correlation between two vectors, v1 and v2.
	It is the number of agreements minus the number of disagreements, all divided by the total of agreements and disagreements.
	A Goodman-Kruskal gamma statistic of 1 means that the correlation is as high as it can be, given the ties.
	
	This function calculates a contingency table using \code{nzContingencyTable}.
	To avoid double computation of the contingency table, it might be useful to compute the contingency table
	once and then use another function in order to compute the test later.
}
\seealso{\code{\link{nzTable}}}

\examples{\dontrun{
	require(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	nzdf <- nz.data.frame("adult")
	g <- nzGoodman(~AGE+FNLWGT, nzdf)
	print(g) 
	
	# nzDisconnect() 
}}
