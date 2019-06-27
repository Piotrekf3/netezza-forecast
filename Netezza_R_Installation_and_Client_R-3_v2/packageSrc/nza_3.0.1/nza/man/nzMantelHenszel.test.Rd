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

\name{nzMantelHenszel.test} 
\Rdversion{1.0}
\alias{nzMantelHenszel.test}
\alias{nzMantelhaen.test}
 
\title{Cochran-Mantel-Haenszel Chi-Squared Test}
\description{
	This function performs a Cochran-Mantel-Haenszel Chi-Squared test.
	
	A local function that also computes the Cochran-Mantel-Haenszel Chi-Squared test is \code{stats::mantelhaen.test}.
}
\usage{
	nzMantelHenszel.test(form, nzdf, ...)
	
	nzMantelhaen.test <- nzMantelHenszel.test
}
\arguments{
    \item{form}{A \code{formula} with two variables on the right hand side.}
    \item{nzdf}{A \code{nz.data.frame} object storing the data to be mined.}
    \item{...}{Further arguments to be passed to \code{mantelhaen.test} from 'stats' package. See details.}
}
\value{Object of class "htest"}
\details{
	The function \code{nzMantelhaen.test} is an alias for \code{nzMantelHenszel.test}.
	
	This function calculates a contingency table using \code{nzContingencyTable} and then calls \code{stats::mantelhaen.test}.
	The \code{...} parameter will by passed to {stats::mantelhaen.test}.
	
	To avoid double computation of the contingency table, it might be useful to compute the contingency table
	once and then use \code{stats::mantelhaen.test} in order to compute the test later. See examples below.
}
\seealso{\code{\link{nzTable}}}

\examples{\dontrun{
	require(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	adult <- nz.data.frame("adult")
	form <- ~EDUCATION+OCCUPATION+INCOME
	m <- nzMantelHenszel.test(form, adult) 
	
	# parameter conf.level=0.99 will be passed to stats::mantelhaen.test
	nzMantelHenszel.test(~EDUCATION+OCCUPATION+INCOME, adult, conf.level=0.99)
	
	# calling nzTable() first in order to avoid double computations of the contingency table
	ctab <- nzContingencyTable(form, adult, T)
	mantelhaen.test(ctab$mat)
	
	# nzDisconnect() 
}}