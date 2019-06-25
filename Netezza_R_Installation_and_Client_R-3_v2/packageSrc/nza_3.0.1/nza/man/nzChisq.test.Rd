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
\name{nzChisq.test} 
\Rdversion{1.0}
\alias{nzChisq.test}
 
\title{Wrapper for Chi-Squared test}
\description{
	This function computes a Chi-Squared test.
	
	A local version of Chi-Squared testing may be found in \code{stats::chisq.test}.
}
\usage{
	nzChisq.test(form, nzdf, ...)
}
\arguments{
    \item{form}{A \code{formula} with two variables on the right hand side.}
    \item{nzdf}{A \code{nz.data.frame} object storing the data to be mined.}
    \item{...}{Further arguments to be passed to \code{chisq.test} from 'stats' package. See details.}
}
\value{Object of class \code{htest}}
\details{
	This function calculates a contingency table using \code{nzContingencyTable} and then calls \code{stats::chisq.test}.
	The \code{...} parameter will by passed to {stats::chisq.test}.
	
	To avoid double computation of the contingency table, it might be useful to compute the contingency table
	once and then use \code{stats::chisq.test} in order to compute the Chi-Squared test later. See examples below.
}
\seealso{\code{\link{nzTable}}}

\examples{\dontrun{
	require(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	form <- ~EDUCATION+OCCUPATION 
	nzdf <- nz.data.frame("adult")
	
	# parameter B=2000 will be passed to stats::chisq.test
	nzChisq.test(form, nzdf, B=2000)
	
	# calling nzTable() first in order to avoid double computations of the contingency table
	ctab <- nzContingencyTable(form, nzdf, T)
	chisq.test(ctab$mat)
	
	# nzDisconnect() 
}}

