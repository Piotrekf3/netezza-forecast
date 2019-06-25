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
\name{nzParseRFormula} 
\Rdversion{1.0}
\alias{nzParseRFormula}
 
\title{Parse R formulas for use with Netezza Analytics Library}
\description{
	This function parses an R formula for use with Netezza Analytics Library,
	i.e. for use with an nz.data.frame.
}
\usage{nzParseRFormula(form, nzdf)}
\arguments{
    \item{form}{The R formula to parse.}
    \item{nzdf}{An nz.data.frame object.}
    }
\value{The procedure returns the parsed formula as \code{list} object.}

\examples{\dontrun{
	library(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	 
	form <- ~EDUCATION+OCCUPATION 
	nzdf <- nz.data.frame("adult")
	cf <- nzParseRFormula(form, nzdf) 
	str(cf)
	 
	# nzDisconnect() 
}}
 
