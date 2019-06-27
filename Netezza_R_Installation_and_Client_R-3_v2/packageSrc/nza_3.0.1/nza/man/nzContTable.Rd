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
\name{nzContingencyTable} 
\Rdversion{1.0}
\alias{nzContingencyTable}
\alias{nzTable}
 
\title{Wrapper for contingency table function}
\description{
	This function computes a contingency table.
	
	A local version of this function may be found in \code{base::table}.
}
\usage{
	nzContingencyTable(form, nzdf, makeMatrix=T)
	
	nzTable <- nzContingencyTable
}
\arguments{
    \item{form}{A \code{formula} with two variables on the right hand side.}
    \item{nzdf}{A \code{nz.data.frame} object storing the data to be mined.}
    \item{makeMatrix}{Denotes whether the contingency table should be downloaded. See value.}
}
\value{A \code{list} with components as follows:
	\itemize{
		\item \code{xml}: string that was used to call \code{nzCallXMProcedure("ctable", xml)}
		\item \code{tab}: the contingency table as \code{nz.data.frame} in a sparse format
		\item \code{mat}: if \code{makeMatrix} was \code{TRUE} the contingency table as named \code{table} object
	}
}
\details{
	The function \code{nzTable} is an alias for \code{nzContingencyTable}.
}

\examples{\dontrun{
	require(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	form <- ~EDUCATION+OCCUPATION 
	nzdf <- nz.data.frame("adult")
	ctab <- nzContingencyTable(form, nzdf, T) 
	str(ctab)
	
	# nzDisconnect() 
}}
 
