% 
% Copyright (c) 2010, 2011, IBM Corp. All rights reserved. 
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
\name{nzCallXMProcedure} 
\Rdversion{1.0}
\alias{nzCallXMProcedure}

\title{Call XML-based in-database stored procedure}
\description{The function \code{nzCallXMProcedure} can be used to run any XML-based in-database stored procedure from the R client.}
\usage{nzCallXMProcedure(pname, xml, debug=1)}
\arguments{
    \item{pname}{The procedure's name.}
    \item{xml}{The parameters in XML format.}
    \item{debug}{The debug level.}
    }
\value{This function returns the results from the called stored procedure, in most cases in XML format.}

\examples{\dontrun{
	library(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	nzdf <- nz.data.frame("adult")
	form <- ~WORKCLASS+EDUCATION+OCCUPATION
	ntab1 <- nzParseRFormula(form, nzdf)
	
	outtable <- nzGetValidTableName()
	xml <- nza:::nzPrepareXML4ContTable(varlist=ntab1$varlist, intable=nzCreateView(nzdf), outtable=outtable)
	res  = nzCallXMProcedure("ctable", xml)
	otab = nz.data.frame(outtable)
	
	# nzDisconnect() 
}}
