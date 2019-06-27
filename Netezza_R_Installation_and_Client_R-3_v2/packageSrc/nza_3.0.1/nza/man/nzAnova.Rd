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
\name{nzAnova} 
\Rdversion{1.0}
\alias{nzAnova}
 
\title{Wrapper for Analysis of Variance (ANOVA)}
\description{
	This function is a wrapper for the \code{nza..ANOVA_RBD_TEST} and \code{nza..ANOVA_CRD_TEST}
	stored procedures.
	All computations are performed on the NPS, the results are converted to a R object.
	
	A local version of ANOVA is available in the \code{stats} package.
}
\usage{nzAnova(form, data, ...)}
\arguments{
	\item{form}{A \code{formula} object that describes the model to fit. See details.}
	\item{data}{A \code{nz.data.frame} object that points to the table where the variables specified in \code{form} are stored.}
	\item{...}{Further arguments to be passed to \code{nza..ANOVA_CRD_TEST} or \code{nza..ANOVA_RBD_TEST}, respectively.}
}
\value{Object of class \code{summary.aov}}
\details{
	R will call the stored procedure \code{nza..ANOVA_CRD_TEST} if there is only one right hand side 
	variable in \code{form}. The left hand side variable will be used as \code{incolumn} parameter, the right
	hand side variable will be used as \code{treatment} parameter when calling \code{nza..ANOVA_CRD_TEST}.
	
	R will call the stored procedure \code{nza..ANOVA_RBD_TEST} if there are two right hand side 
	variables in \code{form}. The left hand side variable will be used as \code{incolumn} parameter, the first
	right hand side variable will be used as \code{treatment} and the second right hand side variable
	will be used as \code{block} parameter when calling \code{nza..ANOVA_CRD_TEST}.

	For more details on the ANOVA algorithm and requirements to the \code{data}, please refer to
	the documentation of \code{nza..ANOVA_CRD_TEST} and \code{nza..ANOVA_RBD_TEST} in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
}

\examples{\dontrun{
	library(nza) 
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	
	nziris <- nz.data.frame("iris") 
	res <- nzAnova(SEPALLENGTH~CLASS, nziris) 
	print(res)
	
	# nzDisconnect()
}}
 
