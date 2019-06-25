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
\name{nzDotProduct} 
\Rdversion{1.0}
\alias{nzDotProduct}
\alias{nzPCA}
 
\title{Compute a dot product table or perform a principle components analysis}
\description{
	The function \code{nzDotProduct} computes a dot product table of a given \code{nz.data.frame}.
	
	The function \code{nzPCA} performs a principal components analysis.
	
	Local versions of these functions my be found in \code{base::matmult} and \code{stats::princomp}, respectively.
}
\usage{
	nzDotProduct(form, nzdf, makeMatrix=F, weights = -1)
	nzPCA(nzdf, scale=FALSE)
}
\arguments{
    \item{form}{A \code{formula} with variables that shall be used on the right hand side.
    			To use all variables, write \code{~.}
    			For backwards compatibility, there may be also a variable on the
    			left hand side that will be handled as if it were on the right hand side.
    			}
    \item{nzdf}{A \code{nz.data.frame} object that stores the data to be mined.}
    \item{makeMatrix}{Denotes whether the contingency table is downloaded to the client.
    					If \code{FALSE}, results will only be stored in a table on the NPS.}
	\item{weights}{Weights, default to be -1.}
	\item{scale}{Will be passed to the stored procedure.}
}
\value{
	The function \code{nzDotProduct} will return a \code{list} with components as follows:
		\itemize{
			\item \code{xml}: string that was used to call \code{nzCallXMProcedure("DotProduct", xml)}
			\item \code{tab}: the dot product table as \code{nz.data.frame} in a sparse format
			\item \code{mat}: if \code{makeMatrix} was \code{TRUE} the dot product table as named matrix object
			\item \code{varnames}: used variable names
			\item \code{size1}, \code{size2}: information on variables and interaction terms
		}
		
	The function \code{nzPCA} will return an object of class \code{princomp}.
}

\examples{\dontrun{
	library(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	 
	nziris <- nz.data.frame("iris") 
	ctab1 <- nzDotProduct(~SEPALLENGTH+(PETALLENGTH+PETALWIDTH)^2+factor(CLASS), nziris, T)
	str(ctab1)
	print(ctab1$mat)
	
	# will create the same result but uses backwards compatible formula
	ctab2 <- nzDotProduct(SEPALLENGTH~(PETALLENGTH+PETALWIDTH)^2+factor(CLASS), nziris, T) 
	
	num.iris <- nziris[ ,2:5]
	res <- nzPCA(num.iris, scale=FALSE) 
	print(res)
	plot(res)
	
	# nzDisconnect() 
}}
 
