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
\name{nzLm} 
\Rdversion{1.0}
\alias{nzLm}
\alias{nzRidge}
\alias{print.nzLm}
 
\title{Wrapper for linear regression (LM)}
\description{
	These functions provide wrappers for linear regression and ridge regression.
	
	Local equivalents to these functions may be found in \code{stats::lm} and \code{MASS::lm.ridge}, respectively.
}
\usage{
	nzLm(form, nzdf, weights=-1)
	nzRidge(form, nzdf, lambda=10)
	
	\method{print}{nzLm}(object, ...)
}
\arguments{
    \item{form}{A \code{formula} object.}
    \item{nzdf}{A \code{nz.data.frame} object that stores the data to be mined.}
    \item{weights}{Weights for nzDotProduct.}
    \item{lambda}{The lambda coefficient for the ridge regression.}
    \item{object}{Object of class \code{nzLm}.}
    \item{...}{Currently unused.}
   }
\value{
	The procedures returns either the linear regression model or the ridge regression model
	as object of class \code{nzLm}.
}
	
\examples{\dontrun{
	library(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL') 
	 
	nzdf <- nz.data.frame("adult") 
	nzLm(AGE~EDUCATION_NUM+HOURS_PER_WEEK, nzdf) 
	nzRidge(AGE~EDUCATION_NUM+HOURS_PER_WEEK, nzdf) 
	
	# nzDisconnect()  
}}
 
