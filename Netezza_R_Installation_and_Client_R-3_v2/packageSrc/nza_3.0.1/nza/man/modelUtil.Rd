%
% Copyright (c) 2011, 2012, Revolution Analytics. All rights reserved.
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
\name{modelUtil}
\Rdversion{1.0}
\alias{modelList}
\alias{modelDrop}
\alias{modelExist}
\alias{model2pmml}

\title{Model tables management}
\description{
	\strong{modelList} lists the current models in database
	
	\strong{modelDrop} drops the specified model from database
	
	\strong{modelExist} checks if the specified model exists in database
	
	\strong{model2pmml} is a wrapper for \code{nza..PMML_MODEL}
}
\usage{
	modelList(...)
	modelDrop(x, ...)
	modelExist(x, ...)
	model2pmml(x, ...)
}
\arguments{
	\item{x}{Model to be dropped, existence-checked or converted to PMML}
	\item{...}{Further parameters to be passed to \code{modelExist} or \code{model2pmml}. Ignored otherwise.}
}
\details{
	The parameter \code{x} may be the model name as a string or the R object, that was returned
	when creating the model.
}

\examples{\dontrun{
	library(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally 
	# on your system using "ODBC_Source_Administrator" 
	# nzConnectDSN('NZSQL')
	
	data <- nz.data.frame("WEATHER")
	form <- PLAY~HUMIDITY
	
	newtree <- nzDecTree(form, data=data, id="INSTANCE", minsplit=3)
	
	modelExist(newtree)
	modelExist("FOO", verbose=T, inverse=T)
	
	model2pmml(newtree, outtable="ARule_pmml")
	as.data.frame(nz.data.frame("ARule_pmml"))
	
	modelList()
	modelDrop(newtree)
	
	# nzDisconnect() 
}}
