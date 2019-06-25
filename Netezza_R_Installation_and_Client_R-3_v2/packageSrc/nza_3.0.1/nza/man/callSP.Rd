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
\name{callSP} 
\Rdversion{1.0}
\alias{callSP}
 
\title{Call in-database stored procedures}
\description{The function \code{callSP()} can be used to run any in-database stored procedure from the R client.}
\usage{callSP(spname, ...)}
\arguments{
	\item{spname}{A string with the name of the in-database procedure. See details.}
    \item{...}{
		A list of named arguments if applicable. Names of these arguments must cover the
		list of obligatory arguments for the procedure specified in \code{spname}. See details.
	}
}
\details{
	This function constructs a SQL query from \code{spname} and all given arguments. 
	Then the function \code{nzr::nzQuery} is used to execute this query.

	For further information about stored procedures and their parameters, check
	the 'IBM Netezza In-Database Analytics Reference Guide' or
	the 'IBM Netezza In-Database Analytics Developers Guide'.

	The name of the procedure may be predeceased with the database name.
	By default procedures are called from the \code{nza..} database.
}
\value{Output is dependent from the called procedure, usually a data-frame or a S3 object.}
\seealso{\link{nzCallXMProcedure}}

\examples{\dontrun{
	library(nza)
	
	#
	# connect with NPS and run a wrapper
	#
	nzConnect('user', 'password', 'machine', 'database')
	nzShowTables()
	
	#
	# call the KMeans function
	callSP("KMEANS", model="iris_mdl", intable="IRIS", outtable="iris_out234", id="ID",
		target="CLASS", distance="euclidean", k=3, maxiter=5, randseed=1234)
	# corresponding SQL query
	# CALL nza..KMEANS('model = iris_mdl, intable=IRIS, outtable=IRIS_out1234, id=id, target=CLASS, distance=euclidean, k=3, maxiter=5, randseed=1234');
	
	#
	# call regression tree function 
	callSP("REGTREE ", intable="nza..weatherr", id="instance", target="grade", model="wrt", maxdepth=4)
	# corresponding SQL query
	# CALL nza..REGTREE('intable=nza..weatherr, id=instance, target=grade, model=wrt, maxdepth=4');
	
	#
	# call decision tree function 
	callSP("DECTREE ", model="adult_tree", intable="nza..adult_train", id="id", target="income", minsplit=1000, eval = "entropy", valtable="nza..adult_prune", qmeasure="wAcc")
	# corresponding SQL query
	# CALL nza..DECTREE('model=adult_tree, intable=nza..adult_train, id=id, target=income, minsplit=1000, eval=entropy, valtable=nza..adult_prune, qmeasure=wAcc');
	
	# call histogram function 
	callSP("HIST", intable="nza..gdm_sales", incolumn="idart", outtable="hist1", right="F")
	# corresponding SQL query
	# CALL nza..HIST('intable=gdm_sales,incolumn=idart,outtable=hist1,right=F');
}} 
 
