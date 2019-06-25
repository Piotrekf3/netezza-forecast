# 
# Copyright (c) 2010, 2014, IBM Corp. All rights reserved. 
# 		
# This program is free software: you can redistribute it and/or modify 
# it under the terms of the GNU General Public License as published by 
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version. 
#
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU General Public License for more details. 
#
# You should have received a copy of the GNU General Public License 
# along with this program. If not, see <http://www.gnu.org/licenses/>. 
# 
# 

nzFilter <- function (x, fun, output.name=NULL, clear.existing=FALSE,  ...)
{
	nzCheckConnection()
	
	filter_wrapper <- function (x, filter_function, ...) {
		return(as.logical(filter_function(x, ...)))
	}
	environment(filter_wrapper) <- emptyenv()
	environment(fun) <- emptyenv()

	# if returns without exception we may proceed further
	outputExists <- nz.run.rae.check.output(output.name, clear.existing)

	toserialize <- list(
		mode  = 'apply',
		fun   = filter_wrapper,
		args  = list(filter_function = fun),
		chunk = 1 # the data chunk size; has to be equal to ONE in the function mode
	)

	datafile <- nz.placefile(data=rawToChar(serialize(toserialize, NULL, ascii=TRUE)))
  tab1 <- nzdf.qualified.name(x)
  col1 <- paste('\"',x@cols,'\"',sep="")
  whereClause <- ifelse(nchar(x@where)>0, paste(" AND ", x@where, sep=""),"")
  
	if (is.null(output.name))
		return(nzQuery(
			" SELECT ", paste(tab1, col1, sep='.', collapse=','), " FROM ", tab1,
			" WHERE nzr..r_filter(", nz.run.rae.prepare.columns(x),
				", CAST('WORKSPACE_PATH=", datafile, "' AS VARCHAR(256))) = TRUE", whereClause
		))
	else {
		nzQuery(
			ifelse(outputExists, paste("INSERT INTO", output.name),
				paste("CREATE TABLE", output.name, "AS")),
			" SELECT ", paste(tab1, col1, sep='.', collapse=','), " FROM ", tab1,
			" WHERE nzr..r_filter(", nz.run.rae.prepare.columns(x),
				", CAST('WORKSPACE_PATH=", datafile, "' AS VARCHAR(256))) = TRUE", whereClause
		)
		return(nz.data.frame(output.name))
	}
}

