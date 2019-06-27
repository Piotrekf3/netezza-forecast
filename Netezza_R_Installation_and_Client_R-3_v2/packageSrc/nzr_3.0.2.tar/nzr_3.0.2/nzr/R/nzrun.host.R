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

nzRunHost <-
function (fun, output.name=NULL, output.signature=NULL, clear.existing=FALSE,case.sensitive=FALSE, ...) {

	environment(fun) <- emptyenv()
	
	# move all arguemnts that are functions into global environment
	args <- list(...)
	args <- lapply(args,
			function(arg) {
				if (is.function(arg)) {
					environment(arg) <- globalenv()
					return(arg)
				} else {
					return(arg)
				}
			}
	)
	
	toserialize <- list(fun=fun, args=args, mode='run')

	if (!is.null(output.signature)) {
		
		#case sensitive column names
		if(case.sensitive) {
			output.signature2 <- output.signature
			names(output.signature2) <- paste("\"",names(output.signature),"\"", sep='')
			toserialize$shaper.list <- output.signature2
		}
		
		else 
			toserialize$shaper.list <- output.signature
		
		
		toserialize$shaper <- 'std'
		udtf <- 'r_udtf_any'
	} else {
		udtf <- 'r_udtf'
	}

	# check the output name
	if (!is.null(output.name) && nzExistTable(output.name)) {
		if (!is.null(clear.existing) && clear.existing)
			nzDeleteTable(output.name)
		else
			stop('the output table already exists, but clear.existing is set to FALSE')
	}
	
	fname <- nz.placefile(data=rawToChar(serialize(toserialize, NULL, ascii=TRUE)))
	if (is.null(output.name)) {
		return(nzQuery("SELECT * FROM TABLE WITH FINAL(nzr..", udtf, "(CAST('WORKSPACE_PATH=",
			fname, "' AS VARCHAR(256))))"))
	} else {
		nzQuery("CREATE TABLE ", output.name, " AS SELECT * FROM TABLE WITH FINAL(nzr..",
			udtf, "(CAST('WORKSPACE_PATH=", fname, "' AS VARCHAR(256))))")
		return(nz.data.frame(output.name))
	}
}
