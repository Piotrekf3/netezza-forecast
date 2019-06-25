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
#

nzStats <- function (type = "system", user = "admin", password = "password", print = TRUE) {
	types <- c("dbms", "system", "database", "host", "hostCpu", "hostFileSystem",
			  "hostIf", "hostMgmtChan", "hostNet", "hwMgmtChan", "query", "queryHist",
			  "spu", "spuPartition", "table", "tableDataSlice", "reclaim")

	if (!is.element(type, types))
		stop(paste("uknown type", type))

	fun <- function (type) {
		getNext()
		lines <- readLines(pipe(paste("LD_LIBRARY_PATH=/usr/lib:/lib nzstats show -u ", user, " -pw ",  password, " -type ", type)))
		for (line in lines) {
			setOutput(0, line)
			outputResult()
		}
	}
	environment(fun) <- emptyenv()

	res <- nzRunHost(fun, type=type, output.signature=list(line=list(NZ.VARIABLE,1024)))
	res <- paste(res[[1]],collapse='\n')

	if (print)
		invisible(cat(res))
	else
		return(res)
}
