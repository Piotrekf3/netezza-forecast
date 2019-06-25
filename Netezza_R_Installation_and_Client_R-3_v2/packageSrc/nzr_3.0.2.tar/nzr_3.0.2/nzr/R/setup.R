# 
# Copyright (c) 2010, 2011, IBM Corp. All rights reserved. 
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

# when connecting to NPS this function is called to set up data types ids

setupNpsDataTypes <- function () {
	
	aedata <- list(
		mode = 'run',
		fun  = function () {
			getNext()
			dts <- getNpsDataTypes()
			for (i in seq_along(dts)) {
				setOutput(0, dts[i])
				setOutput(1, names(dts[i]))
				outputResult()
			}
		},
		args = list()
	)

	environment(aedata$fun) <- emptyenv()
	
	dts <- nzt(nzQuery("SELECT * FROM TABLE WITH FINAL(nzr..r_udtf('CODE_SERIALIZED=",
				encodebase64(rawToChar(serialize(aedata, NULL, ascii=TRUE))),
				"'))"))
	apply(dts, 1, function(x) {	assign(paste('NZ.',x[2], sep=''),
						as.integer(x[1]), envir=globalenv()) } )

	#case sensitiveness
	assign('NZ.IDENTIFIER_CASE', nzScalarQuery("SELECT UPPER(IDENTIFIER_CASE)"), envir=globalenv())
	
	invisible()
}
