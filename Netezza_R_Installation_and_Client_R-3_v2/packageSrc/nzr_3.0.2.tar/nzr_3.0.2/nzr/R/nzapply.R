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

nzApply <- function (X, MARGIN, FUN, output.name=NULL, output.signature=NULL,
						clear.existing=FALSE,  case.sensitive=FALSE,output.distributeOn=NULL, chunkSize=1000, ...)
{
	# make sure that this is a correct data frame
	if (!is.nz.data.frame(X))
		stop("wrong data object passed - must be of class nz.data.frame")
	
	if (is.function(FUN))
		environment(FUN) <- baseenv() #emptyenv()

	# set up the datafile query
#	if (is.null(cappend))
		toserialize = list(fun=FUN, args=list(...), mode='apply',chunk=chunkSize)
#	else 
#		toserialize = list(fun=FUN, args=list(...), mode='apply', chunk=1)

	if (exists("p_nzLibrary") && !is.null(p_nzLibrary))
			toserialize$nzlibrary <- p_nzLibrary
	# run the AE
	return(nz.run.rae(X,
	                  toserialize,
	                  output.name,
	                  output.signature,
	                  clear.existing,
					  case.sensitive,
					  output.distributeOn))
}

# set a S4 method
setMethod("apply", signature(X="nz.data.frame"), nzApply)

