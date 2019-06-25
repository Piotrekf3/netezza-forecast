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

nzRun <- function (x, fun, output.name=NULL, output.signature=NULL, clear.existing=FALSE, case.sensitive=FALSE, output.distributeOn=NULL,...)
{
	if (!is.nz.data.frame(x))
		stop("wrong data object passed - must be of class nz.data.frame")
	
	
	if (is.function(fun))
		environment(fun) <- emptyenv()
	else
		stop('currently only function object `fun` is supported')
	
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
	
	# send the file 
	toserialize = list(fun=fun, args=args, mode='run') 
	if (exists("p_nzLibrary") && !is.null(p_nzLibrary))
		toserialize$nzlibrary <- p_nzLibrary
	
	return(nz.run.rae (x, toserialize, output.name, output.signature, clear.existing,case.sensitive,output.distributeOn))
}
