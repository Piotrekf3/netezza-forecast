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

library(utils)

.onLoad <- function (libname, pkgname) {
	#check R version nzr was built with:
	nzaRver <- strsplit(utils::packageDescription("nza", fields=c("Built")) , ";")[[1]][1]

	#check client R version
	cliver <- paste("R ",version$major,".",version$minor,sep='')
		
	if (!identical(nzaRver, cliver)) {
		warning(paste("package 'nza' was built under R version ", substr(nzaRver,3,nchar(nzaRver)),sep=''), call.=FALSE)
	}
}
