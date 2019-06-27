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

callSP <- function (spname, ...) {
	nzCheckConnection()
	if (length(grep( "\\.\\.",spname))==0)
		spname = paste("nza..", spname, sep="")
	args = list(...)
	views <- c()
	tmp <- c()
	for (name in names(args)) {
		value <- args[[name]]
		if (is.null(value))   next
		if (is.nz.data.frame(value)) {
			view <- nzCreateView(value)
			tmp[length(tmp) + 1] = paste(name, "=", view, sep = "")
			views <- append(views, view)
		}
		else if (is.character(value) && length(grep(" ", value))) 
			tmp[length(tmp) + 1] = paste(name, "=\"", value, "\"", sep = "")
		else if (length(value) > 1) 
			tmp[length(tmp) + 1] = paste(name, "=\"", paste(value, collapse = " "), "\"", sep = "")
		else 
			tmp[length(tmp) + 1] = paste(name, "=", value, sep = "")
	}
	res <- try(nzQuery("CALL ", spname, "('", paste(tmp, collapse = ","), "')"), silent=T)
	for (view in views) nzDropView(view)
	if(inherits(res, "try-error")) {
		stop(res)
	}
	return(invisible(res))
}