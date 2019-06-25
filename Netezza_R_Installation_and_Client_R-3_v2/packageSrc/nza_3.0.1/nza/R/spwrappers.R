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

# Wrappers for Data Mining algorithms implemented as SQL stored
# procedures and stored in NPS.
`spwrapper` <- function (spname, env) {
	nzCheckConnection()

	views <- c()

	tmp <- c()
	for (name in ls(env)) {
	  if (name =='env.tmp') next
		value <- get(name, envir=env)

		if (is.null(value)) {
			next
		}
		if (is.nz.data.frame(value)) {
			view <- nzCreateView(value)
			tmp[length(tmp) + 1] = paste(name, '=', view, sep='')
			views <- append(views, view)
		} else if (is.character(value) && length(grep(' ', value))) {
			tmp[length(tmp) + 1] = paste(name, '="', value, '"', sep='')
		} else if (length(value) > 1) {
			tmp[length(tmp) + 1] = paste(name, '="', paste(value, collapse=' '), '"', sep='')
		} else {
			tmp[length(tmp) + 1] = paste(name, '=', value, sep='')
		}
	}

	res <- try(nzQuery("CALL ", spname, "('", paste(tmp, collapse=','), "')"), silent=T)
	for (view in views)	nzDropView(view)
	if(inherits(res, "try-error")) {
		stop(res)
	}

	return(res)
}

# ----------------------------------------------------------------------

`spwrapperVARCHAR` <- function (spname, env) {
	retVal = spwrapper (spname, env)
	cat(levels(retVal[[1]]))
	retVal
}

# ----------------------------------------------------------------------

`spwrapperINT` <- function (spname, env) {
	invisible(spwrapper(spname, env))
}

# ----------------------------------------------------------------------

dQuoteSimple <- function(str) {
	paste0('"',str,'"')
}
