# 
# Copyright (c) 2011, 2012, Revolution Analytics. All rights reserved.
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

# wrapper for PMML_MODEL
model2pmml <- function(x, ...) {
	if (is.character(x)) {
		nam <- x
	} else  if (is.character(x$model)) {
		nam <- x$model
	} else  if (is.character(x$modelname)) {
		nam <- x$modelname
	} else stop(simpleError("x should be a model name"))
	
	ll <- list(...)
	if (length(ll)>0) {
		args <- paste(",",names(ll), "=", unlist(ll), sep="",collapse="")
	} else {
		args <- NULL
	}
	nzQuery("CALL nza..PMML_MODEL('model=",nam,args,"');")
}

# ----------------------------------------------------------------------

# wrapper for DROP_MODEL
# modelDrop(newtree)
modelDrop <- function(x, ...) {
	if (is.character(x)) {
		name <- x
	} else  if (is.character(x$model)) {
		name <- x$model
	} else  if (is.character(x$modelname)) {
		nam <- x$modelname
	} else {
		stop(simpleError("x should be a model name"))
	}
	nzQuery("CALL nza..DROP_MODEL('model=", name, "');")
}

# ----------------------------------------------------------------------

# wrapper for model_exists
modelExist <- function(x, ...) {
	if (is.character(x)) {
		nam <- x
	} else  if (is.character(x$model)) {
		nam <- x$model
	} else  if (is.character(x$modelname)) {
		nam <- x$modelname
	} else stop(simpleError("x should be a model name"))
	
	ll <- list(...)
	if (length(ll)>0) {
		args <- paste(",",names(ll), "=", unlist(ll), sep="",collapse="")
	} else {
		args <- NULL
	}
	tmp <- nzQuery("CALL nza..model_exists('model=",nam,args,"');") 
	return(tmp == "T" || tmp == "t")
}

# ----------------------------------------------------------------------

# wrapper for list_models
modelList <- function(...) {
	nzQuery("SELECT * FROM INZA.V_NZA_MODELS")
}

# ----------------------------------------------------------------------

nzGetValidModelName <- function (prefix = "model") {
	prefix <- ifelse(nzIsUpper(),toupper(prefix),tolower(prefix)) 
	while (TRUE) {
		name = paste(prefix, floor(runif(1,0,100000)), sep="")
		if (!(modelExist(name)))
			return(name)
	}
}
