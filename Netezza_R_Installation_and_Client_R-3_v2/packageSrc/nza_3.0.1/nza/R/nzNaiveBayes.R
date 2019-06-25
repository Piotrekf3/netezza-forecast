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

nzNaiveBayes <- function(form, data, id="id", relative=F, model=NULL, format="naiveBayes", dropAfter=F, ...) {
	
	ntab1 <- nzParseRFormula(form, data)
	varY  <- dQuoteSimple(ntab1$varlist[1])
	
	if (is.null(model)) {
		model <- nzGetValidModelName(paste(data@table,"_model",sep=""))
	} else if (modelExist(model)) {
		stop("Model name already exists.")
	}
	
	colu <- data@cols
	if (!(id %in% colu))
		stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
	
	dataTmp <- data[,which(data@cols %in% c(ntab1$varlist, id))]
	tmpView <- nzCreateView(dataTmp)
	
	id  <- dQuoteSimple(id)
	
	tryCatch({
				callSP("nza..NAIVEBAYES ", model=model, intable=tmpView, id=id, target=varY, ...)
			}, error = function(e, tmpView) {
				# in case of error, drop view and let user know, what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(tmpView)
			}
	)
	
	if (format=="naiveBayes") {
		result <- nzNaiveBayes.format.naiveBayes(model, relative)
	} else if (format=="raw") {
		result <- nzNaiveBayes.format.raw(model)
	}
	
	if (dropAfter) {
		modelDrop(model)
	}
	
	return(result)
}

#------------------------------------------------------------------------------

nzNaiveBayes.format.naiveBayes <- function(model, relative) {
	# results are converted to naiveBayes object
	model2 <- paste0("INZA.NZA_META_",model,"_MODEL")
	nbOut <- nzr::as.data.frame(nz.data.frame(model2))
	
	if(nzIsUpper()) {
		class <- "CLASS"
		val <- "VAL"
		attribute <- "ATTRIBUTE"
		classvalcount <- "CLASSVALCOUNT"
		attrclasscount <- "ATTRCLASSCOUNT"
		colname <- "COLNAME"
	} else {
		class <- "class"
		val <- "val"
		attribute <- "attribute"
		classvalcount <- "classvalcount"
		attrclasscount <- "attrclasscount"
		colname <- "colname"
	}
	
	classes <- unique(nbOut[[class]])	
	tables <- list()
	xVars <- unique(nbOut[[attribute]])
	for (i in 1:length(xVars)) {
		xVar <- xVars[i]
		tmp <- nbOut[nbOut[[attribute]]==xVar, ]
		values <- unique(tmp[[val]])
		tab <- matrix(0, length(classes), length(values))
		names.list <- list()
		names.list[["Y"]] <- classes
		names.list[[xVar]] <- values
		dimnames(tab) <- names.list
		if (relative) {
			for (j in 1:nrow(tmp)) {
				tab[tmp[j,class], tmp[j,val]] <- tmp[j,classvalcount] / tmp[j,attrclasscount]
			}
		} else {
			for (j in 1:nrow(tmp)) {
				tab[tmp[j,class], tmp[j,val]] <- tmp[j,classvalcount]
			}
		}
		tables[[i]] <- tab
		names(tables)[i] <- xVar
	}
	
	# check if there are automatically made splits for continuous values
	disc.name <- paste0("INZA.NZA_META_",model,"_DISC")
	if (nzExistTable(disc.name)) {
		splits.rcv <- nzr::as.data.frame(nz.data.frame(disc.name))
		classes <- unique(splits.rcv[[colname]])
		splits <- list()
		for (i in 1:length(classes)) {
			splits[[classes[i]]] <- splits.rcv[splits.rcv[[colname]] == classes[i], "BREAK"]
		}
	} else {
		splits <- list()
	}
	
	# apriori property
	apriori <- tapply(nbOut[,6], nbOut[,3], mean, na.rm=T)[unique(nbOut[[class]])]
	
	# prepare object of the class nzNaiveBayes
	new.nb <- list(apriori = apriori, tables = tables, levels = classes, splits = splits, call = sys.call(-1), model = model, modelTable = model2)
	
	class(new.nb) <- c("nzNaiveBayes", "naiveBayes")
	
	return(new.nb)
}

#------------------------------------------------------------------------------

nzNaiveBayes.format.raw <- function(model) {
	model2 <- paste("INZA.NZA_META_",model,"_MODEL",sep="")
	nbOut <- nzr::as.data.frame(nz.data.frame(model2))
	
	disc.name <- paste0("INZA.NZA_META_",model,"_DISC")
	if (nzExistTable(disc.name)) {
		splits <- nzr::as.data.frame(nz.data.frame(disc.name))
	} else {
		splits <- NA
	}
	
	return(list(model=nbOut, splits=splits, modelname=model))
}

# ----------------------------------------------------------------------

predict.nzNaiveBayes <- function(object, newdata, id="id", ...) {
	results <- nzGetValidTableName(paste0(newdata@table,"_predict"))
	
	colu = newdata@cols
	if (!(id %in% colu))
		stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
	
	id  <- dQuoteSimple(id)
	
	tmpView <- nzCreateView(newdata)
	
	tryCatch({
				callSP("nza..PREDICT_NAIVEBAYES ", model=object$model, intable=tmpView, id=id, outtable=results, ...)
			}, error = function(e, tmpView) {
				# in case of error, drop view and let user know, what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(tmpView)
			}
	)
	
	object.pred <- nz.data.frame(results)
	return(object.pred)
}