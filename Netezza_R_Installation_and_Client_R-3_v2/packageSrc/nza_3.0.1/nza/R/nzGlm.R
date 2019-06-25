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

nzGlm <- function(form,
		data,
		id  = "id",
		intercept = T,
		family = "bernoulli",
		family_param = -1,
		link = "logit",
		link_param = 1,
		maxit = 20,
		eps = 1e-3,
		tol = 1e-7,
		method = "irls",
		trials = NULL,
		incolumn = "",
		interaction = "",
		model = NULL, 
		format = "glm",
		raw.resid = F,
		dropAfter = F,
		...) {
	
	if (all(is.null(data), is.null(model))) {
		stop("Plase specify either data or model.")
	}
	
	# switch to retrieve mode, if necessary
	if (all(is.null(data), !is.null(model))) {
		if (format == "glm") {
			result <- nzGlm.format.glm(model, call=NULL, id, form, data=NULL)
		} else if (format == "raw") {
			result <- nzGlm.format.raw(model, raw.resid)
		}
	} else {	
		call <- match.call()
		ntab1 = nzParseRFormula(form, data)
		# dataTmp preparation to run the SQL procedure
		varY  <- paste('\"',ntab1$varlist[1],'\"',sep="")
		dataTmp = data[, which(data@cols %in% c(ntab1$varlist, id, trials))]
		
		if(is.null(model)) {
			model <- nzGetValidModelName(paste(data@table,"_model",sep=""))
		} else if (modelExist(model)) {
			stop("Model name already exists.")
		}
		
		colu = data@cols
		if (!(id %in% colu))
			stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
		id <- paste('\"',id,'\"',sep="")
		
		# check for factors, unless overwritten by user
		if (incolumn == "") {
			if (sum(ntab1$areFactors)!=0) {
				incolumn <- paste(dQuoteSimple(ntab1$varlist[ntab1$areFactors]),collapse=":nom;")
				incolumn <- paste(incolumn,":nom;",sep="")
			} else {
				incolumn <- NULL
			}
		}
		
		# check for interaction terms, unless overwritten by user
		if (interaction == "") {
			if (length(ntab1$siordervars)!=0) {
				interaction <- paste(dQuoteSimple(apply(ntab1$siordervars,2,function(x) paste(ntab1$varlist[x],collapse='*'))),collapse=';')
			} else {
				interaction <- NULL
			}
		}
		
		# check for trials parameter
		if(!is.null(trials)) {
			trials <- dQuoteSimple(trials)
		}
		
		dataTmpv = nzCreateView(dataTmp)
		
		tryCatch({	
					callSP("nza..GLM ",
							model=model,
							intable=dataTmpv,
							id=id,
							target=varY,
							family=family,
							family_param=family_param,
							link=link,
							link_param=link_param,
							maxit=maxit,
							eps=eps,
							tol=tol,
							method=method,
							trials=trials,
							intercept=intercept,
							incolumn=incolumn,
							interaction=interaction,
							...)
				}, error = function(e) {
					# in case of error, let user know what happend
					stop(e)
				}, finally = {
					# drop view
					nzDropView(dataTmpv)
				}
		)
		
		if (format == "glm") {
			result <- nzGlm.format.glm(model, call, id, form, data)
		} else if (format == "raw") {
			result <- nzGlm.format.raw(model, raw.resid)
		}
	}
	
	if (dropAfter) {
		try(modelDrop(model))
	}
	
	return(result)
}

#------------------------------------------------------------------------------

nzGlm.format.glm <- function(model, call, id, form, data) {
	# results are converted
	# NOTE: tables PCOVMATRIX and STATS are no longer availiable
	dict <- paste("INZA.NZA_META_", model, "_DICTIONARY",sep="")
	model2 <- paste("INZA.NZA_META_",model,"_MODEL",sep="")
	facdic <- paste("INZA.NZA_META_", model, "_FACDIC",sep="")
	# pcov <- paste("INZA.NZA_META_",model,"_PCOVMATRIX",sep="")
	ppmat <- paste("INZA.NZA_META_",model,"_PPMATRIX",sep="")
	resids <- paste("INZA.NZA_META_",model,"_RESIDUALS",sep="")
	# stats <- paste("INZA.NZA_META_",model,"_STATS",sep="")
	
	# download results (don't download RESIDUALS, large matrix)
	dict.out <- nzr::as.data.frame(nz.data.frame(dict))
	model2.out <- nzr::as.data.frame(nz.data.frame(model2))
	facdic.out <- nzr::as.data.frame(nz.data.frame(facdic))
	# pcov.out <- nzr::as.data.frame(nz.data.frame(pcov))
	ppmat.out <- nzr::as.data.frame(nz.data.frame(ppmat))
	# stats.out <- nzr::as.data.frame(nz.data.frame(stats))
	
	# get coefficients (=beta)
	coefficients <- model2.out$BETA
	
	# get IDs from FACDIC and match proper names to coefficients
	names(coefficients) <- facdic.out$FAC_EXPRESSION[match(model2.out$FAC_ID,facdic.out$FAC_ID)] 
	
	# get residuals as nz.data.frame (select only ID and RAW)
	residuals <- nz.data.frame(resids)[,c(1,2)]
	
	# prepare summarized output by stored procedure PRINT_GLM
	summary <- callSP("nza..PRINT_GLM", model=model)
	
	##return glmOut list
	glmOut <- list(modelname=model,
			call=call,
			id=id,
			form=form,
			data=data,
			coefficients=coefficients,		
			dict=dict.out,
			model=model2.out,
			facdic=facdic.out,
			#pcov=pcov.out,
			ppmat=ppmat.out,
			residuals=residuals,
			#stats=stats.out,
			summary=summary)
	
	class(glmOut) <- c("nzGlm","glm")
	return(glmOut)
}

#------------------------------------------------------------------------------

nzGlm.format.raw <- function(model, raw.resid) {
	# results are converted
	# NOTE: tables PCOVMATRIX and STATS are no longer availiable
	dict <- paste("INZA.NZA_META_", model, "_DICTIONARY",sep="")
	model2 <- paste("INZA.NZA_META_",model,"_MODEL",sep="")
	facdic <- paste("INZA.NZA_META_", model, "_FACDIC",sep="")
	# pcov <- paste("INZA.NZA_META_",model,"_PCOVMATRIX",sep="")
	ppmat <- paste("INZA.NZA_META_",model,"_PPMATRIX",sep="")
	resids <- paste("INZA.NZA_META_",model,"_RESIDUALS",sep="")
	# stats <- paste("INZA.NZA_META_",model,"_STATS",sep="")
	
	# download results (don't download RESIDUALS, large matrix)
	dict.out <- nzr::as.data.frame(nz.data.frame(dict))
	model2.out <- nzr::as.data.frame(nz.data.frame(model2))
	facdic.out <- nzr::as.data.frame(nz.data.frame(facdic))
	# pcov.out <- nzr::as.data.frame(nz.data.frame(pcov))
	ppmat.out <- nzr::as.data.frame(nz.data.frame(ppmat))
	resids.out <- if (raw.resid) nzr::as.data.frame(nz.data.frame(resids)) else NULL
	# stats.out <- nzr::as.data.frame(nz.data.frame(stats))
	
	# create raw-output as list
	raw <- list(dictionary=dict.out, model=model2.out, facdic=facdic.out, ppmatrix=ppmat.out, residuals=resids.out, modelname=model)
	return(raw)
}

# -----------------------------------------------------------------------------

predict.nzGlm <- function(nb, newdata, id="id", ...) {
	outtable <- nzGetValidTableName(paste(newdata@table,"_predict",sep=""))
	
	colu = newdata@cols
	if (!(id %in% colu))
		stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
	
	id  <- dQuoteSimple(id)
	
	tmpView <- nzCreateView(newdata)
	
	tryCatch({
				callSP("nza..PREDICT_GLM ",
						model=nb$modelname,
						intable=tmpView,
						id=id,
						outtable=outtable, 
						... )
			}, error = function(e) {
				# in case of error, drop view and let user know, what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(tmpView)
			}
	)
	
	nb.pred <- nz.data.frame(outtable)
	return(nb.pred)
}

# -----------------------------------------------------------------------------

print.nzGlm <- function(x) {
	cat("\nModel Name\n")
	cat(x$modelname,"\n")
	cat("\nCall:")
	print(x$call)
	cat("\n\nCoefficients:\n")
	print(x$coef)
	
	trim <- function(s) {
		return(gsub("(^[\n\t ]+|[\n\t ]+$)", "", s))
	}
	
	getGlmDataIndex <- function(l, type) {
		return(match(type, l[[1]]))
	}
	
	outputList <- strsplit(gsub("[\n\t]", "", x$summary[[1]]), "\\|")
	outputList <- lapply(outputList, trim)
	
	cat("\n\nResiduals Summary:\n")
	cat("Pearson:\t\t")
	pIndex <- getGlmDataIndex(outputList, "Pearson")
	cat("RSS:", outputList[[1]][pIndex+1], "\tdf:", outputList[[1]][pIndex+2], "\tp-value:", outputList[[1]][pIndex+3])
	cat("\nDeviance:\t\t")
	dIndex <- getGlmDataIndex(outputList, "Deviance")
	cat("RSS:", outputList[[1]][dIndex+1], "\tdf:", outputList[[1]][dIndex+2], "\tp-value:", outputList[[1]][dIndex+3], "\n")
	invisible(x)
}

# -----------------------------------------------------------------------------

summary.nzGlm <- function(x) {
	cat("\nCall:")
	print(x$call)
	cat("\n\n")
	cat(x$summary[[1]])
	invisible(x$summary)
}

# -----------------------------------------------------------------------------

residuals.nzGlm <- function(x) {
	residuals.frame <- nzr::as.data.frame(x$residuals)
	id <- gsub("\\\"", "", x$id)
	res <- residuals.frame[order(residuals.frame[[id]]),][,'RAW']
	names(res) <- 1:length(res)
	return(res)
}

# -----------------------------------------------------------------------------

fitted.nzGlm <- function(x) {
	residuals.frame <- nzr::as.data.frame(x$residuals)
	id <- gsub("\\\"", "", x$id)
	residuals <- residuals.frame[order(residuals.frame[[id]]),][,'RAW']
	ntab1 = nzParseRFormula(x$form, x$data)
	data <- nzr::as.data.frame(x$data)
	yActual <- data[order(data[[id]]),][,which(names(data) %in% c(x$id,ntab1$varlist[1]))]
	res <- (yActual - residuals)
	names(res) <- 1:length(res)
	res
}
