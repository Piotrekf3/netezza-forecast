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

nzKMeans <- function(
		data,
		k=2,
		maxiter=10,
		distance="euclidean",
		outtable=NULL,
		id="id",
		getLabels=F,
		randseed=1234,
		format="kmeans",
		model=NULL,
		dropAfter=F,
		...) {
	
	if (all(is.null(data), is.null(model))) {
		stop("Plase specify either data or model.")
	}
	
	# switch to retrieve mode, if necessary
	if (all(is.null(data), !is.null(model))) {
		id.no.quotes <- id
		if (format == "kmeans") {
			result <- nzKMeans.format.kmeans(model, k, outtable, distance, getLabels)
		} else if (format == "raw") {
			result <- nzKMeans.format.raw(model, outtable, getLabels)
		}
	} else {	
		colu = data@cols
		if (!(id %in% colu))
			stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
		id.no.quotes <- id
		id  <- dQuoteSimple(id)
		
		if (is.null(model)) {
			model <- nzGetValidModelName(paste(data@table,"_model",sep=""))
		} else if (modelExist(model)) {
			stop("Model name already exists.")
		}
		
		if (is.null(outtable)) {
			outtable <- nzGetValidTableName(paste(data@table,"_model",sep=""))
		} else if (nzExistTable(model)) {
			stop("Outtable name already exists.")
		}
		
		tmpView <- nzCreateView(data)
		
		tryCatch({	
					res <- callSP("nza..KMEANS ",
							model=model,
							intable=tmpView,
							k=k,
							maxiter=maxiter,
							outtable=outtable,
							distance=distance,
							id=id,
							randseed=randseed,
							...)
					actual.k <- as.numeric(res[1,1])
				}, error = function(e) {
					# in case of error, let user know what happend
					stop(e)
				}, finally = {
					# drop view
					nzDropView(tmpView)
				}
		)
		
		if (format=="kmeans") {
			result <- nzKMeans.format.kmeans(model, actual.k, outtable, distance, getLabels)
		} else if (format=="raw") {
			result <- nzKMeans.format.raw(model, outtable, getLabels)
		}
	}
	
	if (dropAfter) {
		modelDrop(model)
		nzDeleteTable(outtable)
	}
	
	# attributes needed for cluster plotting
	attr(result, "data") <- data
	attr(result, "id") <- id.no.quotes
	
	return(result)
}

#------------------------------------------------------------------------------

nzKMeans.format.kmeans <- function(model, k, outtable, distance, getLabels) {
	# results are converted to kmeans object
	model2 <- paste("INZA.NZA_META_",model,"_MODEL",sep="")
	
	model4 <- paste("INZA.NZA_META_",model,"_COLUMN_STATISTICS",sep="")
	kols <- nzr::as.data.frame(nz.data.frame(model4))
	kols <- kols[kols[,1]>0,]
	kols <- kols[order(kols[,1], kols[,2]),]
	
	centCl <- matrix(kols[,4], nrow=k, byrow=T) 
	centNum <- matrix(kols[,7], nrow=k, byrow=T) 
	colnames(centCl) = kols[kols[,1]==1,2]
	colnames(centNum) = kols[kols[,1]==1,2]
	cents <- data.frame(centCl[,!is.na(centCl[1,]),drop=F], centNum[,!is.na(centNum[1,]),drop=F])
	
	model3 <- paste("INZA.NZA_META_",model,"_CLUSTERS",sep="")
	kmOutStat <- nzr::as.data.frame(nz.data.frame(model3))
	kmOutStat <- kmOutStat[order(kmOutStat[,1]),]
	
	cluster <- if (is.null(outtable)) NULL else nz.data.frame(outtable)
	
	tmp = list(
			cluster=cluster,
			centers=cents, 
			withinss=kmOutStat$WITHINSS,
			size=kmOutStat$SIZE,
			distance=distance,
			model=model
	)
	
	if (getLabels) {
		tmp2 = nzr::as.data.frame(tmp$cluster)
		colnames(tmp2) <- toupper(colnames(tmp2))
		tmp3 = tmp2[order(tmp2$ID),"CLUSTER_ID"]
		names(tmp3) = sort(tmp2$ID)
		tmp$cluster = tmp3
	}
	
	class(tmp) = c("nzKMeans", "kmeans")
	return(tmp)
}

#------------------------------------------------------------------------------

nzKMeans.format.raw <- function(modelname, outtable, getLabels) {
	clusters <- nzr::as.data.frame(nz.data.frame(paste("INZA.NZA_META_",modelname,"_CLUSTERS",sep="")))
	columns <- nzr::as.data.frame(nz.data.frame(paste("INZA.NZA_META_",modelname,"_COLUMNS",sep="")))
	column.statistics <- nzr::as.data.frame(nz.data.frame(paste("INZA.NZA_META_",modelname,"_COLUMN_STATISTICS",sep="")))
	model <- nzr::as.data.frame(nz.data.frame(paste("INZA.NZA_META_",modelname,"_MODEL",sep="")))
	centroids <- if(getLabels) nzr::as.data.frame(nz.data.frame(outtable)) else nz.data.frame(outtable)
	
	return(list(clusters=clusters, columns=columns, column.statistics=column.statistics, model=model, centroids=centroids, modelname=modelname))
}

#------------------------------------------------------------------------------

# Taken from print.kmeans
print.nzKMeans <- function (x, ...) {
	cat("KMeans clustering with ", length(x$size), " clusters of sizes ", paste(x$size, collapse = ", "), "\n", sep = "")
	cat("\nCluster means:\n")
	print(x$centers, ...)
	cat("\nClustering vector:\n")
	print(x$cluster, ...)
	cat("\nWithin cluster sum of squares by cluster:\n")
	print(x$withinss, ...)
	# Sum of square statistics from original print.kmeans left out ....
	cat("\nAvailable components:\n")
	print(names(x))
	invisible(x)
}

#------------------------------------------------------------------------------

predict.nzKMeans <- function(object, newdata, id="id", ...) {
	outtable <- nzGetValidTableName(paste(newdata@table,"_predict",sep=""))
	
	colu = newdata@cols
	if (!(id %in% colu))
		stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
	
	id  <- dQuoteSimple(id)
	tmpView <- nzCreateView(newdata)
	
	tryCatch({	
				callSP("nza..PREDICT_KMEANS ",
						model=object$model,
						intable=tmpView,
						id=id,
						outtable=outtable,
						...)
			}, error = function(e) {
				# in case of error, let user know what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(tmpView)
			}
	)
	
	object.pred <- nz.data.frame(outtable)
	return(object.pred)
}
