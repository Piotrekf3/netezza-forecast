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

nzTwoStep <- function(data, outtable=NULL, id="id", distance="loglikelihood", k=0, maxk=20, statistics="none", bins=10, randseed=12345, distancethreshold=0, distancethresholdfactor=2, epsilon=0, nodecapacity=6, leafcapacity=8, maxleaves=1000, outlierfraction=0, model=NULL, dropAfter=F, getLabels=F, format="cluster", ...) {
	if (is.null(model)) {
		model <- nzGetValidModelName(paste(data@table,"_model",sep=""))
	} else if (modelExist(model)) {
		stop("Model name already exists.")
	}
	
	columns <- data@cols
	if (!(id %in% columns))
		stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
	id.no.quotes <- id
	id <- dQuoteSimple(id)
	
	if (is.null(outtable)) {
		outtable <- nzGetValidTableName(paste(data@table,"_model",sep=""))
	} else if (nzExistTable(model)) {
		stop("Outtable name already exists.")
	}

	tmpView <- nzCreateView(data)
	
	tryCatch({
				callSP("nza..TWOSTEP",
						model=model,
						intable=tmpView,
						outtable=outtable,
						id=id,
						distance=distance,
						k=k,
						maxk=maxk,
						statistics=statistics,
						bins=bins,
						randseed=randseed,
						distancethreshold=distancethreshold,
						distancethresholdfactor=distancethresholdfactor,
						epsilon=epsilon,
						nodecapacity=nodecapacity,
						leafcapacity=leafcapacity,
						maxleaves=maxleaves,
						outlierfraction=outlierfraction,
						... )
			}, error = function(e) {
				# in case of error, let user know what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(tmpView)
			}
	)
	
	if (format=="cluster") {
		output <- nzTwoStep.format.cluster(model, outtable, distance, getLabels)
	}
	if (format=="raw") {
		output <- nzTwoStep.format.raw(model, outtable)
	}
	
	if(dropAfter) {
		modelDrop(model)
		nzDeleteTable(outtable)
		if (!getLabels) {
			output$cluster <- NULL
		}
	}
	
	# attributes needed for cluster plotting
	attr(output, "data") <- data
	attr(output, "id") <- id.no.quotes
	
	return(output)
}

#------------------------------------------------------------------------------

nzTwoStep.format.cluster <- function(model, outtable, distance, getLabels) {
	# results are converted to kmeans object
	# - how may clusters have been created?
	modelStr <- paste("INZA.NZA_META_",model,"_MODEL",sep="")
	modelStats <- nzr::as.data.frame(nz.data.frame(modelStr))
	k = modelStats[1,4]
	
	# - get data from COLUMN_STATISTICS
	modelStr <- paste("INZA.NZA_META_",model,"_COLUMN_STATISTICS",sep="")
	columnStats <- nzr::as.data.frame(nz.data.frame(modelStr))
	
	# - select and order by CLUSTERID and COLUMNNAME
	columnStats <- columnStats[columnStats[,1]>0,]
	columnStats <- columnStats[order(columnStats[,1], columnStats[,2]),]
	
	# - compute tabular with cluster means
	centCl <- matrix(columnStats[,4], nrow=k, byrow=T) 
	centNum <- matrix(columnStats[,7], nrow=k, byrow=T) 
	colnames(centCl) = columnStats[columnStats[,1]==1,2]
	colnames(centNum) = columnStats[columnStats[,1]==1,2]
	cents <- data.frame(centCl[,!is.na(centCl[1,]),drop=F], centNum[,!is.na(centNum[1,]),drop=F])
	
	# - get cluster statistics for size and withinss
	modelStr <- paste("INZA.NZA_META_",model,"_CLUSTERS",sep="")
	clusterStats <- nzr::as.data.frame(nz.data.frame(modelStr))
	clusterStats <- clusterStats[order(clusterStats[,1]),]
	
	output = list(
			cluster=nz.data.frame(outtable),
			centers = cents, 
			withinss = clusterStats$WITHINSS,
			size = clusterStats$SIZE,
			distance = distance, model=model
	)
	
	if (getLabels) {
		tmp2 <- nzr::as.data.frame(output$cluster)
		colnames(tmp2) <- toupper(colnames(tmp2))
		tmp3 <- tmp2[order(tmp2$ID),"CLUSTER_ID"]
		names(tmp3) <- sort(tmp2$ID)
		output$cluster <- tmp3
	}
	
	class(output) <- c("nzTwoStep")
	
	return(output)
}

#------------------------------------------------------------------------------

nzTwoStep.format.raw <- function(modelname, outtable) {
	modelStr <- paste("INZA.NZA_META_",modelname,"_CLUSTERS",sep="")
	clusters <- nzr::as.data.frame(nz.data.frame(modelStr))
	
	modelStr <- paste("INZA.NZA_META_",modelname,"_COLUMNS",sep="")
	columns <- nzr::as.data.frame(nz.data.frame(modelStr))
	
	modelStr <- paste("INZA.NZA_META_",modelname,"_COLUMN_STATISTICS",sep="")
	column.statistics <- nzr::as.data.frame(nz.data.frame(modelStr))
	
	modelStr <- paste("INZA.NZA_META_",modelname,"_DISCRETE_STATISTICS",sep="")
	discrete.statistics <- nzr::as.data.frame(nz.data.frame(modelStr))
	
	modelStr <- paste("INZA.NZA_META_",modelname,"_MODEL",sep="")
	model <- nzr::as.data.frame(nz.data.frame(modelStr))
	
	modelStr <- paste("INZA.NZA_META_",modelname,"_NUMERIC_STATISTICS",sep="")
	numeric.statistics <- nzr::as.data.frame(nz.data.frame(modelStr))
	
	return(list(
					clusters=clusters,
					columns=columns,
					column.statistics=column.statistics,
					discrete.statistics=discrete.statistics,
					model=model,
					numeric.statistics=numeric.statistics,
					modelname=modelname,
					cluster=outtable
			))
}

#------------------------------------------------------------------------------

# Equivalent to print.kmeans
print.nzTwoStep <- function (x, ...) {
	cat("TwoStep clustering with ", length(x$size), " clusters of sizes ", paste(x$size, collapse = ", "), "\n", sep = "")
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

# Taken from predict.kmeans
predict.nzTwoStep <- function(object, newdata, id="id", ...) {
	outtable <- nzGetValidTableName(paste(newdata@table,"_predict",sep=""))	
	
	colu = newdata@cols
	if (!(id %in% colu))
		stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
	
	id  <- dQuoteSimple(id)
	tmpView <- nzCreateView(newdata)
	
	tryCatch({	
				callSP("nza..PREDICT_TWOSTEP",
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