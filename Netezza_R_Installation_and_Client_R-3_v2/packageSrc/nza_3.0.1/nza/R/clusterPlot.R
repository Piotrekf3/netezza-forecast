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

clusterPlot <- function (
		clusterModel,
		n.nearest=10,
		n.farest=10,
		select=NULL,
		points.pch=NULL,
		points.cex=NULL,
		points.col=NULL,
		cluster.names=NULL,
		cluster.pch=NULL,
		cluster.cex=NULL,
		cluster.col=NULL,
		...) {
	
	n.cluster <- nrow(clusterModel$centers)
	
	tryCatch({
				tmpView <- nzCreateView(attr(clusterModel, "data"))
				if (nzr::is.nz.data.frame(clusterModel$cluster)) {
					cluster <- nzr:::nzdf.qualified.name(clusterModel$cluster)
					id <- attr(clusterModel, "id")
					
					points <- list()
					for (cid in 1:n.cluster) {
						nearest <- nzr::nzQuery(paste0("SELECT * FROM ", tmpView, " 
												INNER JOIN ", cluster, " ON ", tmpView, ".", dQuoteSimple(id), "=", cluster, ".ID 
												WHERE CLUSTER_ID = ", cid, "
												ORDER BY DISTANCE ",
										if (n.nearest>=0) paste0("LIMIT " ,n.nearest) ,";"))
						farest <- nzr::nzQuery(paste0("SELECT * FROM ", tmpView, " 
												INNER JOIN ", cluster, " ON ", tmpView, ".", dQuoteSimple(id), "=", cluster, ".ID 
												WHERE CLUSTER_ID = ", cid, "
												ORDER BY DISTANCE DESC ",
										if (n.farest>=0) paste0("LIMIT " ,n.farest) ,";"))
						points <- rbind(points, nearest)
						points <- rbind(points, farest)
						points.clusters <- as.numeric(points$CLUSTER_ID)
						if ("ID.1" %in% names(points)) {
							points.data <- subset(points, select = -c(ID, ID.1, CLUSTER_ID,DISTANCE))
						} else {
							points.data <- subset(points, select = -c(ID, CLUSTER_ID, DISTANCE))
							points.data <- subset(points.data, select = (id!=names(points.data)))
						}
					}
				} else {
					id <- attr(clusterModel, "id")
					points.clusters <- as.numeric(clusterModel$cluster)
					names(points.clusters) <- NULL
					points.data <- nzr::as.data.frame(nz.data.frame(tmpView))
					points.data <- points.data[order(points.data[[id]]), ]
					points.data <- subset(points.data, select = (id!=names(points.data)))
				}
			}, error = function(e) {
				# in case of error, let user know what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(tmpView)
			}
	)
	
	# apply select parameter if specified
	if (!is.null(select)) {
		points.data <- subset(points.data, select = select)
	}
	
	# replace non-numeric columns by factors
	numeric <- sapply(points.data, is.numeric)
	points.data[!numeric] <- lapply(points.data[!numeric], function(x) {as.factor(x)})
	
	# order columns of centroid information
	centers.data <- subset(clusterModel$centers, select=names(points.data))
	
	# create all.data and all.clusters
	all.data <- rbind(points.data, centers.data)
	all.clusters <- c(points.clusters, 1:n.cluster)
	
	# new pot should get a new device
	trellis.device()
	
	# account for user parameters
	super.sym <- trellis.par.get("superpose.symbol")
	
	if (is.null(points.pch)) {
		points.pch="o"
	}
	if (is.null(points.cex)) {
		points.cex=0.8
	}
	if (is.null(points.col)) {
		points.col=super.sym$col[points.clusters]
	}
	if (is.null(cluster.names)) {
		cluster.names=list(paste("Cluster", 1:n.cluster))
	}
	if (is.null(cluster.pch)) {
		cluster.pch="+"
	}
	if (is.null(cluster.cex)) {
		cluster.cex=1.6
	}
	if (is.null(cluster.col)) {
		cluster.col=rep(1, times=n.cluster)
	}
	
	# workaround: will not work with other variable name
	groups <- all.clusters
	
	# do plot
	splom(all.data,
			groups = groups,
			panel = function(x, y) {
				panel.grid(h = -1, v = -1)
				panel.splom(x, y,
						pch=c(rep(points.pch, times=nrow(all.data)-n.cluster), rep(cluster.pch, times=n.cluster)),
						cex=c(rep(points.cex, times=nrow(all.data)-n.cluster), rep(cluster.cex, times=n.cluster)),
						col=c(points.col, cluster.col)
				)
			},
			key = list(
					points = list(
							pch = super.sym$pch[1:n.cluster],
							col = super.sym$col[1:n.cluster]
					),
					text = cluster.names,
					columns = ceiling(sqrt(n.cluster))
			),
			superpanel = function(...) {
				panel.pairs(
						varname.cex=0.8,
						axis.text.alpha=0.8,
						axis.text.cex=0.8,
						axis.line.alpha=0.8,
						...,
				)
			}
	)
}

#------------------------------------------------------------------------------

plot.nzTwoStep <- clusterPlot
plot.nzKMeans <- clusterPlot