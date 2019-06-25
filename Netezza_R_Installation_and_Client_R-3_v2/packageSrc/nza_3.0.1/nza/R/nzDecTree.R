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

# main function for tree building
nzDecTree <- function(form, data, minsplit=1000, maxdepth=62, id="id", qmeasure="wAcc", minimprove=0.01, eval="entropy", valtable = NULL, model = NULL, format = "tree", ...) {
	# compute target variable varY from from
	ntab <- nzParseRFormula(form, data)
	varY  <- paste('\"',ntab$varlist[1],'\"',sep="")
	
	# create view on required data dataTmp
	dataTmp <- data[,which(data@cols %in% c(ntab$varlist, id))]
	if (is.null(model)) {
		model <- nzGetValidModelName(paste(dataTmp@table,"_model",sep=""))
	} else if (modelExist(model)) {
		stop("Model name already exists.")
	}
	tmpView <- nzCreateView(dataTmp)
	
	if(!is.null(valtable)) {
		valview <- nzCreateView(valtable)
	} else {
		valview <- NULL
	}
	
	tryCatch({	
				# check if given id is valid
				colu = dataTmp@cols
				if (!(id %in% colu))
					stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
				id  <- paste('\"',id,'\"',sep="")
				
				callSP("nza..DECTREE ", model = model, intable = tmpView, id = id, target = varY, 
						minsplit = minsplit, maxdepth = maxdepth, qmeasure = qmeasure, 
						valtable = valview, minimprove = minimprove, eval = eval, ...)
			}, error = function(e, tmpView) {
				# in case of error, drop view and let user know, what happend
				stop(e)
			}, finally = {
				# drop views
				nzDropView(tmpView)
				if(!is.null(valview)) {
					valview <- nzDropView(valview)
				}
			}
	)
	
	# read results from database
	modelStr <- paste("INZA.NZA_META_",model,"_NODES",sep="")
	nodes <- nzr::as.data.frame(nz.data.frame(modelStr))
	modelStr <- paste("INZA.NZA_META_",model,"_PREDICATES",sep="")
	predicates <- nzr::as.data.frame(nz.data.frame(modelStr))
	modelStr <- paste("INZA.NZA_META_",model,"_COLUMNS",sep="")
	columns <- nzr::as.data.frame(nz.data.frame(modelStr))
	modelStr <- paste("INZA.NZA_META_",model,"_DISCRETE_STATISTICS",sep="")
	discrete <- nzr::as.data.frame(nz.data.frame(modelStr))
	
	# sort data by NODEID (no need to sort columns)
	nodes <- nodes[order(nodes[,"NODEID"]),]
	predicates <- predicates[order(predicates[,"NODEID"]),]
	discrete <- discrete[order(discrete[,"NODEID"]),]
	
	# interprete CLASS and VALUE as factors, ISLEAF as logical
	nodes$CLASS <- as.factor(nodes$CLASS)	
	discrete$VALUE <- as.factor(discrete$VALUE) # contains all possible classes
	nodes$ISLEAF <- as.logical(toupper(nodes$ISLEAF))
	
	# compute availiable classes(=ylevels) and xlevels
	# create tree$terms
	classes <- levels(discrete$VALUE)
	terms <- terms(form, data=head(data))
	xlevels <- attr(terms, "term.labels")
	
	
	# create tree$modelTable
	modelTable <- paste("INZA.NZA_META_",model,"_Model",sep="")
	
	# create a tree-like or rpart-like object
	if (format == "tree") {
		
		# prepare tree$frame$var
		var <- matrix("", nrow=nrow(nodes))
		levels(var) <- columns$COLUMNNAME
		
		# compute tree$frame$yval
		yval <- nodes$CLASS
		
		# compute tree$frame$n
		n <- nodes$SIZE
		
		# tree$frame$dev is not given
		dev <- NA
		
		# compute tree$frame$var, tree$frame$splits and tree$frame$yprob
		splits <- matrix("", nrow=nrow(nodes), ncol=2, byrow=T)
		colnames(splits) <- c("cutleft", "cutright")
		
		yprob <- matrix(0, nrow=nrow(nodes), ncol=length(classes))
		
		for (i in 1:nrow(nodes)) {  	
			j <- predicates$NODEID[i]
			if(!nodes$ISLEAF[predicates$NODEID == j]) {
				var[i] <- predicates$COLUMNNAME[predicates$NODEID == 2*j]
				sValue <- predicates$VALUE[predicates$NODEID == 2*j]
				if (predicates$OPERATOR[predicates$NODEID == 2*j] == "lessOrEqual") {
					splits[i,1] <- paste("<",sValue,sep="")
					splits[i,2] <- paste(">",sValue,sep="")
				} else if (predicates$OPERATOR[predicates$NODEID == 2*j] == "equal") {
					splits[i,1] <- paste("=",sValue,sep="")
					splits[i,2] <- paste("<>",sValue,sep="")
				}
			} else {
				var[i] <- "<leaf>"
			}
			
			for (k in 1:length(classes)) {
				yprob[i,k] <- discrete[discrete$NODEID==j & discrete$VALUE==classes[k],"RELFREQUENCY"]
			}
		}
		
		var <- as.factor(var)
		colnames(yprob) <- classes
		
		# create tree$frame
		frame <- data.frame(var = var, n = n, dev = dev, yval = yval, splits = I(splits), yprob = I(yprob), stringsAsFactors = FALSE)		
		rownames(frame) = nodes$NODEID
		
		# put nodes into right order
		score <- (nodes$NODEID - 2^(floor(log(nodes$NODEID,2))  +0.5)) / 2^(floor(log(nodes$NODEID,2)+1))
		frame <- frame[order(score),]
		
		# create tree
		tree <- list(frame = frame, terms = terms, model=model, modelTable=modelTable)
		
	} else if (format == "rpart") {
		
		# prepare tree$frame$var
		var <- matrix("", nrow=nrow(nodes))
		#levels(var) <- columns$COLUMNNAME
		
		# compute tree$frame$n
		n <- as.integer(nodes$SIZE)
		
		# tree$frame$wt and tree$frame$dev are not given
		wt <- nodes$SIZE
		dev <- NA
		
		# Compute tree$frame$yval
		yval <- nodes$CLASS
		
		# tree$frame$complexity, tree$frame$ncompete and tree$frame$nsurrogate are not given
		complexity <- 0
		ncompete <- 0
		nsurrogate <- 0
		
		# compute tree$frame$yval2
		# sValue will be uses to compute tree$splits
		# compute dummy.xlevels in order to make tree print- and plottable
		yprob1 <- matrix(0, nrow=nrow(nodes), ncol=length(classes))
		yprob2 <- matrix(0, nrow=nrow(nodes), ncol=length(classes))
		yprob3 <- matrix(0, nrow=nrow(nodes))
		yprob4 <- matrix(0, nrow=nrow(nodes))
		sValue <- matrix(0, nrow=nrow(nodes))
		# dummy.xlevels will contain relevant levels from every column
		dummy.xlevels <- list()
		# discretes contains the numbers of rows that are discrete
		discretes <- NULL
		
		for (i in 1:nrow(nodes)) {  	
			j <- predicates$NODEID[i]
			if(!nodes$ISLEAF[predicates$NODEID == j]) {
				var[i] <- predicates$COLUMNNAME[predicates$NODEID == 2*j]
				sValue[i] <- predicates$VALUE[predicates$NODEID == 2*j]
				if (predicates$OPERATOR[predicates$NODEID == 2*j] == "equal") {
					if (is.null(discretes)) {
						discretes <- i
					} else {
						discretes <- c(discretes, i)
					}
					if (!is.null(dummy.xlevels[[as.character(var[i])]])) {
						if (!(sValue[i] %in% dummy.xlevels[[as.character(var[i])]])) {
							dummy.xlevels[[as.character(var[i])]] <- c(dummy.xlevels[[as.character(var[i])]], sValue[i])
						}
					} else {
						dummy.xlevels[[as.character(var[i])]] <- c("<other>", sValue[i])
					}
				}
			} else {
				var[i] <- "<leaf>"
			}
			
			for (k in 1:length(classes)) {
				yprob1[i,k] <- discrete[discrete$NODEID==j & discrete$VALUE==classes[k],"COUNT"]
				yprob2[i,k] <- discrete[discrete$NODEID==j & discrete$VALUE==classes[k],"RELFREQUENCY"]
			}
			yprob3[i] <- as.numeric(yval[i])
			yprob4[i] <- nodes$RELSIZE[i]
		}
		
		# if there are discrete variables, also create dummy.csplit in order to make tree print- and plottable
		if (!is.null(discretes)) {
			dummy.csplit <- matrix(2, nrow = length(discretes), ncol = max(sapply(dummy.xlevels, length)))
			for (l in 1:length(discretes)) {
				ll <- discretes[l]
				dummy.csplit[l,match(sValue[ll],dummy.xlevels[[var[ll]]])] <- 1
			}
			dummy.csplit[,1] <- 3
		} else {
			dummy.csplit <- NULL
		}
		
		var <- as.factor(var)		
		colnames(yprob4) <- "nodeprob"
		yval2 <- cbind(yprob3, yprob1, yprob2, yprob4)
		
		# create tree$frame
		frame <- data.frame(var = var, n = n, wt = wt, dev = dev, yval = yval, complexity = complexity, ncompete = ncompete, nsurrogate = nsurrogate, yval2 = I(yval2))
		rownames(frame) <- nodes$NODEID
		
		# put nodes into right order
		score <- (nodes$NODEID - 2^(floor(log(nodes$NODEID,2))  +0.5)) / 2^(floor(log(nodes$NODEID,2)+1))
		frame <- frame[order(score),]
		
		# compute tree$splits
		count <- nodes$SIZE
		ncat <- -1
		improve <- NA
		index <- suppressWarnings(as.numeric(sValue))
		adj <- NA
		
		splits <- cbind(count, ncat , improve, index, adj)
		rownames(splits) <- var
		
		## if tree is a small tree (i.e. root node is leaf), don't cut the leaves
		smallTree = sum(nodes$ISLEAF == FALSE) == 1
		if (!smallTree) {
			splits <- splits[!nodes$ISLEAF,]
			splits <- splits[order(score[!nodes$ISLEAF]),]
			dummy.csplit <- dummy.csplit[order(score[discretes]),]
		}
		
		
		# add rpart functions print, summary, text to tree
		dummy <- rpart:::rpart.class(0,NULL,NULL,0)
		functions = list()
		functions$print <- dummy$print
		functions$summary <- dummy$summary
		functions$text <- dummy$text
		
		# create tree and set rpart-specific attributes
		tree <- list(frame = frame, terms = terms, splits = splits, functions = functions, model=model, modelTable=modelTable)
		attr(tree, "dummy.xlevels") <- dummy.xlevels
		attr(tree, "dummy.csplit") <- dummy.csplit
	}
	
	# set attributes
	# format and dummies are required for proper printing / plotting
	attr(tree, "xlevels") <- xlevels
	attr(tree, "ylevels") <- classes
	attr(tree, "format") <- format
	
	# set class and return
	class(tree) <- c("nzDecTree")
	return(tree)
}

# ----------------------------------------------------------------------

# wrapper for tree plotting
plot.nzDecTree <- function(x, ...) {
	if (nrow(x$frame) == 1) {
		stop("Plotting trees with no splits is not supported.")
	}
	
	if (attr(x, "format") == "tree") {
		class(x) <- "tree"
		
		# this is a hack for `if (dev.cur() == 1L)` in tree::plot.tree
		if (as.integer(dev.cur()) == as.integer(1))
			dev.new()
		
		plot(x, type="uniform")
		text(x)
	} else if (attr(x, "format") == "rpart") {
		class(x) <- "rpart"
		
		# add dummies in order to make x printable
		attr(x, "xlevels") <- attr(x, "dummy.xlevels")
		x$csplit <- attr(x, "dummy.csplit")
		
		# adjust x$splits for discrete values
		# ncat <- 2 changes from continuous to discrete variable
		# index <- n selects row from x$csplit
		x$splits[is.na(x$splits[,"index"]),"ncat"] <- 2
		if (!is.null(x$csplit)) {
			if (!is.matrix(x$csplit)) {
				x$csplit <- t(as.matrix(x$csplit))
			}
			n.discrete <- nrow(x$csplit)
			x$splits[is.na(x$splits[,"index"]),"index"] <- 1:n.discrete
		}
		
		plot(x, uniform = T, ...)
		text(x)
	}
}

# ----------------------------------------------------------------------

# wrapper for tree printing

print.nzDecTree <- function(x, ...) {
	if (attr(x, "format") == "tree") {
		class(x) <- "tree"
		print(x)
	} else if (attr(x, "format") == "rpart") {
		class(x) <- "rpart"
		
		# add dummies in order to make x printable
		x$method <- "class"		
		attr(x, "xlevels") <- attr(x, "dummy.xlevels")
		x$csplit <- attr(x, "dummy.csplit")
		
		# adjust x$splits for discrete values
		# ncat <- 2 changes from continuous to discrete variable
		# index <- n selects row from x$csplit
		x$splits[is.na(x$splits[,"index"]),"ncat"] <- 2
		if (!is.null(x$csplit)) {
			if (!is.matrix(x$csplit)) {
				x$csplit <- t(as.matrix(x$csplit))
			}
			n.discrete <- nrow(x$csplit)
			x$splits[is.na(x$splits[,"index"]),"index"] <- 1:n.discrete
		}
		
		print(x, ...)
	}
}

# ----------------------------------------------------------------------

# wrapper for predict
predict.nzDecTree <- function(object, newdata, id="id", ...) {
	outtable <- nzGetValidTableName(paste(newdata@table,"_predict",sep=""))
	
	colu = newdata@cols
	if (!(id %in% colu))
		stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", id)))
	
	id  <- dQuoteSimple(id)
	
	tmpView <- nzCreateView(newdata)
	
	tryCatch({	
				callSP("nza..PREDICT_DECTREE ", model=object$model, intable=tmpView, id=id, outtable=outtable, ...)
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