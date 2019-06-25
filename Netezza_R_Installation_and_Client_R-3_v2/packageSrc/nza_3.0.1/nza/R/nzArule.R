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

`nzArule` <- function(
		data,
		tid  = "tid",
		item = "item",
		by = NULL,
		lvl = 1,
		maxsetsize = 6,
		support = 5,
		supporttype = "percent",
		confidence = 0.5,
		model = NULL,
		format = "arule",
		dropAfter = F,
		...) {
	
	if (all(is.null(data), is.null(model))) {
		stop("Plase specify either data or model.")
	}
	
	# switch to retrieve mode, if necessary
	if (all(is.null(data), !is.null(model))) {
		if (format == "arule") {
			result <- nzArule.format.arules(data, model, support, confidence)
		} else if (format == "raw") {
			result <- nzArule.format.raw(model)
		}
		return(result)
	} else {
		# auto generate modelname
		if (is.null(model)) {
			model <- nzGetValidModelName(paste(data@table,"_model",sep=""))
		} else if (modelExist(model)) {
			stop("Model name already exists.")
		}
		
		# create view from given data argument
		tmpView <- nzCreateView(data)
		
		tryCatch({		
					# validate given id and item argument
					colu = data@cols
					if (!(tid %in% colu)) {
						stop(simpleError(paste("Id variable is not avaliable in nz.data.frame:", tid)))
					}
					if (!(item %in% colu)) {
						stop(simpleError(paste("Item variable is not avaliable in nz.data.frame:", item)))
					}
					tid <- paste('\"', tid, '\"', sep="")
					item <- paste('\"', item, '\"', sep="")
					
					# call stored procedure
					callSP("nza..ARULE ",
							model=model,
							intable=tmpView,
							tid=tid,
							item=item,
							by=by,
							lvl=lvl,
							maxsetsize=maxsetsize,
							support=support,
							supporttype=supporttype,
							confidence=confidence,
							...)
				}, error = function(e) {
					# in case of error, let user know what happend
					stop(e)
				}, finally = {
					# drop view
					nzDropView(tmpView)
				}
		)
		
		if (format == "arule") {
			result <- nzArule.format.arules(data, model, support, confidence)
		} else if (format == "raw") {
			result <- nzArule.format.raw(model)
		}
		
		return(result)
	}
	
	if (dropAfter) {
		modelDrop(model)
	}
}

#------------------------------------------------------------------------------

nzArule.format.arules <- function(data, model, support, confidence) {
	# load results from DB
	item <- paste("INZA.NZA_META_",model,"_ITEM",sep="")
	itemset <- paste("INZA.NZA_META_",model,"_ITEMSET",sep="")
	rule <- paste("INZA.NZA_META_", model, "_RULE",sep="")
	
	item.out <- nzr::as.data.frame(nz.data.frame(item))
	itemset.out <- nzr::as.data.frame(nz.data.frame(itemset))
	rule.out <- nzr::as.data.frame(nz.data.frame(rule))
	
	# how many itemsets are there?
	n.sets <- nrow(itemset.out)
	
	# itemsets are saved in sparse matrices, i. e. for each itemset
	# there is one row, containing one column for each item, if this item
	# is inside the itemset by true or false
	# need to convert itemids into position ids in order to create the sparse matrices
	# position[ ,1] contains the itemset id
	# position[ ,2] contains the item id
	position <- matrix(NA, nrow=0, ncol=2)
	
	for (i in 1:n.sets) {
		j <- 1
		repeat {
			colname <- paste0("ITEM",j)
			item <- itemset.out[i,colname]
			if (is.null(item)) {
				break
			} else if (is.na(item)) {
				break
			} else {
				position <- rbind(position, c(i, itemset.out[i,colname]))
				j <- j+1
			}
		}
	}
	
# create sparse matrix with all itemsets
	sets <- sparseMatrix(position[,1],position[,2])
# load itemnames (only required ones 1:ncol(sets))
	sets.labels <- item.out[1:ncol(sets),2]
# for arules compatibility, make sure labels are in 'AsIs' character format
	sets.labels <- I(as.character(sets.labels))
	
# create quality table
	quality <- rule.out[ ,c("SUPPORT", "CONFIDENCE", "LIFT")]
# for arules compatibility, columnnames must be lowercase
	names(quality) <- tolower(names(quality))
	
# compose info data.frame (inculdes comptibility for data=NULL in retrieve mode)
	if(is.null(data)) {
		info <- data.frame(data="RETREIVED FROM EXISTING MODEL", NA, support=support, confidence=confidence, model=I(model))
	} else {
		info <- data.frame(data=I(capture.output(print(data))), ntransactions=nrow(data), support=support, confidence=confidence, model=I(model))
	}
	
# get left and right hand sides from sets
# NOTE: calling as-Matrix again is necessary when there is only one rule
	if (nrow(rule.out)==1) {
		lhs.data <- as(Matrix(sets[rule.out[ ,"LHS_SID"], ]),"nsparseMatrix")
		rhs.data <- as(Matrix(sets[rule.out[ ,"RHS_SID"], ]),"nsparseMatrix")
	} else {
		lhs.data <- Matrix::t(sets[rule.out[ ,"LHS_SID"], ])
		rhs.data <- Matrix::t(sets[rule.out[ ,"RHS_SID"], ])
	}
# create lhs and rhs as "itemMatrix" objects, transpose lhs and rhs for compatibility
	lhs <- new("itemMatrix", data=lhs.data, itemInfo=data.frame(labels=sets.labels))
	rhs <- new("itemMatrix", data=rhs.data, itemInfo=data.frame(labels=sets.labels))
	
# create "rules" object
	rules <- new("rules", lhs=lhs, rhs=rhs, quality=quality, info=info)
	
	return(rules)
}

#------------------------------------------------------------------------------

nzArule.format.raw <- function(model) {
	# load results from DB
	group <- paste("INZA.NZA_META_", model, "_GROUP",sep="")
	item <- paste("INZA.NZA_META_",model,"_ITEM",sep="")
	itemset <- paste("INZA.NZA_META_",model,"_ITEMSET",sep="")
	rule <- paste("INZA.NZA_META_", model, "_RULE",sep="")
	
	group.out <- nzr::as.data.frame(nz.data.frame(group))
	item.out <- nzr::as.data.frame(nz.data.frame(item))
	itemset.out <- nzr::as.data.frame(nz.data.frame(itemset))
	rule.out <- nzr::as.data.frame(nz.data.frame(rule))
	
	# create raw-output as list
	raw <- list(group=group.out, item=item.out, itemset=itemset.out, rule=rule.out, model=model)
	return(raw)
}

#------------------------------------------------------------------------------

plot.rules <- function(...) {
	if (!("package:arulesViz" %in% search())) {
		stop("\n \t For printing and plotting association rules,\n \t you need to install and load the package arulesViz first.")
	}
	
	arulesViz:::plot.rules(...)
}
