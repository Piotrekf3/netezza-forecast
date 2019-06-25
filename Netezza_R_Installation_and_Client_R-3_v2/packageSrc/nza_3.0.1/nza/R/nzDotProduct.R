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

getVariableDesc <- function(node) {
	vsize = ifelse(xmlAttrs(node)[1] == "factor", 
			as.numeric(xmlAttrs(node)[2]), 1)
	vname = ifelse(xmlAttrs(node)[1] == "factor", 
			{ #nz.data.frame(xmlValue(node[[2]]), case.sensitive=TRUE)
				#strip quotation before creating nz.data.frame:
				dict_table = xmlValue(node[[2]])
				if(nchar(dict_table) > 2 && substr(dict_table, 1, 1) == "\"" && substr(dict_table, nchar(dict_table), nchar(dict_table)) == "\"") 
					dict_table <- substr(dict_table, 2, nchar(dict_table) - 1)
				tab = nzr::as.data.frame(nz.data.frame(dQuoteSimple(dict_table)))
				tab = tab[order(tab[,4]),] # 4th should be "VALUE_ID"
				list(paste(tab[,2],".",tab[,3],sep="")) }, # 2nd should be "VAR_NAME" while 3rd "VALUE"
			list(xmlValue(node[[1]])))
	list(size=vsize, names=vname)
}

#------------------------------------------------------------------------------

nzDotProduct <- function(form, nzdf, makeMatrix=F, weights=-1) {
	mat = NULL
	
	intable <- nzCreateView(nzdf)
	tryCatch({	
				outtable <- nzGetValidTableName(paste0(nzdf@table, "_model"))
				
				tmp = nzPrepareXML4DotProduct(form, nzdf, intable=intable, outtable=outtable, weights=weights)
				xml = tmp[[1]]
				modeldesc = tmp[[2]]
				
				res  = nzCallXMProcedure("DotProduct", xml)
			}, error = function(e) {
				# in case of error, let user know what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(intable)
			}
	)
	
	otab = nz.data.frame(outtable)
	xml  = as.character(res[1,1])
	size1 = 0
	size2 = 0
	
	if (makeMatrix) {
		mat = nzSparse2matrix(otab, with.names = FALSE)
		xmlTree = xmlRoot(xmlTreeParse(res[[1]], asText=TRUE))
		
		elements = names(xmlTree)
		
		#
		# first order only
		ind1 = which(elements=="variable")
		tmp = lapply(ind1, function(x) getVariableDesc(xmlTree[[x]]))
		
		size1 = sapply(tmp, function(x) x[[1]])
		names1 = unlist(lapply(tmp, function(x) x[[2]]))
		
		#
		# second order only
		ind2 = which(elements=="interaction")
		tmp = lapply(ind2, function(x) {
					var1 = getVariableDesc(xmlTree[[x]][[1]])
					var2 = getVariableDesc(xmlTree[[x]][[2]])
					list(size = var1$size * var2$size, 
							names = outer(var1$names[[1]],var2$names[[1]], FUN=paste,sep=":"))
				})
		size2 = sapply(tmp, function(x) x[[1]])
		names2 = unlist(lapply(tmp, function(x) x[[2]]))
		
		nameslist = c(names1, names2)
		if ("intercept" %in% elements) 
			nameslist = c(nameslist, "Intercept")
		
		nameslist = gsub('"', '', nameslist)
		
		rownames(mat) = nameslist
		colnames(mat) = nameslist
		
#    nlist = strsplit(strsplit(res[[1]], "[;,]")[[1]], "-")
#    nlist2 = sapply(nlist, function(x) {
#     if (length(x)==0) return(NULL)
#     if (length(x)==1) 
#        if (as.numeric(x[1])==1) return("Intercept") else return(NULL)
#     if (as.numeric(x[2])<2) return(x[1])
#     return(paste(x[1], 1:as.numeric(x[2]), sep="."))
#    } )
#    namesM = unlist(nlist2)
#    rownames(mat) = namesM
#    colnames(mat) = namesM
	}
	list(xml = xml, tab = otab, mat = mat, varnames=modeldesc$varlist, size1 = size1, size2 = size2)
}

# if standarisation will be nedded
#  if (standarize) {
#    varlist = paste(varlist, " - avg(", varlist, ") over () ", sep="")
#  }

#------------------------------------------------------------------------------..

nzPCA <- function(nzdf, scale=FALSE) {
	if (!is.nz.data.frame(nzdf)) 
		stop(simpleError("nzdf must be an object of nz.data.frame class"))
	
	varlist = names(nzdf)
	intable = nzCreateView(nzdf)
	outtable = nzGetValidTableName(paste0(nzdf@table, "_model"))
	
	xml = nzPrepareXML4DotProductsimple(varlist, scaled=rep(scale,length(varlist)), centered=rep(T,length(varlist)), intable = intable, outtable = outtable)
	res  = nzCallXMProcedure("DotProduct", xml)
	
	otab = nz.data.frame(outtable)
	xml  = as.character(res[1,1])
	
	# covariance matrix
	mat = nzSparse2matrix(otab, with.names = FALSE)
	
	# eigen value decomposition
	eig = eigen(mat)
	
	loadings = eig$vec 
	sdev     = eig$val
	names(sdev) = paste("Comp.",1:length(sdev),sep="")
	colnames(loadings) = paste("Comp.",1:length(sdev),sep="")
	rownames(loadings) = varlist 
	
	out = list(loadings=loadings, sdev=sdev, n.obs=dim(nzdf)[1], call=match.call())
	class(out) = "princomp"
	out
}

