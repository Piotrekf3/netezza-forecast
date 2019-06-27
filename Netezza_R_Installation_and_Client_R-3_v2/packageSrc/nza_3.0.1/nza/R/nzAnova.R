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
nzAnova <- function(form, data, ...) {
	ntab1 <- nzParseRFormula(form, data)
	tmpView <- nzCreateView(data)
	
	obj <- data.frame(matrix(NA,length(ntab1$varlist),5))
	colnames(obj) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
	rownames(obj) <- c(ntab1$varlist[-1],"Residuals")
	
	ntab1$varlist <- dQuoteSimple(ntab1$varlist)
	
	if (length(ntab1$varlist)==2) { #one way
		tryCatch({
					res <- callSP("nza..ANOVA_CRD_TEST",
							intable=tmpView,
							incolumn=ntab1$varlist[1],
							treatment=ntab1$varlist[2], 
							...	)[1,1]
				}, error = function(e) {
					# in case of error, let user know what happend
					stop(e)
				}, finally = {
					# drop view
					nzDropView(tmpView)
				}
		)
		
		tmp <- strsplit(res," ")[[1]]
		
		obj[2,1] <- as.numeric(tmp[8])
		obj[2,2] <- as.numeric(tmp[6])
		obj[2,3] <- obj[2,2] / obj[2,1]
		
		obj[1,1] <- as.numeric(tmp[4])
		obj[1,2] <- as.numeric(tmp[2])
		obj[1,3] <- obj[1,2] / obj[1,1]
		obj[1,4] <- as.numeric(tmp[10])
		obj[1,5] <- 1 - as.numeric(tmp[12])
	} else if (length(ntab1$varlist)==3) { #two way
		tryCatch({
					res = callSP("nza..ANOVA_RBD_TEST", 
							intable=tmpView, 
							incolumn=ntab1$varlist[1],
							treatment=ntab1$varlist[2],
							block=ntab1$varlist[3],
							...	)[1,1]
				}, error = function(e) {
					# in case of error, let user know what happend
					stop(e)
				}, finally = {
					# drop view
					nzDropView(tmpView)
				}
		)
		tmp = strsplit(res,"[ \\\\]")[[1]]
		
		obj[2,1] <- as.numeric(tmp[4])
		obj[2,2] <- as.numeric(tmp[2])
		obj[2,3] <- obj[2,2] / obj[2,1]
		obj[2,4] <- as.numeric(tmp[10])
		obj[2,5] <- 1 - as.numeric(tmp[12])
		
		obj[1,1] <- as.numeric(tmp[16])
		obj[1,2] <- as.numeric(tmp[14])
		obj[1,3] <- obj[1,2] / obj[1,1]
		obj[1,4] <- as.numeric(tmp[18])
		obj[1,5] <- 1 - as.numeric(tmp[20])
		
		obj[3,1] <- as.numeric(tmp[8])
		obj[3,2] <- as.numeric(tmp[6])
		obj[3,3] <- obj[3,2] / obj[3,1]
	} else { 
		cat("Only one way and two-ways ANOVA is supported\n")
		return(invisible())
	}
	
	class(obj) <- c("anova","data.frame")
	resO <- list(obj)
	class(resO) <- "summary.aov"
	resO
}
