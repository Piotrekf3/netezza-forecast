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

nzContingencyTable <- function(form, nzdf, makeMatrix=T) {
	ntab1 <- nzParseRFormula(form, nzdf)
	
	tmpView <- nzCreateView(nzdf)
	
	tryCatch({	
				outtable <- nzGetValidTableName(paste0(nzdf@table, "_model"))
				xml <- nzPrepareXML4ContTable(varlist=ntab1$varlist, intable=tmpView, outtable=outtable)
				res  <- nzCallXMProcedure("ctable", xml)
			}, error = function(e) {
				# in case of error, let user know what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(tmpView)
			}
	)
	
	otab <- nz.data.frame(outtable)
	xml <- as.character(res[1,1])
	
	mat <- NULL
	if (makeMatrix) {
		mat <- as.table(nzSparse2matrix(otab))
	}
	
	list(xml=xml, tab=otab, mat=mat)
}

#------------------------------------------------------------------------------

nzTable <- nzContingencyTable

#------------------------------------------------------------------------------

nzChisq.test <- function(form, nzdf, ...) {
	out <- nzContingencyTable(form, nzdf, makeMatrix = T)
	stats::chisq.test(out$mat, ...)
}

#------------------------------------------------------------------------------

nzMantelHenszel.test <- function(form, nzdf, ...) {
	out <- nzContingencyTable(form, nzdf, makeMatrix = T)
	stats::mantelhaen.test(out$mat, ...)
}

#------------------------------------------------------------------------------

nzMantelhaen.test <- nzMantelHenszel.test

#------------------------------------------------------------------------------

#nzKendall()
#out = nzContingencyTable(~factor(V1)+factor(V2), mat2, makeMatrix = T)

#------------------------------------------------------------------------------

nzGoodman <- function(form, nzdf, ...) {
	out <- nzContingencyTable(form, nzdf, makeMatrix = T)
	mmm <- out$mat
	nazC <- as.numeric(colnames(mmm))
	nazR <- as.numeric(rownames(mmm))
	sig <- kronecker(t(nazC), nazR, function(x, y) 1 - 2*(x<y))
	sum(sig*mmm)/sum(mmm)
}

#------------------------------------------------------------------------------

