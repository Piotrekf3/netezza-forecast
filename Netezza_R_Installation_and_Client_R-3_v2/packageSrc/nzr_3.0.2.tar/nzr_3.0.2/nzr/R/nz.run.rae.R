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

nz.run.rae.prepare.columns <-
function (x) {
  	if (ncol(x) > 63) {
  		cols   <- paste('inza..nzaemultiarg(',
  			lapply(suppressWarnings(split(paste('\"',x@cols,'\"',sep=""), gl(ceiling(length(x)/64),64))),
  					function(x)paste(x,collapse=',')),
  			')',collapse=',')
  	} else
  		cols <- paste(paste('\"',x@cols,'\"',sep=""), collapse=',')
  
	return(cols)
}


nz.run.rae.check.output <-
function (output.name, clear.existing) {
	# used when deciding on creating the output table
	outputExists <- FALSE

	# if the table exists we assume that this is an error ...
	if (!is.null(output.name) && nzExistTable(output.name)) {
		# ... unless the clear flag is set
		if (is.logical(clear.existing) && isTRUE(clear.existing))
			nzDeleteTable(output.name)
		else if (is.character(clear.existing) && identical(tolower(clear.existing), "truncate")) {
			nzTruncateTable(output.name)
			outputExists <- TRUE
		} else
			stop("the output table already exists and the clear flag is set to FALSE")
	}

	return(outputExists)
}

nz.run.rae <-
function (x, aedata, output.name=NULL, output.signature=NULL, clear.existing=FALSE, case.sensitive=FALSE, distributeOn=NULL,cappend=NULL) {

	#Check if we need to create a view to cast NUMERIC and DECIMAL
	viewName <- nzCreateSimplifiedView(x);
	
	if(!is.null(viewName)) {
		x <- nz.data.frame(viewName);
	} 
	
	tryCatch({
	
	`nz.run.rae.from` <- function(x, aename) {
		if (!is.null(x@table))
    		ff = paste(nzdf.qualified.name(x),"AS from_alias",sep=" ")        
		else
			ff = paste("(", nzdf.query(x), ") AS from_alias")

		datafile <- nz.placefile(data=rawToChar(serialize(aedata, NULL, ascii=TRUE)))
		return(paste(" FROM ", ff, ",TABLE WITH FINAL(", aename, "(",
				nz.run.rae.prepare.columns(x), ",CAST('WORKSPACE_PATH=", datafile,
				"' AS VARCHAR(256)))) AS ae_output_t", ifelse(nchar(x@where)>0,paste(" WHERE ",x@where,sep=""),""), sep=""))
	}

	# if returns without exception we may proceed further
	outputExists <- nz.run.rae.check.output(output.name, clear.existing)

	# appending input columns
	if (!is.null(cappend)) {
		if (length(setdiff(seq(length(x@cols)), cappend)))
			stop("cappend columns incorrect")

		cappdef <- paste(",", nzTableDef(x[,cappend]))
		cappend <- paste(",",  paste('\"',x@cols[cappend],'\"',sep=""), collapse="")
	} else {
		cappdef <- ""
		cappend <- ""
	}

	if (!is.null(output.signature)) {
		if (is.nz.data.frame(output.signature)) {
			output.signature <- nzSimpleSignature(output.signature)
		} else if (!is.list(output.signature)) {
			stop('output signature has to be a list of form (field.name=NZ.DATA.TYPE, ...)')
		}
		
		if(case.sensitive) {
			output.signature2 <- output.signature
			names(output.signature2) <- paste("\"",names(output.signature),"\"", sep='')
			aedata$shaper.list <- output.signature2;
		}
		
		aedata$shaper.list <- output.signature
			
		aedata$shaper <- 'std'
		aename <- 'nzr..r_udtf_any'
	}
	else {
		aename <- 'nzr..r_udtf'
	}

	# just return a regular data.frame
	if (is.null(output.name)) {
		ans <- nzQuery("SELECT ae_output_t.*", cappend, nz.run.rae.from(x, aename))
		#ans is whatever user provided regardless of upper or lowercase
		# additional type casting: BIGINT & NUMERIC -> numeric

	
		
		#	if (case.sensitive){#convert the column names to the proper case
		#		if (!is.null(output.signature)) names(ans) <- names(output.signature)
		#	}
		#	else {
		#		if(nzIsLower()) 
		#			newnames <- tolower(names(ans))
		#		else 
		#			newnames <- toupper(names(ans))
		#		
		#		names(ans) <- newnames
		#	
		#	}
			for (name in names(output.signature)) {
				type <- output.signature[[name]]
				if (type == NZ.INT64 || type  == NZ.NUMERIC128 || type  == NZ.NUMERIC64 || type  == NZ.NUMERIC32) {
					
					ans[[name]] <- as.numeric(ans[[name]])
				}
			}
		
		return(ans)
	}

	# CREATE A GENERIC TABLE AND RETURN NZ.DATA.FRAME
	else {
		if (is.null(output.signature)) {
			if (nchar(cappend))
				stop("cappend may not be used with generic output type")

			if (!outputExists)
				nzQuery("CREATE TABLE ", output.name, " AS SELECT ", paste("from_alias.rowid AS rowid_orig, ae_output_t.* ", nz.run.rae.from(x, 'nzr..r_udtf')),ifelse(!is.null(distributeOn),paste(" distribute on (",distributeOn,")",sep=""),""))
			else
				nzQuery("INSERT INTO ", output.name, " SELECT ", paste("from_alias.rowid AS rowid_orig, ae_output_t.* ", nz.run.rae.from(x, 'nzr..r_udtf')))
		} else {
			if (!outputExists)
				nzQuery("CREATE TABLE ", output.name, " AS SELECT ae_output_t.* ", cappend, nz.run.rae.from(x, 'nzr..r_udtf_any'),ifelse(!is.null(distributeOn),paste(" distribute on (",distributeOn,")",sep=""),""))
			else
				nzQuery("INSERT INTO ", output.name, " SELECT ae_output_t.* ", cappend, nz.run.rae.from(x, 'nzr..r_udtf_any'))
		}
		
		return(nz.data.frame(output.name))
	}
	
	}, error = function(e, tmpView) {
				stop(e)
			}, finally = {
				if(!is.null(viewName)) {
					nzDropView(viewName)
				}
			}
	)
}

