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
# 

nz.tapply.prepare.columns <-
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

nzTApply <- function (X, INDEX, FUN = NULL, output.name=NULL, output.signature=NULL,
		clear.existing=FALSE, case.sensitive=FALSE,output.distributeOn=NULL,debugger.mode = FALSE, ..., simplify = TRUE) {
	
	if (!is.nz.data.frame(X))
		stop("this function can only be run for nz.data.frame")
		
	#If functions are passed as arguments, make sure to remove the name space		
	args <- list(...);
	args <- lapply(args,
			function(arg) {
				if (is.function(arg)) {
					environment(arg) <- globalenv()
					return(arg)
				} else {
					return(arg)
				}
			}
	);	
	
	
	
	#	we are distinguishing columns with the row_number value
	#	cols <- X@cols[X@cols != INDEX]
	cols <- X@cols
	#paste('\"',x@cols,'\"',sep="")
	# set function's environment
	#environment(FUN) <- emptyenv() # is it really necessary?
	FUNcall <- deparse(substitute(FUN))
	if ("standardGeneric" %in% class(FUN)) {
		FUN <- get(FUNcall, env=baseenv(), mode="function")
		environment(FUN) <- baseenv()
	} 
	environment(FUN) <- globalenv();
	
	if (debugger.mode == FALSE) { 
		toserialize <- list(fun=FUN, args=args, cols=cols, mode='tapply')
		
	} else { # debugger mode
		output.name=NULL
		output.signature=NULL
		FUN2 <<- function(..., FUN2s) {
			newPath = paste(getFilePath(), "Rdebug", sep="")
			dir.create(newPath, showWarnings = FALSE)
			nz.file2dump <- paste(newPath, "/", round(runif(1)*10^12), ".dump", sep="")
			
			FUNoutput = tryCatch(FUN2s(...), error = function(e) {
						calls <- sys.calls()
						last.dump <- sys.frames()
						names(last.dump) <- limitedLabels(calls)
						last.dump <- last.dump[-length(last.dump)]
						attr(last.dump, "error.message") <- geterrmessage()
						class(last.dump) <- "dump.frames"
						string = rawToChar(serialize(last.dump, NULL, ascii=T), multiple=F)
						
						writeChar(string, nz.file2dump)
						invisible(structure(conditionMessage(e), class = "try-error"))
					})
			
			list(content = paste(FUNoutput, sep=",", collapse="; "), is.error = class(FUNoutput), error.path = nz.file2dump)
		}
		environment(FUN2) <- parent.env(parent.env(parent.env(environment(FUN2))))
		
		toserialize <- list(fun=FUN2, args=list(args, FUN2s=FUN), cols=cols, mode='tapply')
	}  # END debugger mode
	
	if (exists("p_nzLibrary") && !is.null(p_nzLibrary))
		toserialize$nzlibrary <- p_nzLibrary
	
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
	
	if (!is.null(output.signature)) {
		
		if (is.nz.data.frame(output.signature))
			output.signature <- nzSimpleSignature(output.signature)
		else if (!is.list(output.signature))
			stop('output signature has to be a list of form (field.name=NZ.DATA.TYPE, ...)')
		
		if(case.sensitive)
			names(output.signature) <- paste("\"",names(output.signature),"\"", sep='')
		#TODO: Add else that sets columns to default case
		
		toserialize$shaper <- 'std'
		toserialize$shaper.list <- output.signature
		aename <- 'nzr..r_udtf_any'
	}
	else {
		aename <- 'nzr..r_udtf'
	}				
	
	datafile <- nz.placefile (data = rawToChar(serialize(toserialize,NULL,ascii=TRUE)))
	
	
	# determine the grouping column
	#II <- substitute(INDEX, environment())
	#if (!exists(as.character(II)) && is.symbol(II))
	#	INDEX <- as.character(II)
	
	if (is.nz.data.frame(INDEX)) {
		if (length(INDEX@cols) != 1)
			stop("only one column can be specified as INDEX")
		INDEX = INDEX@cols
	}
	else if (is.integer(INDEX) || (is.double(INDEX) && floor(INDEX) == INDEX)) {
		if (INDEX > length(X@cols))
			stop("input data frame has only ", length(X@cols), " columns")
		INDEX = X@cols[INDEX]
	}
	else if (!is.character(INDEX))
		stop("INDEX value cannot be determined with value of type ", typeof(INDEX))
	
	
	# check whether this columns exists in X
	if (!is.element(INDEX, listTableColumns(nzdf.qualified.name(X))))
		stop("input data frame does not have column named ", INDEX)
	
	# add the INDEX column...
	if (!is.element(INDEX, X@cols)) {
		X@cols <- c(X@cols, INDEX)
		warning("INDEX column available in table but not available in nz.data.frame")
	}
	xcols <- paste('\"',X@cols,'\"',sep="")
	INDEX <- paste('\"',INDEX,'\"',sep="")
	
	# a function has to be supplied
	if (is.null(FUN) || !is.function(FUN))
		stop("FUN has to be a function")
	
	viewName <- nzCreateSimplifiedView(X);
	
	if(!is.null(viewName)) {
		X <- nz.data.frame(viewName);
	} 
	
	tryCatch({
				
	# just return a regular data.frame
	if (is.null(output.name) || debugger.mode) {
		r <- nzQuery("SELECT ae_output_t.* FROM ",
				"(SELECT row_number() OVER(PARTITION BY ", INDEX, " ORDER BY ", INDEX, ") AS nzrn,",
				" count(*) OVER (PARTITION BY ", INDEX, ") AS nzcnt,",
				" from_alias.* FROM (", nzdf.query(X), ") AS from_alias)",
				" AS outer_from, TABLE WITH FINAL (", aename ,"(", nz.tapply.prepare.columns(X)
				, ",", INDEX, ", nzrn, nzcnt, 'WORKSPACE_PATH=", datafile, "')) AS ae_output_t ")
		if (!is.null(output.signature)) {
			return(r)
		}
		else { # no output signature given, transform and return a regular data.frame
			ret <- nzt(r)
			if (ncol(ret) == length(cols)+1)
				names(ret) <- c(cols, INDEX)
			
			# debugger mode	
			if (debugger.mode) {
				names(ret) = c("values","type","file","group")
				noerrors = sum(ret[,2]=="try-error")
				cat("\n\n Found ",noerrors," error",ifelse(noerrors==1,"","s")," \n\n ",sep="")
				print(ret[,-3])
				retobj <- ret[,-3]
				
				if (noerrors>0) {
					noerr = which(ret[,2]=="try-error")[1]
					cat("\n\n Recalling environment for group ",ret[noerr,4]," \n\n Take environment no. 11 and check for the args variable \n\n ",sep="")
					#note the cast to varchar(16001); this is due to the bug in RODBC, which drops 256th character (!) when querying for string data; the solution is to cast to varchar (_ctf_utl_readFile returns nvarchar) and enhance
					#it by 1 - crazy but works; note also, that casting to varchar will cause problems when system.frames() will dump any national characters ...  
					qres <- nzQuery(paste("select chunk::varchar(16001) from table with final(nza.._ctf_utl_readFile('",ret[noerr, 3], "'));", sep = "", collapse = ""))                    
					retobj = unserialize(charToRaw(paste(qres[,1],sep = "", collapse = "")))
					debugger(retobj)
					return(invisible(retobj))
				}
			}
			return(ret)
		}
	}
	
	# CREATE A GENERIC TABLE AND RETURN NZ.DATA.FRAME
	else {
		if (is.null(output.signature)) {
			stop("output.signature must be given when output.name is set")		
		}
		else {
			r <- nzQuery("CREATE TABLE ",output.name," as (SELECT ae_output_t.* FROM ",
					"(SELECT row_number() OVER(PARTITION BY ", INDEX, " ORDER BY ", INDEX, ") AS nzrn,",
					" count(*) OVER (PARTITION BY ", INDEX, ") AS nzcnt,",
					"from_alias.* FROM (", nzdf.query(X), ") AS from_alias)",
					" AS outer_from, TABLE WITH FINAL (nzr..r_udtf_any(",
					nz.tapply.prepare.columns(X), ",", INDEX, ", nzrn, nzcnt, 'WORKSPACE_PATH=", datafile, "')) AS ae_output_t)",ifelse(!is.null(output.distributeOn),paste("distribute on (",output.distributeOn,")",sep=""),""));
			
			return(nz.data.frame(output.name))		
		}
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

setMethod("tapply", signature(X="nz.data.frame", INDEX="ANY"), nzTApply)

