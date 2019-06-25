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

# nz.data.frame implementation
# R & SQL relational & logical operators; used to overload
# nz.data.frames operators

relop <- data.frame(op=c("<",">", "<=", ">=", "!=", "=="),
					sql=c("","","","","<>", "="))

logop <- data.frame(op=c("&", "|"), sql=c("AND", "OR"))

# ---------------------------------------------------------------------

nz.data.frame <- function (table) {
	
	if(!nzExistTable(table)) {
		stop("Table ", table, " does not exist");
	}
	
	tableRef <- parseTableName(table);
	
	cols <- listTableColumns(table)
	
	for (i in 1:length(cols)) 
		if (nchar(cols[i]) > 2 && substr(cols[i], 1, 1) == '"' && substr(cols[i], nchar(cols[i]), nchar(cols[i])) == '"') {
			
			#remove surrounding \"
			cols[i] <- substr(cols[i],2,nchar(cols[i])-1)
		}
	
	if(!is.null(tableRef$schema))
		return(new(Class="nz.data.frame", table=tableRef$table  , schema=tableRef$schema,where="", cols=cols,
						case.sensitive.table=tableRef$case.sensitive.table,case.sensitive.schema=tableRef$case.sensitive.schema))
	else
		return(new(Class="nz.data.frame", table=tableRef$table,schema='', where="", cols=cols,
						case.sensitive.table=tableRef$case.sensitive.table,case.sensitive.schema=tableRef$case.sensitive.schema))
}

nzdf.from.where <- function(nzdf) {
	
	ans <- nzdf.from(nzdf);
	
	if(nchar(nzdf@where)>0) {
		ans <- paste(ans, "WHERE", nzdf@where,sep=' ')	
	} 

	return(ans)	
}

nzdf.qualified.name <- function (nzdf) {
	
	if(nchar(nzdf@schema)>0) {
		if(nzdf@case.sensitive.schema)
			ans <- paste('"',nzdf@schema,'".',sep='')
		else
			ans <- paste(nzdf@schema,'.',sep='')	
	} else
		ans <- ''
	
	
	if(nzdf@case.sensitive.table)
		ans <- paste(ans,'"',nzdf@table,'"',sep='')
	else
		ans <- paste(ans,nzdf@table,sep='')		
	
	return(ans)
}

# ---------------------------------------------------------------------

is.nz.data.frame <-
function(x) {
	return(inherits(x, "nz.data.frame"))
}


# ---------------------------------------------------------------------

setMethod("as.data.frame", signature(x="nz.data.frame"), 
	function (x, row.names=NULL, optional=FALSE, max.rows=NULL, order.by=TRUE, ...) {

		ans <- materialize(x, max.rows=max.rows, order.by=order.by)
		
			columns <- nzTableDef(x, collapse=FALSE)
			for (name in names(ans)) {
				type <- columns$type[columns$name == name]
				if (toupper(substr(type, 1, 6)) == 'BIGINT' || toupper(substr(type, 1, 7)) == 'NUMERIC')
					ans[[name]] <- as.numeric(ans[[name]])
			}
		

		return(ans)
	}
)

# ---------------------------------------------------------------------

setGeneric("as.nz.data.frame",
	useAsDefault = function (x, table = NULL, distributeon = NULL, clear.existing=FALSE, fast=TRUE, port=12345,case.sensitive=FALSE) {
		stop("casting from data type '", class(x), "' is not implemented yet")
	}
)

# ---------------------------------------------------------------------
setMethod("as.nz.data.frame", signature(x="data.frame"),
	function (x, table = NULL, distributeon = NULL, clear.existing=FALSE, fast=TRUE) {
		spec <- lapply(as.list(x), function(xx) getNPSDataType(xx))
		tmpnames <- gsub("\\.\\.", "%%", names(spec))
		tmpnames <- gsub("\\.", "_", tmpnames)
		cnames <- gsub("%%", "\\.\\.", tmpnames)

		# some column names are not allowed
		forbidden <- c("rows","cols")
		if (length(intersect(forbidden, cnames)) > 0)
			stop("following column names are not allowed: ", paste(forbidden, collapse=","))

		# determine the table name
		if (!is.null(table)) {
			
			name <- table;
			
			if (nzExistTable(name)) {
				if (clear.existing)
					nzDeleteTable(name)
				else
					stop(paste('table', name, 'already exists'))
			}
		} else {
			# named after the original data.frame?
			name <- as.character(substitute(x, environment()))
			# does it contain forbidden characters?
			if (regexpr('[^a-zA-Z0-9_]', name)[1] > -1)
				name <- nzGetValidTableName()
			else if (nzExistTable(name)) {
				if (clear.existing)
					nzDeleteTable(name)
				else
					name <- nzGetValidTableName()
			}
	
		}
		
		tabname <- name
		
		# convert factors to strings
		for (i in seq(ncol(x)))
			if (is.factor(x[,i]))
				x[,i] = levels(x[,i])[x[,i]]

		# distribute on
		DO <- substitute(distributeon, environment())
		if (!is.null(DO) && !exists(as.character(DO)) && is.symbol(DO))
			distributeon <- as.character(DO)

		if (!is.null(distributeon)) {
			# specified by index
			if (is.integer(distributeon)) {
				if (distributeon > length(x))
					stop("distributeon index out of bounds")
				distributeon <- names(x)[distributeon]
			}
			# specified by name
			else if (is.character(distributeon)) {
				if (!is.element(tolower(distributeon), tolower(names(x))))
					stop("specified data.frame does not have column named ", distributeon)
			}
			else
				stop("cannot determine the 'distributeon' value")
		}
		
		cnames <- paste("\"",cnames,"\"", sep='')
		
		if (fast){
		
				tmpFileNameUpload <- file.path(tempdir(),"dataRUpload.csv");
				while (file.exists(tmpFileNameUpload)) {
					tmpFileNameUpload <- file.path(tempdir(),paste("dataRUpload.csv",floor(runif(1,0,100000)),".csv",sep=""));
				}
		
				write.table(x, file=tmpFileNameUpload,row.names=FALSE, quote=F,col.names=FALSE, sep=",")

				sqlCommandUpload <- paste("CREATE TABLE ", tabname, " AS SELECT * from external '",tmpFileNameUpload,"' (", paste(cnames, as.character(spec), collapse=","), ") USING (DELIM ',' REMOTESOURCE 'ODBC')",ifelse(is.null(distributeon), "", paste(' DISTRIBUTE ON ("', distributeon, '")')),sep='');
				#print(sqlCommandUpload);

				tryCatch({	
					nzQuery(sqlCommandUpload);
				}, finally= {if(file.exists(tmpFileNameUpload)) file.remove(tmpFileNameUpload)});
		} else {
		
				nzQuery("CREATE TABLE ", tabname , " (", paste(cnames, as.character(spec), collapse=","), ")",
				ifelse(is.null(distributeon), "", paste(" DISTRIBUTE ON (", distributeon, ")")))
		
			# insert the data
			for (row in sequence(nrow(x))) {
				nzQuery("INSERT INTO ", tabname, "(", paste(cnames, collapse=","),
						") VALUES ('", paste(x[row,], collapse="','"),"')")
			}
		}	
	
		return(nz.data.frame(tabname));		
	}
)

# ---------------------------------------------------------------------
as.nz.data.frame.vector <- function (x, table = NULL, distributeon = NULL) {
	y <- as.data.frame(x)
	colnames(y) <- 'value'
	return(as.nz.data.frame(y, table, distributeon))
}

setMethod("as.nz.data.frame", signature(x="numeric"), as.nz.data.frame.vector)
setMethod("as.nz.data.frame", signature(x="character"), as.nz.data.frame.vector)
setMethod("as.nz.data.frame", signature(x="logical"), as.nz.data.frame.vector)

# ---------------------------------------------------------------------

setMethod("[", signature(x = "nz.data.frame"), 
	function (x, i=NULL, j=NULL, ..., drop=NA)
	{
		c <- c()
		# check arguments - columns
		if (try(!is.null(j),silent=TRUE) == TRUE) {
			if (is.numeric(j))
				c <- c(c,as.integer(j))
			else if (!is.integer(j))
				if (is.character(j)){	

					for (n in j){
							if (is.element(n, x@cols))
								c <- c(c, which(names(x)==n))
							else 
								if (is.element(tolower(n), x@cols))
									stop(paste("No column named ", n, " in the table. Column names are case-sensitive. Did you mean ", tolower(n), "?"))
								else if (is.element(toupper(n), x@cols))
									stop(paste("No column named ", j, " in the table. Column names are case-sensitive. Did you mean ", toupper(n), "?"))
								else stop(paste("No column named ", n, " in the table. Column names are case-sensitive.  Candidates are: ", paste(listTableColumns(x@table), collapse=', '), "."))
							
					}
	
					
				}
				else
					stop("columns argument must be integer or character")
		}
		# check arguments i (row)
		
		if(!missing(i)) {
		if (tryCatch(!is.null(i),error = function(e) {print("Sub set selection could not be created, the left-hand side of the expression must be a column reference, the right-hand side must be a value or a column reference in the same table.")}) == TRUE) {
			if (is.numeric(i))
				stop("row numbering is not allowed")
			else if (class(i) != "nz.data.frame.rows")
				stop("row object does not specify a subset")
			else if (is.null(x@where) || !nchar(x@where))
				x@where <- i@where
			else
				x@where <- paste("(", x@where, ") AND (", i@where, ")", sep="")
		}}
		# compute the right subset of columns
		if (!is.null(x@cols) && !is.null(c))
			x@cols <- x@cols[c]
		# i variable has to be of a special class "nz.data.frame.rows"
		return(x)
	}
)

# ---------------------------------------------------------------------

setMethod("$", signature(x = "nz.data.frame"),
	function(x, name) {
		c <- tolower(x@cols)
		n <- tolower(name)
		if (!is.element(n, c))
			stop("no such column in this nz.data.frame")
		x@cols = x@cols[c == n]
		return(x)
	}
)

# ---------------------------------------------------------------------

nzCreateWherePart <-
function (obj, value, operator) {
	cols <- obj@cols
	
	return(new(Class="nz.data.frame.rows",
		where = paste('"',cols, "\" ", operator, " ", value,  sep="", collapse=" AND ")))
}

# ---------------------------------------------------------------------

invisible(apply(relop, 1,
	function(x) setMethod(x[1], signature(e1="nz.data.frame"),
		function (e1, e2) {
			
			#We allow for column references on the right hand side but only if a single column is referenced and if both data frames point
			#to the same table in the data base
			if(inherits(e2,'nz.data.frame')) {
				if((length(e2@cols)==1)&&(e1@table==e2@table)&&(e1@schema==e2@schema)&&(e1@case.sensitive.table==e2@case.sensitive.table)&&(e1@case.sensitive.schema==e2@case.sensitive.schema)) {
					value <- paste('"',e2@cols[1],'"',sep='');
				} else {
					value <- e2;
				}
			} else {
				value <- paste("'",e2,"'",sep='');
			}
		
			return(nzCreateWherePart(e1, value, ifelse(nchar(x[2]), x[2], x[1])))
		}
	)
))

# ---------------------------------------------------------------------
invisible(apply(logop, 1,
	function(x) setMethod(x[1],
		signature(e1="nz.data.frame.rows",	e2="nz.data.frame.rows"),
		function(e1, e2)
			return (new(Class="nz.data.frame.rows",
		            where=paste("(", e1@where, ") ", x[2], " (", e2@where, ")")))
	)
))

# ---------------------------------------------------------------------

setMethod("dim", signature(x="nz.data.frame"),
	function(x) {
		rowsno <- nzScalarQuery("SELECT COUNT(*) FROM ",
			nzdf.from(x), ifelse(nchar(x@where), paste(" WHERE ", x@where), ""))
		
		# Integers are 32bit in R, there are tables with more rows than that
		# In this case, we need to return double instead
		if(as.integer(rowsno) == as.numeric(rowsno)){	
			return(c(as.integer(rowsno), length(x@cols)))
		} else {
			return(c(as.double(rowsno), length(x@cols)))
		}
	}
)

# ---------------------------------------------------------------------

setMethod("dim<-", signature(x="nz.data.frame", value="ANY"),
	function(x, value) stop("you cannot overwrite this", call.=FALSE)
)

# ---------------------------------------------------------------------

setMethod("length", signature(x="nz.data.frame"),
	function(x) { return(length(x@cols)) }
)

setMethod("length", signature(x="nz.data.frame"),
	function(x) { return(length(x@cols)) }
)


# ---------------------------------------------------------------------

setMethod("NROW", signature(x="nz.data.frame"),
	function(x) { return(nrow(x)) }
)

# ---------------------------------------------------------------------

setMethod("NCOL", signature(x="nz.data.frame"),
	function(x) { return(ncol(x)) }
)

# ---------------------------------------------------------------------

setMethod("colnames", signature(x="nz.data.frame"),
  function(x) { x@cols }
)

# ---------------------------------------------------------------------

setMethod("head", signature(x="nz.data.frame"),
		function(x, n = 6, ...) {
			ord <- ifelse(nzIsView(x), "", " ORDER BY rowid ")
			if (n >= 0) {
				ans <- nzQuery(nzdf.query(x), ord, " LIMIT ", n)
				columns <- nzTableDef(x, collapse=FALSE)
				for (name in names(ans)) {
					type <- columns$type[columns$name == name]
					if (toupper(substr(type, 1, 6)) == 'BIGINT' || toupper(substr(type, 1, 7)) == 'NUMERIC')
						ans[[name]] <- as.numeric(ans[[name]])
				}
				return(ans)
			}
			if (n < 0) {
				nr <- nrow(x)
				n <- abs(n)
				ans <- nzQuery(nzdf.query(x), ord, " LIMIT ", nr - n)
				columns <- nzTableDef(x, collapse=FALSE)
				for (name in names(ans)) {
					type <- columns$type[columns$name == name]
					if (toupper(substr(type, 1, 6)) == 'BIGINT' || toupper(substr(type, 1, 7)) == 'NUMERIC')
						ans[[name]] <- as.numeric(ans[[name]])
				}
				
				if ((nr-n) != 0) rownames(ans) <- 1:(nr-n)
				return(ans)
			}
		}
)


# ---------------------------------------------------------------------

setMethod("names", signature(x="nz.data.frame"),
	function(x) { return(x@cols) }
)

# ---------------------------------------------------------------------

setMethod("tail", signature(x="nz.data.frame"),
		function(x, n = 6, ...) {
			ord <- " ORDER BY rowid ";
			nr <- nrow(x)
			if (n > 0) {
				off <- min(nr - n, nr)
				ans <- nzQuery(nzdf.query(x), ord, " OFFSET ", off)
				columns <- nzTableDef(x, collapse=FALSE)
				for (name in names(ans)) {
					type <- columns$type[columns$name == name]
					if (toupper(substr(type, 1, 6)) == 'BIGINT' || toupper(substr(type, 1, 7)) == 'NUMERIC')
						ans[[name]] <- as.numeric(ans[[name]])
				}
				rownames(ans) <- (off+1):nr
			} else if (n < 0) {
				n <- abs(n)
				ans <- nzQuery(nzdf.query(x), ord, " OFFSET ", n)
				columns <- nzTableDef(x, collapse=FALSE)
				for (name in names(ans)) {
					type <- columns$type[columns$name == name]
					if (toupper(substr(type, 1, 6)) == 'BIGINT' || toupper(substr(type, 1, 7)) == 'NUMERIC')
						ans[[name]] <- as.numeric(ans[[name]])
				}
				rownames(ans) <- (n+1):nrow(x)
			}
			return(ans)
		}
)

# ---------------------------------------------------------------------

materialize <-
		function (nzdf, max.rows=NULL, order.by=FALSE) {
	if (nzIsView(nzdf) || !order.by)
		return(nzQuery(nzdf.query(nzdf, max.rows=max.rows)))
	else
		return(nzQuery(nzdf.query(nzdf, max.rows=max.rows, order.by='rowid')))
}



# ---------------------------------------------------------------------

nzdf.query <-
		function (nzdf, max.rows=NULL, order.by=NULL, case.sensitive=NULL) {
	if (!is.nz.data.frame(nzdf))
		stop(paste("cannot query object of class: ", class(nzdf)))
	
	cols <- paste('"', nzdf@cols,'"', sep='')
	
	paste("SELECT ", paste( cols, collapse=","), " FROM ",
			nzdf.from(nzdf),
			ifelse(nchar(nzdf@where), paste(" WHERE ", nzdf@where), ""),
			ifelse(is.null(order.by), "", paste(" ORDER BY ", order.by, sep="")),
			ifelse(is.null(max.rows), "", paste(" LIMIT ", max.rows, sep="")),
			sep='')
}

nzdf.from <- function (x) {
	if (!is.nz.data.frame(x))
		stop('x has to be a nz.data.frame')
	
	if(nchar(x@schema)>0) {
		if(x@case.sensitive.schema)
			ans <- paste('"',x@schema,'".',sep='')
		else
			ans <- paste(x@schema,'.',sep='')	
	} else
		ans <- '';
	
	
	if(x@case.sensitive.table)
		ans <- paste(ans,'"',x@table,'"',sep='')
	else
		ans <- paste(ans,x@table,sep='')		
	
	return(ans)
	
}


# ---------------------------------------------------------------------



nzdf.query.print <-
		function (nzdf, max.rows=NULL, order.by=NULL) {
	if (!is.nz.data.frame(nzdf))
		stop(paste("cannot query object of class: ", class(nzdf)))
	
	paste("SELECT ", paste("\"", nzdf@cols, "\"", collapse=",", sep="") , " FROM ",
			nzdf.from(nzdf),
			ifelse(nchar(nzdf@where), paste(" WHERE ", nzdf@where), ""),
			ifelse(is.null(order.by), "", paste(" ORDER BY ", order.by, sep="")),
			ifelse(is.null(max.rows), "", paste(" LIMIT ", max.rows, sep="")),
			sep='')
}

setMethod("print", signature(x="nz.data.frame"),
	function (x) {
		cat(nzdf.query.print(x),"\n")
	}
)

# ---------------------------------------------------------------------

setMethod("show", signature(object="nz.data.frame"),
	function (object) {
		cat(nzdf.query.print(object),"\n")
	}
)

# ---------------------------------------------------------------------

nzSimpleSignature <-
function (x) {
	if (!is.nz.data.frame(x))
		stop("this method can be applied only on nz.data.frame")

	nz.type <- function(y) {
		switch(typeof(y),
			double  = NZ.DOUBLE,
			integer = NZ.INT32,
			character = list(NZ.VARIABLE, 256),
			logical = NZ.BOOL
		)
	}

	d <- as.data.frame(x, max.rows=0)
	
	r <- list()
	for (n in names(d)) {
		r[[n]] <- nz.type(d[[n]])
	}

	return(r)
}

# ---------------------------------------------------------------------

nzTableDef <-
		function(nzdf,  collapse=TRUE) {
	if (!is.nz.data.frame(nzdf))
		stop("this method can be applied only on nz.data.frame")
	
	# try among tables
	objid <- nzScalarQuery ("SELECT objid FROM _v_table WHERE tablename = '", nzdf@table,"'", ifelse(nchar(nzdf@schema)>0,paste(" AND schema='",nzdf@schema,"'",sep=''),''));
	# and now try among views, if that failed
	if (is.na(objid)) {
		objid <- nzScalarQuery ("SELECT objid FROM _v_view WHERE viewname = '", nzdf@table,"'", ifelse(nchar(nzdf@schema)>0,paste(" AND schema='",nzdf@schema,"'",sep=''),''));
	}
	# if it's neither a table nor a view - throw an error
	if (is.na(objid))
		stop('could not fetch database object-id: neither a table, nor a view')
	
	attrs <- nzQuery("SELECT attname, atttype FROM _v_relation_column_def WHERE objid = ", objid,
			" AND attnum > 0 ORDER BY attnum")
	
	idx <- tolower(attrs[,1]) %in% tolower(nzdf@cols)
	
	if (as.logical(collapse))
		return(paste(attrs[idx,1], attrs[idx,2], collapse=","))
	else
		return(data.frame(name=attrs[idx,1], type=attrs[idx,2]))
}

# ---------------------------------------------------------------------

nzCreateView <- function (x, simplified=F) {
	if (!is.nz.data.frame(x))
		stop("nzCreateView is valid only for nz.data.frame objects")

	if(simplified) {
		return(nzCreateSimplifiedView(x));
	} else {
		name <- nzGetValidTableName("nz_view_")
		nzQuery("CREATE VIEW ", name, " AS ", nzdf.query(x))
		return(name)
	}
}

# ---------------------------------------------------------------------

nzCreateSimplifiedView <- function(nzdf) {
	y <- nzTableDef(nzdf, collapse=F)

	res <- c();
	count <- 0;
	for(i in 1:nrow(y)) {
		name <- as.character(y[i,1])
		type <- as.character(y[i,2])
	
		if((substr(type,1,7)=='DECIMAL')||(substr(type,1,7)=='NUMERIC')) {
			res <- c(res, paste('"',name,'"::DOUBLE as "',name,'" ',sep=''));
			count <- count+1;
		} else {
			res <- c(res, paste('"',name,'"',sep=''));
		}
	}

	if(count>0) {
		viewName <- nzGetValidTableName("NZ_VIEW_");

		nzQuery("CREATE VIEW ", viewName , " AS SELECT ", paste( res, collapse=","), " FROM ",
			nzdf.from(nzdf),
			ifelse(nchar(nzdf@where), paste(" WHERE ", nzdf@where), ""))

		return(viewName);
	} else {
		return(NULL);
	}
}


# ---------------------------------------------------------------------

nzDropView <- function(v) {
	if (!nzExistTable(v) || !nzIsView(v))
		stop("view ", v, " does not exist or is not a view")

	nzQuery("DROP VIEW ", v)
}

