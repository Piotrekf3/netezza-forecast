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

# various functions enabling NPS tables manipulation in R client

`nzShowTables` <- function() {
	nzQuery('SELECT distinct SCHEMA as "Schema", objname as "Name", objtype as "Type", Owner "Owner"
			from _v_obj_relation where objclass in (4905,4906,4908,4907,4909,4940,4911,4913,4953) ORDER BY "Schema","Name"')
	# you can add 4921 for "system view"
}

# ---------------------------------------------------------------------

nzObjType <- function (x) {
	
	if (is.nz.data.frame(x)) {
		tableName <- nzdf.from(x);
	} else {
		tableName <- x;
	}
	
	nzQuery('SELECT UPPER(objtype) as "TYPE" FROM _v_obj_relation WHERE ',
			prepareComparisonString(parseTableName(tableName), 'objname'))
}



# ---------------------------------------------------------------------

nzIsView <- function (x) {	
	return(nzObjType(x) %in% c('VIEW','SYSTEM VIEW'))
}

nzGetCurrentSchema <- function() {
	return(nzScalarQuery("select current_schema"));
}

nzIsSchemaEnabled <- function() {
	return(p_nzSchemaEnabled);
}


# ---------------------------------------------------------------------

parseTableName <- function(tableName) {
	
	case.sensitive.schema <- FALSE;
	case.sensitive.table <- FALSE;
	
	inTabNameQuotes <- F;
	sepTableHits <- c();
	
	for(i in 1:nchar(tableName)) {
		
		c <- substr(tableName,i,i);
		if(c=="\"")
			inTabNameQuotes <- !inTabNameQuotes;
		
		if((c==".")&&(!inTabNameQuotes))
			sepTableHits <- c(sepTableHits,i);	
	}
	
	if(inTabNameQuotes) {
		stop("Table name not formatted correctly");
	}
	
	#Only the table name, if DB is schema enabled, we need to 
	# read the current schema, as this could change after the
	# nz.data.frame was created
	if(length(sepTableHits)==0) {
		table <- tableName;
		if(nzIsSchemaEnabled()) {
			schema <- nzGetCurrentSchema();
		} else {
			schema <- NULL;
		}
	#Table and schema name provided, if db is not schema enabled, ignore schema	
	} else if(length(sepTableHits)==1) {
		
		if(nzIsSchemaEnabled()) {
			schema <- substr(tableName, 1, sepTableHits[1]-1);
		} else
			schema <- NULL;
		
		table <- substr(tableName, sepTableHits[1]+1, nchar(tableName));
	#DB,schema and table name, which is not allowed	
	} else if(length(sepTableHits)==2) {
			stop("Cross db access currently not supported");
	} else {
			stop("Table name ", table, " is not well-formed");
	}
	
	#Remove quotes and set to default case
	if (nchar(table) > 2 && substr(table, 1, 1) == '"' && substr(table, nchar(table), nchar(table)) == '"') {
		table <- substr(table,2,nchar(table)-1)
		case.sensitive.table <- TRUE;
	} else {
		case.sensitive.table <- FALSE;
		ifelse (nzIsUpper(), table <- toupper(table), table <- tolower(table))
	}

	#Remove quotes and set to default case
	if(!is.null(schema)) {
		if (nchar(schema) > 2 && substr(schema, 1, 1) == '"' && substr(schema, nchar(schema), nchar(schema)) == '"') {
			schema <- substr(schema,2,nchar(schema)-1)
			case.sensitive.schema <- TRUE
		} else { 
			case.sensitive.schema <- FALSE;
			ifelse (nzIsUpper(), schema <- toupper(schema), schema <- tolower(schema))
		}
	}
	
	return(list(table=table, schema=schema,case.sensitive.table=case.sensitive.table,case.sensitive.schema=case.sensitive.schema));
	
}


prepareComparisonString <- function (tableRef,tableNameAtt) {
	
	cmp <- paste(tableNameAtt, " = '", tableRef$table, "'", sep='')
	if (!is.null(tableRef$schema)&&(nchar(tableRef$schema)>0)) {		
		cmp <- paste(cmp, " AND schema ='", tableRef$schema, "'", sep='')
	}
	
	return(cmp)
}

# ---------------------------------------------------------------------

nzExistTable <- function (table) {
	nzCheckConnection()
	cmp <- prepareComparisonString(parseTableName(table), 'objname')
	return(as.logical(nzScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) AS field FROM _v_obj_relation WHERE ", cmp)))
}



# ---------------------------------------------------------------------

nzIsUpper <- function () {
	nzCheckConnection()
	
	return("UPPERCASE"==NZ.IDENTIFIER_CASE)
}

# ---------------------------------------------------------------------


nzIsLower <- function () {
	nzCheckConnection()
	
	return("LOWERCASE"==NZ.IDENTIFIER_CASE)
}

# ---------------------------------------------------------------------


nzAssertExists <- function (table) {
	
	if (!nzExistTable(table)) stop(paste(table, 'does not exist'), call.=FALSE)
	return(1)
}


# ---------------------------------------------------------------------

nzDeleteTable <- function(table) {
	if (nzExistTable(table))
		nzQuery("DROP TABLE ", table)
	invisible()
}

# ---------------------------------------------------------------------

nzTruncateTable <- function(table) {
	if (nzExistTable(table))
		nzQuery("TRUNCATE TABLE ", table);
	invisible()
}

# ---------------------------------------------------------------------

nzNoDSlices <- function () {
	return(nzScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM _V_DUAL_DSLICE"))
}

# ---------------------------------------------------------------------

nzDualDSliceTable <- function() nz.data.frame('DEFINITION_SCHEMA._V_DUAL_DSLICE')

# ---------------------------------------------------------------------

nzDualTable <- function() nz.data.frame('DEFINITION_SCHEMA._V_DUAL')

# ---------------------------------------------------------------------

nzQuery <- function (..., as.is=TRUE) {
	nzCheckConnection()

	# prepare the query
	query <- paste(..., sep="", collapse="")

	# debug
	if (nzIsDebug())
		cat(query, "\n")

	# execute the query
	result <- sqlQuery(p_nzConnection, query, believeNRows=FALSE,
				stringsAsFactors=FALSE, as.is=as.is)

	# IF THE QUERY RETURNS A STRING, IT IS AN EXCEPTION.
	if (is.character(result) && length(result) > 0) {
		stop(paste(result, collapse='\n'))
#		result <- result[length(result)]
#	    stop(paste(result, " (query: ", sub("\n.*\n", "[...]", query), ")", sep=""), call. = FALSE)
	}
	
	# return the result
	return(result)
}

# ---------------------------------------------------------------------

nzScalarQuery <- function (..., as.is=TRUE) {
	return(nzQuery(..., as.is=as.is)[[1]][1])
}

# ----------------------------------------------------------------------

nzDBName <- function() {
	nzCheckConnection()
	dsn <- strsplit(attr(p_nzConnection, 'connection.string'), ';')[[1]]
    dbname <- strsplit(dsn[grep('Database', dsn)], '=')[[1]][2]
	return(dbname)
}

# ----------------------------------------------------------------------

listTableColumns <- function (table) {
	nzCheckConnection()
	cmp <- prepareComparisonString(parseTableName(table), 'name')
	return(as.vector(nzQuery("SELECT attname AS field FROM _V_RELATION_COLUMN WHERE ", cmp, " ORDER BY ATTNUM")[[1]]))
}


