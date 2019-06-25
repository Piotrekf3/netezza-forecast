# 
# Copyright (c) 2013, 2014, IBM Corp. All rights reserved. 
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
# nz.list operators

nz.list <- function (tableName, createTable=FALSE,indexType=NULL) {
	#check if table exists
	#possibly create the table
	#use indexType
	
	if(!nzExistTable(tableName)) {
		if(createTable) {
			nz.createObjTable(tableName,indexType=="integer");			
		} else {
			stop("the corresponding table does not exist, use option createTable=T to create empty one.");
		}		
			
	}
	
	return(new(Class="nz.list", tableName=tableName))
	
}

setMethod("[", signature(x = "nz.list"),
	function(x, i) {
		nz.getObj(i,x@tableName);
		#print("get: ");print(x@tableName);print(i);
	}
)

setMethod("$", signature(x = "nz.list"),
	function(x, name) {
		nz.getObj(name,x@tableName);
		#print("get: ");print(x@tableName);print(name);
	}
)

setMethod("[<-", signature(x = "nz.list"),
	function(x, i,value) {
		nz.storeObj(i,value,x@tableName)
		#print("store: ");print(x@tableName);print(i);print(value)
		x;
	}
)

setMethod("$<-", signature(x = "nz.list"),
	function(x, name,value) {
		nz.storeObj(name,value,x@tableName)
		#print("store: ");print(x@tableName);print(name);print(value)
		x;
	}
)

setMethod("length", signature(x="nz.list"),
	function(x) { return(nzScalarQuery("select count(distinct OBJID) from ",x@tableName)) }
)

setMethod("names", signature(x="nz.list"),
	function(x) { return(nzQuery("select distinct OBJID from ",x@tableName)[[1]]) }
)

setMethod("print", signature(x="nz.list"),
	function (x) {
		cat(x@tableName,"\n")
	}
)

setMethod("show", signature(object="nz.list"),
	function (object) {
		cat(object@tableName,"\n")		
	}
)

is.nz.list <- function(x) {
	return(inherits(x, "nz.list"))
}

nzAppendList <- function(x,listToAppend) {
	#TODO: Add check for duplicates
	
	if(is.nz.list(x) && is.nz.list(listToAppend)) {
		nzQuery("INSERT INTO ",x@tableName," SELECT * FROM ", listToAppend@tableName);
	}
	else {
		stop("this method can only be used to append elements of one nz.list to another nz.list");	
	}	
}

nzDeleteList <- function(x) {
	if(is.nz.list(x)) {
		nzQuery("DROP TABLE ",x@tableName);
	}
	else {
		nzQuery("DROP TABLE ",x);
	}	
}

nz.storeObj <- function(id, obj, tableName) {

	nz.delObj(id,tableName);
	
	if(!is.null(obj)) {
		objStr <- rawToChar(serialize(obj, ascii=TRUE,connection=NULL),multiple=FALSE);
		offset <-0;
		maxSnippetSize <- 32000;
		numSnippets <- nchar(objStr)/maxSnippetSize;
		count <- -floor(numSnippets);
	
		while(count<=0){
			objStrSnippet <- substring(objStr,offset,offset+maxSnippetSize-1);
			nzQuery("INSERT INTO ", tableName," VALUES( '", id, "',", count, ",'", objStrSnippet,"')");
			count <- count+1;
			offset<-offset+maxSnippetSize;
		}
	}
}

nz.createObjTable <- function(tableName,objIdInt) {
	if(objIdInt==TRUE) {
		nzQuery("CREATE TABLE ", tableName, " (OBJID BIGINT, SID INTEGER, SNIPPET VARCHAR(32768))");
	}
	else {
		nzQuery("CREATE TABLE ", tableName, " (OBJID VARCHAR(500), SID INTEGER, SNIPPET VARCHAR(32768))");
	}
}

nz.delObj <- function(id, tableName) {
	nzQuery("delete from ",tableName," WHERE OBJID='",id,"'");
}

nz.getObj <- function(id, tableName) {
	objTable.df <- nz.data.frame(tableName);
	mdf <- as.data.frame(objTable.df[objTable.df$objid==id,]);
	
	#Key does not exist
	if(NROW(mdf)==0)
		return(NULL)
	
	obj <- NULL;
	
	df <- mdf[with(mdf, order(SID)), ]
	snippets <- c();
	for(i in 1:nrow(df)) {
		groupid <- df[i,1];
		objStrSnippet<-df[i,3];
		snippetId <- df[i,2];
		snippets <- c(snippets, objStrSnippet);
		if(snippetId == 0) {
			objStr <- paste(snippets,collapse = "",sep="");
			obj<- unserialize(charToRaw(objStr));
		}
	}
	obj
}
