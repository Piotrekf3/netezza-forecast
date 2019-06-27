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


nzSingleModel <- function(X,modelingFun,force=F) {
	
	MAX_NUM_ROWS <- 100000;
	numRows <- NROW(X);
	if(numRows>MAX_NUM_ROWS && !force)
		stop("Table has too many rows to be processed with this function.");
	
	name <- nzGetValidTableName("nz_view_");
	colName <- nzGetUniqueColumnName(X,"XID_");
	
	#TODO: Generate random column name 	
	nzQuery("CREATE VIEW ", name, " AS SELECT ", paste('"',names(X),'",',collapse="",sep=""),"1 AS ",colName," FROM ",nzdf.from.where(X)); 
	nzViewDF <- nz.data.frame(name);
	nameOut <- nzGetValidTableName("nz_table_");
	
	obj <- NULL;
	tryCatch({	
		nzl <- nzBulkModel(nzViewDF,colName,modelingFun=modelingFun,output.name=nameOut);
		obj <- nzl[1];
				
	}, error = function(e) {
				stop(e)
			}, finally = {
				nzDropView(name);
				nzDeleteTable(nameOut);	
			}
	);
		
	obj
}