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
#

nzGetUniqueColumnName <- function(nzdf,prefix) {
	prefix <- ifelse(nzIsUpper(),toupper(prefix),tolower(prefix)) 	
	while (TRUE) {
		name <- paste(prefix, floor(runif(1,0,100000)), sep="")
			if (!(name %in% nzdf@cols))
				return(name)
	}
}

nzGetValidTableName <- function (prefix = "data_frame_") {
    prefix <- ifelse(nzIsUpper(),toupper(prefix),tolower(prefix)) 
	while (TRUE) {
		name = paste(prefix, floor(runif(1,0,100000)), sep="")
			if (!nzExistTable(name))
				return(name)
	}
}

#nzExistColumn <- function (name, table, case.sensitive=FALSE) {
#	nzCheckConnection()
#	cmp <- prepareComparisonString(name, 'objname')
#	
#	cmp2 <- prepareComparisonString(table, 'objname')
#	
#	return(as.logical(nzScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) AS field FROM _v_obj_relation WHERE ", cmp, " AND ", cmp2)))
#}