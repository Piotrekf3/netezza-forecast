# 
# Copyright (c) 2010, 2011, IBM Corp. All rights reserved. 
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

getNPSDataType <-
function (column) {
	if (is.integer(column))
		return("integer")
	if (is.numeric(column))
		return("double")
	if (is.character(column) || is.factor(column)) {
		if (is.factor(column))
			column <- levels(column)
		len  <- max(nchar(column))
		type <- ifelse(any(Encoding(column) == "UTF-8"), "nvarchar", "varchar")
		return(paste(type, "(", len, ")", sep=''))
	}

	stop("data type '", class(column), "' is not handled yet")
}

