# 
# Copyright (c) 2010, 2014, IBM Corp. All rights reserved.
# Copyright (c) 2012 Revolution Analytics 
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
		
if (!any(names(odbcDataSources()) == 'NZSQL')) {
  simpleError("The NZSQL DSN must be defined!")
}

nzDisconnect()
nzConnectDSN('NZSQL')
if (!nzExistTable('iris')) { 
     res <- try(nzQuery("CREATE VIEW iris AS SELECT * FROM nza..iris"))
	 if (inherits(res, "try-error")) {
		stop("NZA database does not have iris data loaded. \nSee Netezza Analytics Administrator's Guide for details.")
	 }   
}
nzIris <- nz.data.frame('iris') 
################################################################
# Basic information about NPS

nzShowTables()
nzExistTable("iris")
nzTableDef(nzIris) 
nzQuery("select count(*) from iris;")

# HIT ENTER
invisible(readline())

################################################################
# Uploading / Downloading data

class(nzIris)
head(nzIris)
dim(nzIris)
nzSmallIris <- if(nzIsUpper()) nzIris[nzIris$SEPALLENGTH < 5, 1:3] else nzIris[nzIris$sepallength < 5, 1:3]
as.data.frame(nzSmallIris)

# HIT ENTER
invisible(readline())

################################################################
# Parallel apply/tapply

sqrtFUN = function(x) sqrt(x[[1]])
res = nzApply(nzIris[,2:3], 1, sqrtFUN)
as.data.frame(res)

nzTApply(nzIris, 2, function(x) mean(x))

# HIT ENTER
invisible(readline())

################################################################
# Debugging

nzDebug(TRUE)
head(nzTApply(nzIris, 5, function(x) mean(x)))
nzDebug(FALSE)

#FUN2debug = function(x) if(min(x[,1]) < 4.5) cov(0) else min(x[,1])
#
# Hit 0 to exit
#
# You need to have nzrserver installed on your machine to run this example
#library(nzrserver)
#nzTApply(nzIris, 5, FUN2debug, debugger.mode=T)

nzDisconnect()

