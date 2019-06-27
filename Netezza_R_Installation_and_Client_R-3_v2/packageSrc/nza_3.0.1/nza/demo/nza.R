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

################################################################
# Prepare environment

if (!any(names(odbcDataSources()) == 'NZSQL')) {
  simpleError("The ODBC DSN named 'NZSQL' must be defined!")
}

nzDisconnect()
nzConnectDSN('NZSQL')

if (!nzExistTable('adult')) { 
     res <- try(nzQuery("CREATE VIEW adult AS SELECT * FROM nza..adult"))
	 if (inherits(res, "try-error")) {
		stop("NZA database does not have adult data loaded. \nSee Netezza Analytics Administrator's Guide for details.")
	 }   
}
nzAdult <- nz.data.frame("adult") 

# HIT ENTER
invisible(readline())

################################################################
# Classification tree

nzTr <- nzDecTree(INCOME ~ AGE+WORKCLASS+FNLWGT+EDUCATION+RACE+SEX, data=nzAdult, id="ID", maxdepth=4)

plot(nzTr)
print(nzTr)

# HIT ENTER
invisible(readline())

################################################################
# Clustering: K-Means and TwoStep 

nzClust1 <- nzKMeans(nzAdult, k=5, id="ID")
print(nzClust1)

# HIT ENTER
invisible(readline())

nzClust2 <- nzTwoStep(nzAdult, id="ID")
print(nzClust2)

# HIT ENTER
invisible(readline())

################################################################
# Contingency table

nzC = nzCa(~EDUCATION+OCCUPATION,nzAdult)

plot(nzC)

# HIT ENTER
invisible(readline())

################################################################
# PCA and linear regression

nzLm(AGE~EDUCATION_NUM+factor(INCOME)+factor(SEX), nzAdult)

pca <- nzPCA(nzAdult[,c(2,4,6)])

loadings(pca)
plot(pca)

# HIT ENTER
invisible(readline())

################################################################
# Diconnect

nzDisconnect()

 