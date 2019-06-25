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

#nzdf <- nz.data.frame('IRIS')
#irisnz <- as.data.frame(nzdf)
#lmiris <- lm('SEPALLENGTH~SEPALWIDTH',irisnz)
#teiris <- tree('SEPALLENGTH~SEPALWIDTH',irisnz)
#sc <- nzScore(nzdf,rpiris,output.name='IRISSCNEW2','ID',output.signature=list(ID=NZ.INT32,PRED=NZ.DOUBLE,SE=NZ.DOUBLE),clear.existing=T,se.fit=T)

nzScore <- function(nzdf, model, output.name, idCol, output.signature=NULL, clear.existing=T, requiredPkg=NULL, batchSize=10000, ...) {
	
	idIndex <- which(names(nzdf)==idCol);
	if(length(idIndex)==0) {
		stop("ID column not found")
	}

	modelClass <- class(model);
	if(is.null(requiredPkg)) {
		if((modelClass=='tree')||(modelClass=='rpart')) {
			requiredPkg <- as.character(modelClass);
		}
	}

scoreFun <- function(model,colNames,idIndex,requiredPkg,batchSize,...) {
	
	if(!is.null(requiredPkg)) {
		library(requiredPkg,character.only=T)
	}	

	df <- fetchRows(batchSize);
	while(!is.null(df)) {
		names(df) <- colNames; 
		pred <- predict(object=model,newdata=df,...);
		
		seCalc <- (class(pred)=="list");		
		if(seCalc) {
			predVals <- pred$fit;
			predSE <- pred$se.fit
		} else {
			predVals <- pred;
		}

		for(i in 1:NROW(df)) {
			setOutput(1,predVals[i]);
			setOutput(0,df[[idIndex]][i]);
			if(seCalc) {
				setOutput(2,predSE[i])
			}

			outputResult();	
		}	
		df <- fetchRows(batchSize);			
	}
}

# Do the scoring
nzRun(nzdf,scoreFun, output.name = output.name, output.signature = output.signature, 
clear.existing = clear.existing, case.sensitive = FALSE, output.distributeOn = idCol,model,names(nzdf), idIndex, requiredPkg,batchSize,... );
}
