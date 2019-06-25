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

nzBulkModel <- function(X, INDEX, modelingFun, output.name, clear.existing=FALSE, indexType="integer", debugger.mode = FALSE,...) {
	
	if(indexType=="integer") {
		outputSig <- list(OBJID=NZ.INT64,SID=NZ.INT32,SNIPPET=list(NZ.VARIABLE,32768));
	} else {
		outputSig <- list(OBJID=list(NZ.VARIABLE,500),SID=NZ.INT32,SNIPPET=list(NZ.VARIABLE,32768));
	}
	
	res <- nzTApply(X=X, INDEX=INDEX, 
	FUN=function (df,modelingFun) {
		model <- modelingFun(df);
		modelStr <- rawToChar(serialize(model, ascii=TRUE,connection=NULL),multiple=FALSE);
		offset <-0;
		maxSnippetSize <- 32000;
		numSnippets <- nchar(modelStr)/maxSnippetSize;
		count <- -floor(numSnippets);
		
		while(count<=0){
			modelStrSnippet <- substring(modelStr,offset,offset+maxSnippetSize-1);
			setOutput(0,getGroupValue());
			setOutputString(2,modelStrSnippet);
			setOutput(1,count);
			count <- count+1;
			offset<-offset+maxSnippetSize;
			outputResult();
		}
		c(NULL,NULL,NULL,NULL)
	},output.name=output.name,output.signature=outputSig, clear.existing=clear.existing, case.sensitive=FALSE,output.distributeOn="OBJID",debugger.mode = debugger.mode,modelingFun);	
	
	if(is.null(output.name)) {
		return(res);
	} else {
		return(nz.list(output.name));
	}
}
