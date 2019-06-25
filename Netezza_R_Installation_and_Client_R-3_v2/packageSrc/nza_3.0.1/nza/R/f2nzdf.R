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

nzParseRFormula <- function(form, nzdf){
  if (!is.nz.data.frame(nzdf)) 
      stop(simpleError("nzdf must be an object of nz.data.frame class"))

  vars = all.vars(form)

  # is there a dot?  
  # special case - if there is . in the variable list
  colu = nzdf@cols
  where.is.dot = grep(".",vars,fixed=T)
  if (length(where.is.dot)>0) {
    if (!all(vars[-where.is.dot] %in% colu))
      stop(simpleError(paste("Some variables are not avaliable in nz.data.frame:", paste(setdiff(vars[-where.is.dot], colu),collapse=", "))))    
    vars = c(vars[-where.is.dot], setdiff(colu, vars))
    fiordervars = seq_along(vars)[-1]
    siordervars = NULL
  } else {
    tab1 = attr(terms(form), "factors")
    order1 = attr(terms(form), "order")
    
    #
    # only first order variables
    fordervars = sapply(which(order1==1), function(i) vars[tab1[,i]>0])
    fiordervars = sapply(which(order1==1), function(i) which(tab1[,i]>0))
  
    #
    # only second order variables
    sordervars = sapply(which(order1==2), function(i) vars[tab1[,i]>0])
    siordervars = sapply(which(order1==2), function(i) which(tab1[,i]>0))
    # test: are all variables in data frame?
    if (!all(vars %in% colu))
      stop(simpleError(paste("Some variables are not avaliable in nz.data.frame:", paste(setdiff(vars, colu),collapse=", "))))
  }

  
  # or +0 or -1
  addIntercept = attr(terms(form, keep.order=T, data=data.frame(x=1)), "intercept")
  isResponse   = attr(terms(form, keep.order=T, data=data.frame(x=1)), "response")
  
  areFactors = rep(F, length(vars))
  if (length(where.is.dot) == 0) 
      areFactors   = paste("factor(",vars,")",sep="") %in% rownames(tab1)
  
  # check agains transformations    
  if (length(where.is.dot)>0) {
  # left side    
      if (length(form)==3 & !(as.character(form)[2] %in% vars))
         stop(paste("Error: Variable transformations are not supported."))
  } else {
    are.not1 <- !(vars %in% rownames(tab1))
    are.not2 <- !(paste("factor(",vars,")",sep="") %in% rownames(tab1))
    are.not <- are.not1 & are.not2 
    if (any(are.not)) {
       stop(paste("Error: Variable transformations are not supported."))
    }
  }

  # create corresponding nz data frame
  indy = unlist(sapply(vars, function(x) which(colu==x, T)))
  list(data=nzdf[,indy], intercept=addIntercept, response=isResponse, varlist=vars, areFactors = areFactors, fiordervars = fiordervars, siordervars = siordervars)
}

#
# test cases
#
# nzIris <- nz.data.frame("iris")
# nzParseRFormula(class~., nzIris)
# nzParseRFormula(class~sepallength, nzIris)
# nzParseRFormula(class~sepallength+petalwidth, nzIris)
# nzParseRFormula(class~sepallength+petalwidth-1, nzIris)
# nzParseRFormula(~., nzIris)
# nzParseRFormula(y~., nzIris)
# 

