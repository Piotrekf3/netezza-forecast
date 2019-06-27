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

# nzSparse2matrix converts a data.frame or nz.data.frame 
# with row-column-value format to standard R matrix format
# rownames and column namse are taken from the initiate matrix
nzSparse2matrix <- function(tab, with.names=TRUE) {
  if (class(tab)=="nz.data.frame") 
    tab = nzr::as.data.frame(tab)
  if (ncol(tab)<2) 
      stop(simpleError("two or more columns are expected in the row-column-value format!!!"))

  dimensions = ncol(tab)-1
  label = list()
  for (d in 1:dimensions) {
     label[[d]] = unique(tab[,d])
  }
  if (with.names) {
    wsp = NULL
    wspd = numeric(dimensions)
    for (d in 1:dimensions) {
      tab[,d] = as.numeric(factor(tab[,d], levels=unique(tab[, d])))
      wspd[d] = max(tab[,d])
    }
  } else {
    wsp = NULL
    wspd = numeric(dimensions)
    for (d in 1:dimensions) {
      tab[,d] = as.numeric(tab[,d])
      wspd[d] = max(tab[,d])
    }
  }
  datas = numeric(prod(wspd))
  layers = c(1,cumprod(wspd)[-dimensions])
  for (i in 1:nrow(tab)) {
    pos = sum((tab[i,1:dimensions]-1)*layers)+1
    datas[pos] = tab[i,dimensions+1]
  }
  if (with.names) {
    mat = array(datas, dim=wspd, dimnames=label)
  } else {
    mat = array(datas, dim=wspd)
  }
  mat
}


#  llabs = sort(labels123,T)[1:5000]
#  la1 = as.character(tab[,1])
#  la2 = as.character(tab[,2])
#  mat = matrix(0,5000,5000)
#  for (i in 1:length(la1)) 
#     if (length(which(llabs==la1[i]))>0 & length(which(llabs==la2[i]))>0)
#        mat[which(llabs==la1[i]), which(llabs==la2[i])] = tab[i,3]
        
        
  
matrix2rcv <- function(tab) {
  row = rep(1:nrow(tab), times=ncol(tab))
  col = rep(1:ncol(tab), each=nrow(tab))
  value = unlist(tab)
  data.frame(row, col, value)
}
  
  
