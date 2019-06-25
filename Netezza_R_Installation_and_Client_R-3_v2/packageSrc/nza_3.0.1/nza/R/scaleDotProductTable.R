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

# scaleDotProductTable scales Dot Product table 
# since columns are not centered, user may center them
scaleDotProductTable <- function(x, center=TRUE) {
  sums = x[nrow(x), -ncol(x)]
  n    = x[nrow(x), ncol(x)]
  x    = x[-nrow(x), -ncol(x)]
  if (!center) return(list(mat = x, n = n))
  list(mat = x - outer(sums/n, sums/n)*n, n = n)
}

