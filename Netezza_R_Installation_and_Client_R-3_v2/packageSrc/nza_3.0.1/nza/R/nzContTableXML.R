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
#<?xml version="1.0"?>
#<!DOCTYPE ctable [
#<!ELEMENT ctable (input,variable+,output)>
#
#<!ATTLIST ctable name CDATA #IMPLIED>
#<!ATTLIST ctable sufficientStatistic CDATA #FIXED "contingencyTable">
#
#<!ELEMENT input EMPTY>
#<!ATTLIST input table CDATA #REQUIRED>
#
#<!ELEMENT output EMPTY>
#<!ATTLIST output table CDATA #REQUIRED>
#<!ATTLIST output sparse (0|1) #REQUIRED>
#
#<!ELEMENT variable EMPTY>
#<!ATTLIST variable name CDATA #IMPLIED>
#]>
#

# inTAble;outTable;varaibles+separated+by+plus;sparse
nzPrepareXML4ContTable <- function(varlist, intable, outtable=nzGetValidTableName(), name="simple correspondence analysis") {
  varlist = paste("\"",varlist,"\"",sep="")
  intable = paste("\"",intable,"\"",sep="")
  outtable = paste("\"",outtable,"\"",sep="")
  
  variableList = paste("   <variable>",varlist,"</variable>",collapse="\n",sep="")
  inString     = paste("   <input><table>",intable,"</table></input>",sep="")
  outString    = paste("   <output sparse=\"1\"><table>",outtable,"</table></output>",sep="")
  xmlModel     = paste("<ctable name=\"",name,"\">\n",inString,"\n",outString,"\n",variableList,"\n</ctable>",sep="")
  xmlModel
}

# OLD
#  xmlModel = paste(intable, ";", outtable, ";", paste(varlist, collapse="+"), ";", 1, sep="")
#
