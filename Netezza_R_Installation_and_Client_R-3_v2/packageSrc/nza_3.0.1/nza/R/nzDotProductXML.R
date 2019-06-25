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

nzPrepareXML4DotProduct <- function(form, nzdf, intable, outtable=nzGetValidTableName(), pname="linear regression",weights=-1) {
	ntab1 = nzParseRFormula(form, nzdf)
	varlist = paste("\"",ntab1$varlist,"\"",sep="")
	areFactors = ntab1$areFactors
	inter = ntab1$intercept
	fiordervars = ntab1$fiordervars
	siordervars = ntab1$siordervars
	intable = paste("\"",intable,"\"",sep="")
	outtable = paste("\"",outtable,"\"",sep="")
	
	
	# OLD
	#  part1 = paste(intable, ";", outtable, ";", sep="")
	#  part2 = paste("0-",paste(varlist, areFactors+0, 0, 0, sep=":"), sep="", collapse=",")
	#  part3 = paste(";;0-",inter+0,";",weights,":;0", sep="")
	
	#  xmlModel = paste(part1, part2, part3, sep="")
	
	# NEW
	variableList = ""
	if (length(fiordervars) > 0) {
		variableList = ifelse(areFactors[fiordervars], 
				paste("   <variable type=\"factor\">",varlist[fiordervars],"</variable>",sep=""),
				paste("   <variable>",varlist[fiordervars],"</variable>",sep=""))
		variableList = paste(variableList, collapse="\n")
	}
	
	variableList2 = ""
	if (length(siordervars)>0) {
		variableList2 = apply(siordervars, 2, function(xi) {
					a1 = ifelse(areFactors[xi[1]], 
							paste("   <variable type=\"factor\">",varlist[xi[1]],"</variable>",sep=""),
							paste("   <variable>",varlist[xi[1]],"</variable>",sep=""))
					a2 = ifelse(areFactors[xi[2]], 
							paste("   <variable type=\"factor\">",varlist[xi[2]],"</variable>",sep=""),
							paste("   <variable>",varlist[xi[2]],"</variable>",sep=""))
					paste("<interaction>",a1,a2,"</interaction>", sep="")
				})
		variableList2 = paste(variableList2, collapse="\n", sep="")
	}
	
	# include variableDepe only if it is a left hand side variable 
	variableDepe = ifelse(varlist[1] %in% varlist[fiordervars],
			"",
			paste("   <variable>",varlist[1],"</variable>",collapse="\n",sep=""))
	inString     = paste("   <input><table>",intable,"</table></input>",sep="")
	outString    = paste("   <output savedict=\"1\"><table>",outtable,"</table></output>",sep="")
	intercept    = ifelse(inter==1,"\n   <intercept/>","")
	xmlModel     = paste("<dotproduct name=\"",pname,"\">\n",inString,"\n",outString,"\n",variableDepe,variableList,"\n",variableList2,"\n",intercept,"\n</dotproduct>",sep="")
	list(xmlModel = xmlModel, ntab1 = ntab1)
}

#dp_small_kdd;dp_model2;0-duration:0:0:0,0-diff_srv_rate:0:0:0,0-protocol_type:1:0:0,0-attack_type:1:0:0;0-duration:0:0:0#same_srv_rate:0:0:0,0-srv_count:0:0:0#attack_type:1:0:0,0-flag:1:0:0#count:0:0:0,0-protocol_type:1:0:0#attack_type:1:0:0;0-1;-1:;0
#inputTable; outputTable;group - name:factor:scaled:centered; interactions grupa-zmienna:factor:scaled:centered # grupa-zmienna:factor:scaled:centered; group - 1 (interecept); -1 without weights | 0 with poisson | 1 : with weights; savedict

nzPrepareXML4DotProductsimple <- function(varlist, areFactors=rep(F,length(varlist)), scaled=rep(F,length(varlist)), centered=rep(F,length(varlist)), inter = F, intable, outtable=nzGetValidTableName(), pname="linear regression", weights=-1) {
	## OLD
	#  part1 = paste(intable, ";", outtable, ";", sep="")
	#  part2 = paste("0-",paste(varlist, areFactors+0, scaled+0, scaled+0, sep=":"), sep="", collapse=",")
	#  part3 = paste(";;0-",inter+0,";",weights,":;0", sep="")
	#
	#  xmlModel = paste(part1, part2, part3, sep="")
	
	# NEW
	varlist = paste("\"",varlist,"\"",sep="")
	intable = paste("\"",intable,"\"",sep="")
	outtable = paste("\"",outtable,"\"",sep="")
	
	part1 = ifelse(areFactors, " type=\"factor\" ", "")
	part2 = ifelse(scaled    , " scaled=\"T\" ", "")
	part3 = ifelse(centered  , " centered=\"T\" ", "")
	
	variableList = paste("   <variable ", part1, part2, part3, ">",varlist, "</variable>", sep="", collapse="\n")
	
	inString     = paste("   <input><table>",intable,"</table></input>",sep="")
	outString    = paste("   <output savedict=\"0\"><table>",outtable,"</table></output>",sep="")
	intercept    = ifelse(inter==1,"\n   <intercept/>","")
	xmlModel     = paste("<dotproduct name=\"",pname,"\">\n",inString,"\n",outString,"\n",variableList,"\n",intercept,"\n</dotproduct>",sep="")
	xmlModel
}



