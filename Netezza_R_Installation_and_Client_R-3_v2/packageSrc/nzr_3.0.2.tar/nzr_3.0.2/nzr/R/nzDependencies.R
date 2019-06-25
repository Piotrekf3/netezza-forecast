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


nzDependencies <- function(types=c("r_ae")) {
	ret <- ""
# 'r' & 'r_ae' cartridge version
	if (!is.null(types)) {
		
		fun <- function (types) {
			getNext()
			t <-  readLines(pipe(paste("/nz/var/nzcm/current/nzcm --linstalled")))
			
			for (i in 1:length(t)) {
				cartridge <- strsplit(t[i], "\\ ")[[1]][1]
				
				if (is.element(cartridge,types)) {
					version <- strsplit(t[i], "\\ ")[[1]][3] 
					setOutput(0, paste("Installed version of ' " , cartridge," ' cartridge: ", version))
					outputResult()	
					types <- types[!types==cartridge]
				}
			}
			
			for (type in types) {
				setOutput(0,paste("Cartridge ' ",type ," ' is not installed", sep=''))
				outputResult()
				setOutput(0, " ")
				outputResult()
				setOutput(0, " ")
				outputResult()
			}
			
			setOutput(0," ")
			outputResult()
			
			
		}
		
		
		environment(fun) <- emptyenv()
		
		rcart <- nzt(nzRunHost(fun, types=types))
		if (!is.null(rcart)) {
			i <- 1
			while (i < length(rcart[[1]])){
				ret <- paste (ret, rcart[[1]][i], "\n", sep='')
				i <- i + 1
			}
		}
	}
# R host and spu version
	
	
	f <- function(x) {return(paste("R ",version$major,".",version$minor,sep=''))}	
	environment(f) <- emptyenv()
	rspu <- nzt(nzApply(nzDualDSliceTable(), 0, f))
	
	fun2 <- function (x) {
		getNext()
		lines <- paste("R ",version$major,".",version$minor,sep='')
		for (line in lines) {
			setOutput(0, line)
			outputResult()
		}
	}
	environment(fun2) <- emptyenv()
	rhost <- nzt(nzRunHost(fun2))
	ret <- paste (ret,  "\nOn the spus: ", rspu[[1]][1],"\nOn the host: ", rhost[[1]][1], sep='')

	cat(ret)
}
