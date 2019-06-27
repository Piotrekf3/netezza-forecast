# 
# Copyright (c) 2010, 2011, IBM Corp. All rights reserved. 
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

# working with CRAN packages

nzInstallPackages <- function (pkg, installOnSpus=TRUE) 
{
	# determine the package location
	if (length(grep("^http", pkg)) > 0)
		input <- url(pkg, "rb")
	else
		input <- file(pkg, "rb")

	fname <- gsub(" *$", "", basename(pkg))
	fname <- gsub("^ *", "", fname)

	# send the package file
	CHUNK_SIZE = 24000
	chunk = encodebase64raw(readBin(input, 'raw', n=CHUNK_SIZE))
	nzScalarQuery("SELECT filename FROM TABLE WITH FINAL(nzr..placefilebin('createfile','", fname, "','", chunk, "'))")

	while (TRUE) {
		chunk = encodebase64raw(readBin(input, 'raw', n=CHUNK_SIZE))
		if (nchar(chunk) <= 0)
			break;
		nzScalarQuery("SELECT * FROM TABLE WITH FINAL(nzr..placefilebin('appendfile','", fname, "','", chunk, "'))")
	}
	close(input)

	# meta-data for NPS
	toserialize <- list(mode = 'install', file=basename(pkg))

	# Host
	cat("Host:\n")
	res <- nzt(nzQuery("SELECT * FROM TABLE WITH FINAL(nzr..r_udtf('CODE_SERIALIZED=",
			encodebase64(rawToChar(serialize(toserialize,NULL,ascii=TRUE))), "'))"))
	cat(paste(res[[1]], collapse="\n"), "\n")

	# SPUs
	if (installOnSpus) {
		res <- nzt(nzQuery("SELECT * FROM _V_DUAL_DSLICE,TABLE WITH FINAL(nzr..r_udtf(dsid,'CODE_SERIALIZED=",
				encodebase64(rawToChar(serialize(toserialize,NULL,ascii=TRUE))), "')) ",
						" WHERE _V_DUAL_DSLICE.dsid=1"))
		cat("SPUs:\n")
		cat(paste(res[[1]], collapse="\n"),"\n")
	}

}

# ----------------------------------------------------------------------

nzIsPackageInstalled <- function (package) {
	package <- as.character(substitute(package, environment()))

	f <- function(x,package) { return(is.element(package, rownames(installed.packages()))) }	
	g <- function(package) {	
		getNext()
		p <- is.element(package, rownames(installed.packages()))		
		setOutput(0, p)
		outputResult()
		}

	ret <- c(
		as.logical(nzt(nzRunHost(g, package=package))),
		all(as.logical(nzt(nzApply(nzDualDSliceTable(), 0, f, package=package))[,1]))
	)
	
	names(ret) <- c('host','spus')
	return(ret)
}

