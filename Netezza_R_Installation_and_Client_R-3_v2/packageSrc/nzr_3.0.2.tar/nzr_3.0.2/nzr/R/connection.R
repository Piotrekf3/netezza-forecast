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


setSpuCount <- function () {
	# NPS bug bypass
	spuCount <- nzScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM _V_DUAL_DSLICE")
	# until this is fixed the query below has to be executed so we will
	# be able to run our functions on SPUs and not on the Host
	if (spuCount < 12)
		nzQuery ("SET SPU_COUNT=100");
}

# ----------------------------------------------------------------------

nz.connection.sanity.check <- function (force) {
	if (nzIsConnected()) {
		if (force)
			warning('opening connection but the previous one is not closed')
		else
			stop('already connected - close the previous connection with ',
					'`nzDisconnect` before opening a new one')
	}
}

# ----------------------------------------------------------------------

initialQueries <- function (verbose) {
	
	#Detect schema support
	assign('p_nzSchemaEnabled', F, envir=globalenv())
	try({currentSchema <- nzScalarQuery('select current_schema');nzQuery('set schema ', currentSchema);assign('p_nzSchemaEnabled', T, envir=globalenv())},silent=T);
	
	queries <- function () {
		setSpuCount()
		setupNpsDataTypes()
		if (verbose) nzDependencies(type=NULL)
	}
	res <- try(queries(), silent=TRUE)
	if (inherits(res, "try-error")) {
		cat(res[1])
		cat("\n\n\t\tIMPORTANT!\n\n")
		cat("In order to use the *nzr* package one has to make sure that the R Adapter\n")
		cat("is installed on the Netezza host one tries to connect to.\n")
		cat("See the  `prerequisites'  manual page which is a part of the *nzr* package\n")
		return(FALSE)
	}
	
	
	return(TRUE)
}

# ----------------------------------------------------------------------

nzConnect <- function(user, password, machine, database, force=FALSE, queryTimeout=0, loginTimeout=0, verbose=TRUE) {
	nz.connection.sanity.check(force)

	# setup the connection string
	connectionString <- paste("DRIVER={NetezzaSQL};SERVER=", machine, ";DATABASE=", database,
						";UID=", user, ";PWD=", password, ";QueryTimeout=", queryTimeout,
						";LoginTimeout=", loginTimeout, sep="")
	# in debug (if turned on before opening the connection) mode will create a file
	# specified by LogPath (by default: /tmp)
	if (nzIsDebug()) {
		connectionString <- paste(connectionString,
			";DebugLogging=1;DebugOnTheFly=1", sep="")
	}

	# remember the password - needed for nz.run.stored.procedure
	assign('p_nzPassword', password, envir=globalenv())
	# open the connection
  opt.warn <- options()$warn
  options(warn=2)
  assign('p_nzConnection', odbcDriverConnect(connectionString), envir=globalenv())
  options(warn=opt.warn)
  
	initialQueries(verbose)
	invisible(as.logical(p_nzConnection))
}

# ----------------------------------------------------------------------

nzConnectDSN <- function(dsn, force=FALSE, verbose=TRUE) {
	nz.connection.sanity.check(force)

	# open the connection
  opt.warn <- options()$warn
  options(warn=2)
	assign('p_nzConnection', odbcConnect(dsn), envir=globalenv())
  options(warn=opt.warn)
	# remember password if given
	m <- grep("PWD=[^;]+", dsn, ignore.case=TRUE)
	if (length(m))
		assign('p_nzPassword', substr(dsn, m+4, m+attr(m, "match.length")), envir=globalenv())
	else
		assign('p_nzPassword', NULL, envir=globalenv())

	initialQueries(verbose)
	invisible(as.logical(p_nzConnection))
}

# ----------------------------------------------------------------------

nzIsConnected <- function () {
	return(exists("p_nzConnection") && inherits (p_nzConnection, "RODBC"))
}

# ----------------------------------------------------------------------

nzDisconnect <- function () {
	if (nzIsConnected()) {
		try(odbcClose(p_nzConnection), silent=TRUE)
		if (exists("p_nzConnection"))
			rm("p_nzConnection", envir=globalenv())
		if (exists("p_nzPassword"))
			rm("p_nzPassword", envir=globalenv())
	}
}

# ----------------------------------------------------------------------

nzCheckConnection <- function () {
	# make sure that the connection exists
	if (!exists("p_nzConnection") || is.null(p_nzConnection))
		stop("connection not opened", call.=FALSE)
}

