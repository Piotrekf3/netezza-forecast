% 
% Copyright (c) 2010, 2014, IBM Corp. All rights reserved. 
% 		
% This program is free software: you can redistribute it and/or modify 
% it under the terms of the GNU General Public License as published by 
% the Free Software Foundation, either version 3 of the License, or 
% (at your option) any later version. 
%
% This program is distributed in the hope that it will be useful, 
% but WITHOUT ANY WARRANTY; without even the implied warranty of 
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
% GNU General Public License for more details. 
%
% You should have received a copy of the GNU General Public License 
% along with this program. If not, see <http://www.gnu.org/licenses/>. 
% 
% 
\name{nzRun}
\alias{nzRun}
\alias{nzRunHost}
\title{Run R code on Netezza - low level}
\description{
  These functions run any arbitrary user-provided code, using
  the R server API, on Netezza.
}
\usage{
nzRun(x, fun, output.name = NULL, output.signature = NULL,
  clear.existing = FALSE, case.sensitive = FALSE, output.distributeOn=NULL, ...)

nzRunHost(fun, output.name = NULL, output.signature = NULL,
  clear.existing = FALSE, case.sensitive = FALSE, output.distributeOn=NULL,...)
}
\arguments{
  \item{x}{Input \code{nz.data.frame}.}
  \item{fun}{A \code{function} object.}
  \item{output.name}{The name of the output table to be created on
    Netezza.}
  \item{output.signature}{The signature for the output table; if not
    provided, a generic sparse table is created.}
  \item{clear.existing}{Determines if the output table should be
    deleted if it currently exits. If \code{TRUE}, the existing output
	table is deleted.}
  \item{case.sensitive}{Denotes if the output columns should be case-sensitive or not.}
  \item{output.distributeOn}{Optional: Column(s) along which to distribute the resulting table}
  \item{\dots}{These arguments will be passed to \code{fun}.}
}
\details{
  The \code{nzRun} function runs the code on the specified
  \code{nz.data.frame}, where it may be run in parallel on the SPUs.
  
  The \code{nzRunHost} function does not take an \code{nz.data.frame}
  as input, and runs the user-supplied function on the Host.

  Below are the most important API functions provided by the R Adapter:
  \itemize{
  \item \code{getNext} prepares the next row for processing
  \item \code{getInputColumn} returns the given column value from the current input row
  \item \code{setOutputNull} sets the given output column to \code{NULL}
  \item \code{setOutput} sets the given output column to the given value
  \item \code{outputResult} outputs one row
  \item \code{log} logs the given message according to \emph{LOGMASK}
    and other \emph{DEBUG}/\emph{TRACE} parameters; the log level
	(parameter \code{level}) can be either \code{1} (\emph{TRACE}) or
	\code{2} (\emph{DEBUG})
  \item \code{inputColumnCount} returns the number of input columns
  \item \code{getRuntime} returns some details on the current transaction, SPU, and so on
  \item \code{userError} prints the error message and ceases any further code execution
  \item \code{getSystemLog} returns the system log file name
  \item \code{getEnv} returns the environment variable specified by its name
  \item \code{getLibraryInfo} returns the list of shared libraries associated with this Analytic Executable
  }

  For further details see the \emph{nzrserver} (server-side R package)
  Manual, which is a part of the \emph{R Adapter cartridge}.
}
\value{
  An object of class \code{nz.data.frame}.
}
\seealso{\code{\link{nzRunHost}},
 \code{\link{nz.data.frame}}, \code{\link{output.signature}}}
\examples{
nzConnectDSN('NZSQL')

d <- nz.data.frame("iris")
f <- function() {
	while(getNext()) {
		setOutput(0,getInputColumn(0))
		outputResult()
	}
}
r <- nzRun(d, f, output.name='apply_output', clear.existing=TRUE,
	output.signature=list(square_root=NZ.DOUBLE))

nzDisconnect()
}
\keyword{manip}
