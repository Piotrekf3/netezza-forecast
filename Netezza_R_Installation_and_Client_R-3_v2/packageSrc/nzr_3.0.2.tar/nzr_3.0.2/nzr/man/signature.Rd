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
\name{nzSimpleSignature}
\alias{nzSimpleSignature}
\alias{output.signature}
\title{Output signatures}
\description{
  This page describes how an output signature should be created.
}
\usage{
nzSimpleSignature(x)
}
\arguments{
  \item{x}{Input data frame.}
}
\details{
Functions such as \code{nzApply}, \code{nzFilter}, or \code{nzRun} have
a number of working modes, some of which accept an \emph{output signature},
that is, a description of the output table. In the default mode a two-column
table is returned, containing the \emph{column-id} and \emph{value}
columns; it is sometimes called a \emph{sparse table}.
This signature defines the output columns' names and types and can be
used whenever the \emph{sparse} table output is not desired.

An output signature is a list:
\enumerate{
  \item names (tags) define the output table column names
  \item values define the output table column types
}

The available data types are: \code{NZ.FIXED}, \code{NZ.VARIABLE},
\code{NZ.NATIONAL_FIXED}, \code{NZ.NATIONAL_VARIABLE}, \code{NZ.BOOL},
\code{NZ.DATE}, \code{NZ.TIME}, \code{NZ.TIMETZ}, \code{NZ.NUMERIC32},
\code{NZ.NUMERIC64}, \code{NZ.NUMERIC128}, \code{NZ.FLOAT}, \code{NZ.DOUBLE},
\code{NZ.INTERVAL}, \code{NZ.INT8}, \code{NZ.INT16}, \code{NZ.INT32},
\code{NZ.INT64}, \code{NZ.TIMESTAMP}.

Most of the above data types can be specified using \code{name=TYPE}.
However, this does not apply to character and numeric data types,
which must be specified differently:
\enumerate{
  \item character: \code{name=list(TYPE, length)}, where \code{TYPE} can
    be one of: \code{NZ.FIXED}, \code{NZ.VARIABLE}, \code{NZ.NATIONAL_FIXED},
    \code{NZ.NATIONAL_VARIABLE}
  \item numeric: \code{name=list(TYPE, precision, scale)}, where \code{TYPE}
    can be one of: \code{NZ.NUMERIC32}, \code{NZ.NUMERIC64}, \code{NZ.NUMERIC128}
}

Below a number of examples of output signatures are presented:
\enumerate{
  \item \code{list(x=NZ.DOUBLE, y=NZ.INT32)}
  \item \code{list(name=list(NZ.VARIABLE, 64), age=NZ.INT32)}
  \item \code{list(type=list(NZ.FIXED, 10), amount=list(NZ.NUMERIC64, 10, 4))}
}


The \code{nzSimpleSignature} function can be used to create the output
signature for a given \code{nz.data.frame}. The \code{nzApply},
\code{nzFilter}, and \code{nzRun} functions should accept the
\code{nz.data.frame} object as the \code{output.signature} and pass it
internally to the \code{nzSimpleSignature} function to determine the
actual signature.

For all string input columns, the \code{nzSimpleSignature} function
returns a \code{NZ.VARIABLE} column of size 256.

The data types identifiers enumerated above are created during the RODBC
connection initialization. When a connection is successfully established,
the connection function, either \code{nzConnect} or \code{nzConnectDSN},
calls an internal function, \code{setupNpsDataTypes}, which in turn calls
the \code{getNpsDataTypes} server-side function. The output value of the
\code{getNpsDataTypes} function is returned into the client session and
assigned in the global environment. This allows the client package to be
free of hard-coded identifier values and assures that any values are
up-to-date with the server software.
}
\value{
  A \code{list} defining the output signature.
}
\seealso{\code{\link{nzRun}}, \code{\link{nzApply}}, \code{\link{nz.data.frame}}}
\examples{
#nzConnectDSN('NZSQL')
d <- as.nz.data.frame(iris, table="iris2", clear.existing=TRUE,case.sensitive=T)

nzSimpleSignature(d)

# The last column of the nzTApply output
# is always the group identifier value.
nzTApply(d, 'Species', function(x) { return(mean(x[,1:4])) },
  output.signature=list(x=NZ.DOUBLE,y=NZ.DOUBLE,z=NZ.DOUBLE,
                        v=NZ.DOUBLE,g=list(NZ.VARIABLE,10)))

#nzDisconnect()
}
