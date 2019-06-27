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
\name{prerequisites}
\alias{prerequisites}
\title{Prerequisites for the nzr package}
\description{
This section describes a number of server-side steps which must be
performed before using \emph{nzr}.
}
\details{
To run \emph{nzr} correctly you must:
\itemize{
  \item install on Netezza a compatible version of \emph{R Language
    Adapter} (\emph{r_ae}), and its dependencies such as \emph{system_ae}
	and the the R language package, on Netezza.
  \item create the \emph{nzr} database
  \item register \emph{system_ae} in the \emph{nzr} database
  \item register \emph{r_ae} in the \emph{nzr} database
}
These processes are carried out when the R Adapter is installed and
registered via \emph{nzcm}. In the case of any incomprehensible error,
ensure that the R Adapter is registered correctly, and check that the
following required UDXs are, in fact, present in the \emph{nzr} database:
\itemize{
  \item \code{r_udtf}, a table function that returns data in the \emph{sparse}
    mode, that is, as a table containing the \emph{columnid} and \emph{value} columns
  \item \code{r_udtf_any}, a table function that is defined as
    \emph{RETURNS TABLE(ANY)} and requires a shaper, but returns data \dQuote{as-is},
	that is, without any internal transformations/casting
  \item \code{r_uda}, which is an aggregate
  \item \code{r_filter}, a UDF used in \code{nzFilter}
These three depend on libraries provided in \emph{system_ae}.
}
}
