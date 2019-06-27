%
% Copyright (c) 2011, 2012, Revolution Analytics. All rights reserved.
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

\name{nzTs}
\Rdversion{1.0}
\alias{nzTs}
\alias{forecast.nzTs}
\alias{forecast}
\alias{print.nzTs}
\alias{summary.nzTs}

\title{Wrapper for time series (TS) modeling algorithms}
\description{
	This function is a wrapper for the \code{nza..TIMESERIES} stored procedure
	which provides four independent time series modeling algorithms:
	\itemize{
		\item Exponential smoothing
		\item ARIMA
		\item Seasonal trend decomposition
		\item Spectral analysis
	}
	In addition, this function can also provide seasonal behavior of time series.
}

\usage{
nzTs(
	data,
	algorithm,
	time,
	target,
	by=NULL,
	from=NULL,
	to=NULL,
	trend=NULL,
	seasonality=NULL,
	pdq=NULL,
	download=F,
	model=NULL,
	dropAfter=F,
	...
)

\method{print}{nzTs}(object, ...)
\method{summary}{nzTs}(object, ...)
\method{forecast}{nzTs}(object, ...)
}
\arguments{
	\item{data}{Input table name as \code{string} or \code{nz.data.frame} object containing the data}
	\item{algorithm}{The algorithm used, which can be one of the following:
		\itemize{
			\item \code{"exponentialsmoothing"} or \code{"esmoothing"}
			\item \code{"ARIMA"}
			\item \code{"seasonaltrenddecomposition"} or \code{"std"}
			\item \code{"spectralanalysis"} or \code{"spectral"}
		}
	}
	\item{time}{Name of the column that contains the time dimension as numeric values.}
	\item{target}{Name of the column that contains the target data dimension.}
	\item{by}{Name of the data column that identifies the series of values.}
	\item{from}{The value of time to start analysis from. If not specified, start from the first value of the series.}
	\item{to}{The value of time to stop analysis at. If not specified, stop at the last value of the series.}
	\item{trend}{The trend type for \code{algorithm="exponentialsmoothing"} can be one of the following:
		\itemize{
			\item \code{"N"}: None
			\item \code{"A"}: Additive
			\item \code{"DA"}: Damped additive
			\item \code{"M"}: Multiplicative
			\item \code{"DM"}: Damped multiplicative
		}
		If not specified, the trend type is determined by the algorithm.
	}
	\item{seasonality}{The seasonality type for \code{algorithm='exponentialsmoothing'} can be any of the following:
		\itemize{
			\item \code{"N"}: Node
			\item \code{"A"}: Additive
			\item \code{"M"}: Multiplicative
		}
		If not specified, the seasonality type is determined by the algorithm.
	}
	\item{pdq}{ARIMA's p, d and q parameters as a string, e.g 'p<=5,d=2,q<=5'. If not specified, these values will be determined automatically.}
	\item{download}{If \code{TRUE}, all results will be downloaded and returned as \code{data.frame}s instead of \code{nz.data.frame}s.}
	\item{model}{Name for the model. Will be created automatically unless specified otherwise.}
	\item{dropAfter}{Whether to drop the results after fetching them.}
	\item{object}{Another time series object returned from \code{nzTs}.}
	\item{...}{Further arguments, that will be passed to other functions. See details.}
}
\details{
	Please note that case sensitive columnname parameters such as \code{time}, \code{target} or \code{by}
	need to be double-quoted manually before calling \code{nzTs}.

	The function \code{nzTs} will pass \code{...} to the \code{nza..TIMESERIES} procedure.
	All other functions will ignore the vales for \code{...}.
	
	The function \code{forecast} will simply display the \code{forecast} component from \code{object} and not perform
	further computations.
	
	For more details on the TIMESERIES algorithms and requirements to the \code{data}, please refer to
	the documentation of \code{nza..TIMESERIES} in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
}
\value{
	The procedure returns a time series model in terms of pointers to output data tables or the output data tables themselves,
	depending on the \code{download} parameter.	There are output tables as follows:
	\itemize{
		\item series
		\item periods
		\item interpolated
		\item forecast
		\item seasonalitydetails
		\item expodetails
		\item arimadetails
		\item armadetails
		\item stddetails
	}
}

\examples{\dontrun{
	library(nza)

	# NZSQL - ODBC connection id, has to be defined locally
	# on your system using "ODBC_Source_Administrator"
	# nzConnectDSN('NZSQL')
	
	curves <- nz.data.frame('CURVES')
	tsModel <- nzTs(data=curves, algorithm='esmoothing', time='x', target='y', by='curve')
	
	print(tsModel)
	summary(tsModel)
	
	forecast(tsModel)
	
	# nzDisconnect()
}}
