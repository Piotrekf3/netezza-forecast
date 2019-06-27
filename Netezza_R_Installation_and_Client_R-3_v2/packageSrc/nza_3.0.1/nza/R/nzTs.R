#
# Copyright (c) 2011, 2012, Revolution Analytics. All rights reserved.
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

# Rewrite callSP to support ARIMA's pdq e.g. (p<=5,d=2,q<=5)
# literals will be kept unchanged
callSPEx <- function (spname, literals=NULL, ...) {
	nzCheckConnection()
	if (length(grep( "\\.\\.",spname))==0)
		spname = paste("nza..", spname, sep="")
	args = list(...)
	views <- c()
	tmp <- c()
	for (i in 1:length(args)) {
		value <- args[[i]]
		name <- names(args)[i]
		if (is.null(value)) next
		if (is.nz.data.frame(value)) {
			view <- nzCreateView(value)
			tmp[length(tmp) + 1] = paste(name, "=", view, sep = "")
			views <- append(views, view)
		}
		else if (is.character(value) && length(grep(" ", value))) 
			tmp[length(tmp) + 1] = paste(name, "=\"", value, "\"", sep = "")
		else if (length(value) > 1) 
			tmp[length(tmp) + 1] = paste(name, "=\"", paste(value, collapse = " "), "\"", sep = "")
		else 
			tmp[length(tmp) + 1] = paste(name, "=", value, sep = "")
	}
	if (!is.null(literals)) {
		tmp[length(tmp) + 1] = literals
	}
	res <- try(nzQuery("CALL ", spname, "('", paste(tmp, collapse = ","), "')"), silent=T)
	for (view in views) nzDropView(view)
	if(inherits(res, "try-error")) {
		stop(res)
	}
	return(invisible(res))
}

#------------------------------------------------------------------------------

nzTs <- function(data, algorithm, time, target, by=NULL, from=NULL, to=NULL, trend=NULL, seasonality=NULL, pdq=NULL, download=F, model=NULL, dropAfter=F, ...) {
	call <- match.call()
	
	## Getting input table name
	if (class(data) == "character") {
		intable <- data
		tmpView <- nzCreateView(nz.data.frame(data))
	} else if (class(data) == "nz.data.frame") {
		intable <- data@table
		tmpView <- nzCreateView(data)
	} else {
		stop("data must be a table name string or a nz.data.frame object")
	}
	
	if (is.null(model)) {
		model <- nzGetValidModelName(paste(intable, "_model", sep=""))
	} else if (modelExist(model)) {
		nzDropView(tmpView)
		stop("Model name already exists.")
	}
	
	tryCatch({
				if (algorithm == "ARIMA") {
					callSPEx("nza..TIMESERIES", model=model, intable=tmpView, algorithm=algorithm, time=time, target=target, by=by, from=from, to=to, literals=pdq, ...)
				} else if (algorithm == "esmoothing" || algorithm == "exponentialsmoothing") {
					callSP("nza..TIMESERIES", model=model, intable=tmpView, algorithm=algorithm, time=time, target=target, by=by, from=from, to=to, trend=trend, seasonality=seasonality, ...)
				} else if (algorithm == "seasonaltrenddecomposition" || algorithm == "std") {
					callSP("nza..TIMESERIES", model=model, intable=tmpView, algorithm=algorithm, time=time, target=target, by=by, from=from, to=to, ...)
				} else if (algorithm == "spectralanalysis" || algorithm == "spectral") {
					callSP("nza..TIMESERIES", model=model, intable=tmpView, algorithm=algorithm, time=time, target=target, by=by, from=from, to=to, ...)
				}
			}, error = function(e, tmpView) {
				# in case of error, drop view and let user know, what happend
				stop(e)
			}, finally = {
				# drop view
				nzDropView(tmpView)
			}
	)
	
	## Getting output data
	## Time series generic information
	series <- nz.data.frame(paste("INZA.NZA_META_", model, "_SERIES", sep = ""))
	periods <- nz.data.frame(paste("INZA.NZA_META_", model, "_PERIODS", sep = ""))
	interpolated <- nz.data.frame(paste("INZA.NZA_META_", model, "_INTERPOLATED", sep = ""))
	forecast <- nz.data.frame(paste("INZA.NZA_META_", model, "_FORECAST", sep = ""))
	
	if (download) {
		series <- nzr::as.data.frame(series)
		periods <- nzr::as.data.frame(periods)
		interpolated <- nzr::as.data.frame(interpolated)
		forecast<- nzr::as.data.frame(forecast)
	}
	
	## Algorithm specific data
	if (algorithm == "esmoothing" || algorithm == "exponentialsmoothing") {
		seasonalitydetails <- nz.data.frame(paste("INZA.NZA_META_", model, "_SEASONALITYDETAILS", sep = ""))
		expodetails <- nz.data.frame(paste("INZA.NZA_META_", model, "_EXPODETAILS", sep = ""))
		if (download) {
			seasonalitydetails <- nzr::as.data.frame(seasonalitydetails)
			expodetails <- nzr::as.data.frame(expodetails)
		}
		ts.out <- list(
				model = model,
				call = call,
				algorithm = algorithm,
				series = series,
				periods = periods,
				seasonalitydetails = seasonalitydetails,
				interpolated = interpolated,
				forecast = forecast,
				expodetails = expodetails
		)
	} else if (algorithm == "ARIMA") {
		arimadetails <- nz.data.frame(paste("INZA.NZA_META_", model, "_ARIMADETAILS", sep = ""))
		armadetails <- nz.data.frame(paste("INZA.NZA_META_", model, "_ARMADETAILS", sep = ""))
		if (download) {
			arimadetails <- nzr::as.data.frame(arimadetails)
			armadetails <- nzr::as.data.frame(armadetails)
		}
		ts.out <- list(
				model = model,
				call = call,
				algorithm = algorithm,
				series = series,
				periods = periods,
				interpolated = interpolated,
				forecast = forecast,
				arimadetails = arimadetails,
				armadetails = armadetails
		)
	} else if (algorithm == "seasonaltrenddecomposition" || algorithm == "std") {
		stddetails <- nz.data.frame(paste("INZA.NZA_META_", model, "_STDDETAILS", sep = ""))
		if (download) {
			stddetails <- nzr::as.data.frame(stddetails)
		}
		ts.out <- list(
				model = model,
				call = call,
				algorithm = algorithm,
				series = series,
				periods = periods,
				interpolated = interpolated,
				forecast = forecast,
				stddetails = stddetails
		)
	} else if (algorithm == "spectralanalysis" || algorithm == "spectral") {
		ts.out <- list(
				model = model, 
				call = call,
				algorithm = algorithm,
				series = series,
				periods = periods,
				interpolated = interpolated,
				forecast = forecast
		)
	}
	
	# sort results alphabetically (in case of ties, use very next column)
	if (download) {
		ts.out <- sapply(ts.out, function(x) {
					if (is.data.frame(x)) {
						x <- x[do.call(order, x), ]
						rownames(x) <- NULL
						return(x)
					} else {
						return(x)
					}
				}
		)
	}
	
	if (dropAfter) {
		modelDrop(model)
	}
	
	class(ts.out) <- "nzTs"
	return(ts.out)
}

#------------------------------------------------------------------------------

print.nzTs <- function(x, ...) {
	cat("\nCall:\n")
	print(x$call)
	cat("\nModel: ", x$model)
	cat("\n")
	
	if (x$algorithm == "esmoothing" || x$algorithm == "exponentialsmoothing") {
		cat("\nAlgorithm: ", "Exponential smoothing")
		cat("\nOutput:\n")
		expodetails <- nzr::as.data.frame(x$expodetails)
		print(expodetails)
	} else if (x$algorithm == "ARIMA") {
		cat("\nAlgorithm: ", "ARIMA")
		cat("\nOutput:\n")
		arimadetails <- nzr::as.data.frame(x$arimadetails)
		print(arimadetails)
	} else if (x$algorithm == "seasonaltrenddecomposition" || x$algorithm == "std") {
		cat("\nAlgorithm: ", "Seasonal trend decomposition")
		cat("\nOutput:\n")
		stddetails <- nzr::as.data.frame(x$stddetails)
		print(stddetails)
	} else if (x$algorithm == "spectralanalysis" || x$algorithm == "spectral") {
		cat("\nAlgorithm: ", "Spectral analysis")
		cat("\nOutput:\n")
		perioddetails <- nzr::as.data.frame(x$periods)
		print(perioddetails)
	}
}

#------------------------------------------------------------------------------

summary.nzTs <- function(x, ...) {
	print.nzTs(x)
}

#------------------------------------------------------------------------------

# Workaround: there is (by default) no other forecast function.
forecast <- function(x, ...) {
	forecast.nzTs(x, ...)
}

#------------------------------------------------------------------------------

forecast.nzTs <- function(x, ...) {
	nzr::as.data.frame(x$forecast)
}
