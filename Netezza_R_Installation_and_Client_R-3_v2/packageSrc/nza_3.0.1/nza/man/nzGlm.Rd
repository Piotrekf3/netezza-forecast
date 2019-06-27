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
\name{nzGlm}
\Rdversion{1.0}
\alias{nzGlm}
\alias{print.nzGlm}
\alias{summary.nzGlm}
\alias{residuals.nzGlm}
\alias{fitted.nzGlm}
\alias{predict.nzGlm}

\title{Wrapper for Generalized Linear Regression (GLM)}
\description{
	This function is a wrapper for the \code{nza..GLM} stored procedure.
	All computations are performed on the NPS, the results are converted to a R object.
	
	Overloaded functions such as \code{print} and \code{summary} can be used for these objects.
	
	A local way to generate a Generalized Linear Model is calling \code{stats::glm}.
}
\usage{
nzGlm(
	form,
	data,
	id="id", 
	intercept = T,
	family=c("bernoulli", "gaussian", "poisson", "binomial", "negativebinomial", "wald", "gamma"),
	family_param = -1,
	link=c("logit", "canbinom", "cangeom", "cannegbinom", "cauchit", "clog", "cloglog", "gaussit", "identity", "inverse", "invnegative", "invsquare", "log", "loglog", "oddspower", "power", "probit", "sqrt"),
	link_param = 1,
	maxit = 20,
	eps = 1e-3,
	tol = 1e-7,
	method=c("irls", "psgd"),
	trials = NULL,
	incolumn = "",
	interaction = "",
	model = NULL, 
	format = c("glm", "raw")
	raw.resid = F,
	dropAfter = F,
	...
)

\method{print}{nzGlm}(object)
\method{summary}{nzGlm}(object)
\method{residuals}{nzGlm}(object)
\method{fitted}{nzGlm}(object)
\method{predict}{nzGlm}(object, newdata, id="id", ...)
}
\arguments{
    \item{form}{A \code{formula} object that describes the GLM to build.}
    \item{data}{A \code{nz.data.frame} object that stores the data to be used for GLM building.}
    \item{id}{The ID column name.}
	\item{family}{The type of error distribution.}
	\item{family_param}{A family-specific parameter.}
	\item{link}{Type of link function.}
	\item{link_param}{Link parameter, 1 by default.}
	\item{maxit}{Maximum number of iterations. 20 by default.}
	\item{eps}{Maximum (relative) error used as a stopping criterion. This should be sufficiently small for the algorithm to work.}
	\item{tol}{The tolerance for the linear equation solver to consider a value equal to be zero. This should be sufficiently small for the algorithm to work.}
	\item{method}{Computing algorithm: either Iteratively Reweighted Least Square (IRLS) or Parallel Stochastic Gradient Descent (PSGD).}
	\item{trials}{The input table column containing the number of trials for the binominal distribution. Ignored unless family is 'binomial'.}
	\item{incolumn}{Overwrite automatic creation of incolumn parameter and specify your own incolumn here.}
	\item{interaction}{Overwrite automatic creation of interaction parameter and specify your own incolumn here.}
	\item{model}{Specify model name. Will be created automatically if not specified.}
	\item{format}{Specify output format. Either \code{"glm"} for output looking like \code{stats::glm} or \code{raw} for downloading all results as \code{data.frame}s.}
	\item{raw.resid}{If \code{format} equals \code{"raw"}, whether to download the residuals or return \code{NULL} instead.}
	\item{dropAfter}{Whether to drop the results after downloading them as specified in \code{format}.}
	\item{object}{A \code{nzGlm} object.}
	\item{newdata}{New data used for prediction as \code{nz.data.frame}.}
	\item{...}{Will be passed to \code{nza..GLM} or \code{nza..PREDICT_GLM} respectively.}
}
\details{
	The function \code{residuals()} will download the 'residuals' table, defined by the \code{residuals} element,
	that every \code{nzGlm()} return value has.
	The function \code{fitted()} will download \code{data} and the residuals table and then compute their difference.
	The function {predict.nzGLM} is an overloaded wrapper for \code{nza..PREDICT_GLM.}.
	
	For more details on the GLM algorithm and requirements to the \code{data}, please refer to
	the documentation of \code{nza..GLM} and in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
}
\value{
	\itemize{
		\item The function \code{nzGlm} returns the generalized linear regression model of classes 
			\code{glm} and \code{nzGlm} if \code{format} equals \code{"glm"} or a \code{list} of
			\code{data.frame}s if \code{format} equals \code{"raw"}.
		\item The functions \code{print} and \code{summary} have no return values.
		\item The function \code{predict} returns a \code{nz.data.frame} that contains the predicted values.
		\item The functions \code{residuals} and \code{fitted} return matrices.
	}
}

\examples{\dontrun{
	library(nza)
	
	# NZSQL - ODBC connection id, has to be defined locally
	# on your system using "ODBC_Source_Administrator"
	# nzConnectDSN('NZSQL')
	
	# Weather Data
	weather.train <- nz.data.frame('WEATHERR')
	glmFit <- nzGlm(TEMPERATURE ~ GRADE + factor(WINDY) + factor(OUTLOOK), weather.train, id="INSTANCE", family="gaussian", link="identity", method='irls')
	
	print(glmFit)
	summary(glmFit)
	
	resid <- residuals(glmFit)
	head(resid)
	
	fit <- fitted(glmFit)
	head(fit)
	
	pred <- predict(glmFit, weather.train, id="INSTANCE")
	head(pred)
	
	# Iris Data
	nziris <- nz.data.frame('IRIS')
	glmFit <- nzGlm(PETALWIDTH ~ SEPALLENGTH * SEPALWIDTH + factor(CLASS), nziris, id="ID", family="gaussian", link="identity", method='irls')
	summary(glmFit)
	
	glmFit <- nzGlm(PETALWIDTH ~ SEPALLENGTH * SEPALWIDTH * PETALLENGTH, nziris, id="ID", family="gamma", link="inverse", method='irls')
	summary(glmFit)
	
	# nzDisconnect()
}}
