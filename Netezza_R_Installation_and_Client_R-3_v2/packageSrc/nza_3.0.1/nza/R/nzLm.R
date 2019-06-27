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

nzLm <- function(form, nzdf, weights=-1) {
	out = nzDotProduct(form, nzdf, makeMatrix=T, weights=weights)
	mat = out$mat
	
	p = ncol(mat)-1
	n = nrow(nzdf)
	YTY = mat[1,1]
	XTX = mat[-1,-1]
	XTY = mat[1,-1]
	
	#
	# update XTX to invertible matrix
	tmp1 = unlist(c(out$size1, out$size2))
	if (sum(tmp1 > 1)>0) {
		toRem  = cumsum(tmp1)[(tmp1 > 1)[-1]]
		XTX = XTX[-toRem, -toRem]
		XTY = XTY[-toRem]
	}
	
	nz.lm(XTX, XTY, YTY, n, weights=weights)
}

nzRidge <- function(form, nzdf, lambda=10) {
	form = update(form, .~.+1)
	out = nzDotProduct(form, nzdf, makeMatrix=T)
	mat = out$mat
	
	p = ncol(mat)-1
	n = nrow(nzdf)
	tmp = mat[1:p,p+1]/n
	mat = mat[1:p,1:p] - outer(tmp, tmp) * n
	p = ncol(mat)-1
	# from here there is no intercept
	
	YTY = mat[1,1]
	XTX = mat[-1,-1]
	XTY = mat[1,-1]
	
	tmp = nz.lm(XTX+diag(rep(lambda,p),p,p), XTY, YTY, n)
	tmp$RSS = tmp$RSS + lambda * t(tmp$coefficients) %*% tmp$coefficients
	tmp$coefftab[,3:4] = NA
	tmp
}

getBeta <- function(XTX, XTY, nzInvXTX) {
	beta = nzInvXTX %*% XTY
	rownames(beta) = colnames(XTX)
	beta
}

getLogLikelihood <- function(RSS, n, p) {
	-n/2*log(2*pi) - n/2*log(RSS/(n-p)) - (n-p)/2
}

getAIC <- function(L, n, p) {
	-2*L + 2*p
}

getBIC <- function(L, n, p) {
	-2*L + log(n)
}

getR2 <- function(XTX, YTY, beta) {
	(t(beta) %*% XTX %*% beta)/YTY
}

getTest <- function(nzInvXTX, beta,n, p, sigmasq=1) {
	sdbeta =  sqrt(diag(nzInvXTX)*sigmasq)
	tval  = beta/sdbeta
	pval   = 1 - abs(1 - 2*pt(tval, n-p))
	cbind(sdbeta, tval, pval)
}

nz.lm <- function(XTX, XTY, YTY, n, weights=-1) {
	#
	# in current implementation we use (X^T X)^-1 X^T Y equation
	# that's due to compability with ridge
	nzInvXTX = ginv(XTX)
	beta = getBeta(XTX, XTY, nzInvXTX)
	names(beta) = colnames(XTX)
	RSS  = (YTY - 2* t(beta) %*% XTY + t(beta) %*% XTX %*% beta)[1,1]
	ran = sum(abs(eigen(XTX, only.values=T)$values)>10^-8)
	p    = min(ran, n-1)
	likelihood = getLogLikelihood(RSS, n, p)
	AIC  = getAIC(likelihood, n, p)
	BIC  = getBIC(likelihood, n, p)
	tests      = getTest(nzInvXTX, beta, n, p, RSS/(n-p))
	coeftab = data.frame(Estimate = beta, Std.Error = tests[,1], t.value = tests[,2], p.value = tests[,3])
	rownames(coeftab) = names(beta)
	res = list(coefficients = beta, RSS=RSS, effects=NULL, rank = p, df.residuals = n-p, coefftab = coeftab, Loglike = likelihood, AIC=AIC, BIC=BIC)
	class(res) = c("nzLm")
	res
}

print.nzLm <- function(x, ...) {
	cat("\nCoefficients:\n")
	print(x$coefftab)
	cat("\nResidual standard error: ", x$RSS, " on ",x$df.residuals," degrees of freedom\n\n")
	cat("Log-likelihood: ", x$Loglike, "\n")
	cat("AIC: ", x$AIC, "\n")
	cat("BIC: ", x$BIC, "\n")
}
