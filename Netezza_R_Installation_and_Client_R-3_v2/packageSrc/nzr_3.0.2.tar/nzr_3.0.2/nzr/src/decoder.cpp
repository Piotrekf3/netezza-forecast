/* 
 * Copyright (c) 2010, 2011, IBM Corp. All rights reserved. 
 * 

 * This program is free software: you can redistribute it and/or modify 
 * it under the terms of the GNU General Public License as published by 
 * the Free Software Foundation, either version 3 of the License, or 
 * (at your option) any later version. 

 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
 * GNU General Public License for more details. 

 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>. 
 * 
 */ 
#include "decoder.h"
#include "base64.hpp"

#include <string>
#include <sstream>
#include <vector>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#define REPORT_ERROR(expr) do {                                    \
		std::stringstream str;                                     \
		str << __FILE__ << ':' << __LINE__ << ' ' << expr << '\n'; \
		Rf_error(str.str().c_str());                               \
		exit(EXIT_FAILURE);                                        \
	} while(0);


SEXP decodebase64 (SEXP _str) {
	if (!IS_CHARACTER(_str) || LENGTH(_str) != 1)
		REPORT_ERROR("expecting 1-element character vector")

	const char * raw = CHAR(STRING_ELT(_str,0));
	const unsigned int rawsize = strlen(raw);
	if (rawsize % 4)
		REPORT_ERROR("input string length has to be a multiple of 4");

	size_t bufsize = rawsize;
	std::vector<char> buff(bufsize);
	char * buf = &buff[0];
	memset(buf, 0, bufsize);
	Base64 base64;
	if (!base64.decode (raw, rawsize, buf, bufsize))
		REPORT_ERROR("could not decode string as base64");
	SEXP ret;
	PROTECT(ret = NEW_CHARACTER(1));
	SET_STRING_ELT(ret, 0, mkCharLen(buf,bufsize));
	UNPROTECT(1);
	return ret;
}

SEXP encodebase64 (SEXP _str) {
	if (!IS_CHARACTER(_str) || LENGTH(_str) != 1)
		REPORT_ERROR("expecting 1-element character vector")

	const char * raw = CHAR(STRING_ELT(_str,0));
	const unsigned int rawsize = strlen(raw);

	size_t nBlocks = ceil((double)rawsize/3);
	size_t bufsize = 4*nBlocks;

	std::vector<char> buff(bufsize);
	char * buf = &buff[0];
	Base64 base64;
	if (!base64.encode (raw, rawsize, buf, bufsize))
		REPORT_ERROR("could not encode string as base64");

	SEXP ret;
	PROTECT(ret = NEW_CHARACTER(1));
	SET_STRING_ELT(ret, 0, mkCharLen(buf,bufsize));
	UNPROTECT(1);
	return ret;
}


SEXP encodebase64raw (SEXP _str) {
	if (!IS_RAW(_str))
		REPORT_ERROR("expecting raw vector");

    const char * raw = (const char*) RAW(_str);
	const unsigned int rawsize = LENGTH(_str);

	size_t nBlocks = ceil((double)rawsize/3);
	size_t bufsize = 4*nBlocks;

	std::vector<char> buff(bufsize);
	char * buf = &buff[0];
	Base64 base64;
	if (!base64.encode (raw, rawsize, buf, bufsize))
		REPORT_ERROR("could not encode string as base64");

	SEXP ret;
	PROTECT(ret = NEW_CHARACTER(1));
	SET_STRING_ELT(ret, 0, mkCharLen(buf,bufsize));
	UNPROTECT(1);
	return ret;
}
