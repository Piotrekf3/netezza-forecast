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
#ifndef DECODER_H__
#define DECODER_H__

#include <iostream>
#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP decodebase64 (SEXP _str);
SEXP encodebase64 (SEXP _str);
SEXP encodebase64raw (SEXP _str);

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* DECODER_H__ */
