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

#ifndef BASE64_H__
#define BASE64_H__

#include <cstdlib>
#include <cstring>
#include <iostream>

class Base64 {

	// encode-decode arrays
	const char * base64chars;
	char base64inv[256];
	bool base64inv_filled;

public:

	Base64 () {
		base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
		memset(base64inv, 0xFF, 256);
		for (size_t i=0; i<strlen(base64chars); ++i)
			base64inv[(int)base64chars[i]] = (unsigned char)i;
	}


	bool encode (const char * _from, size_t _fromsize, char * _to, size_t & _tosize) {
		if (_tosize * 3 < _fromsize * 4)
			return false;

		size_t i,j;
		unsigned char c1, c2, c3;

		for (i=0, j=0; i<_fromsize; i+=3) {
			c1 = _from[i];
			c2 = i+1 < _fromsize ? _from[i+1] : 0;
			c3 = i+2 < _fromsize ? _from[i+2] : 0;

			_to[j]   = base64chars[c1 >> 2];
			_to[j+1] = base64chars[((c1 & 3) << 4) | (c2 >> 4)];
			_to[j+2] = base64chars[((c2 & 15) << 2) | (c3 >> 6)];
			_to[j+3] = base64chars[c3 & 63];

			if (i+2 >= _fromsize) {
				_to[j+3] = '=';
				if (i+1 >= _fromsize)
					_to[j+2] = '=';
			}
			j += 4;
		}
		_tosize = j;
		return true;
	}

	/**
	 * @brief Decode a string _from of size _fromsize and write the output into
	 * a buffer pointed by _to of size _tosize. If the output buffer size is to
	 * small then do nothing.
	 *
	 * @return Whether the whole input string could be correctly decoded and written
	 * into the output buffer.
	 */
	bool decode (const char * _from, size_t _fromsize, char * _to, size_t & _tosize) {
		if (_fromsize % 4 != 0 || _tosize-1 < _fromsize * 3 / 4) // _tosize-1 because of the trailing zero
			return false;

		size_t padding = 0, i, j;
		if (_fromsize > 0 && (_from[_fromsize-1] == '='))
			++padding;
		if (_fromsize > 1 && (_from[_fromsize-2] == '='))
			++padding;

		for (i=0, j=0; i<_fromsize && j<_tosize; i+=4) {
			char a = base64inv[(unsigned int)_from[i]],
				 b = base64inv[(unsigned int)_from[i+1]],
				 c = base64inv[(unsigned int)_from[i+2]],
				 d = base64inv[(unsigned int)_from[i+3]];

			_to[j++] = ((a & 0x3f) << 2) + ((b & 0x30) >> 4);
			_to[j++] = ((b & 0x0f) << 4) + ((c & 0x3c) >> 2);
			_to[j++] = ((c & 0x03) << 6) + ((d & 0x3f));
		}
		_tosize = j-padding;
		_to[_tosize] = 0;
		return true;
	}

};

#endif /* BASE64_H__ */

