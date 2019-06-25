# 
# Copyright (c) 2010, 2011, IBM Corp. All rights reserved. 
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

nz.placefile <- function (file.name=NULL, data=NULL, output.name=NULL)
{
	if ((is.null(file.name) && is.null(data)) || (!is.null(file.name) && !is.null(data)))
		stop("either file name or data vector has to be provided")

	CHUNK_SIZE = 32768
	
	# a file name is given
	if (!is.null(file.name)) {
		if (!file.exists(file.name))
			stop("file does not exist")
	}
	# data is given
	else {
		file.name = tempfile()
		output  = file(file.name, "wb")
		write(data, output)
		close(output)
	}

	input = file(file.name, 'rb')
	chunk = encodebase64raw(readBin(input, 'raw', n=CHUNK_SIZE))

	if (is.null(output.name)) {
		fname = nzScalarQuery("SELECT filename FROM TABLE WITH FINAL(nzr..placefilebin('placefile','", chunk, "'))")
	} else {
		fname = nzScalarQuery("SELECT filename FROM TABLE WITH FINAL(nzr..placefilebin('createfile','", output.name,
					"','", chunk, "'))")
	}

	while (TRUE) {
		chunk = encodebase64raw(readBin(input, 'raw', n=CHUNK_SIZE))
		if (nchar(chunk) <= 0)
			break;

		nzScalarQuery("SELECT * FROM TABLE WITH FINAL(nzr..placefilebin('appendfile','", fname, "','", chunk, "'))")
	}

	close(input)

	return(fname)
}

