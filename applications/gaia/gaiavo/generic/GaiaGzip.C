/*+
 *  Name:
 *     gaia::gzip

 *  Purpose:
 *     GZIP utility functions for GAIA.

 *  Description:
 *     Wrappers to Skycat's gzip functions.

 *  Language:
 *     C

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     20-JUN-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DCompress.h"

#include "GaiaGzip.h"

namespace gaia {

    /**
     *  Decompress a character buffer.
     *
     *  inbuf    - the compressed buffer.
     *  inbufsz  - size of the compressed buffer.
     *  outbufsz - suggested initial size for the output buffer,
     *             set to inbufsz if 0.
     *  outbuf   - the decompressed buffer, use free_gzip to release it.
     */
    void decode_gzip( const char *inbuf, size_t inbufsz, size_t &outbufsz,
                      char *&outbuf )
    {
        Compress compressor;
        int insize = (int) inbufsz;
        int outsize = (int) outbufsz;

        /*  Allocate the initial quantity of memory suggested. */
        outbuf = (char *) malloc( outbufsz ? outbufsz : inbufsz );
        compressor.decompress( inbuf, insize, outbuf, outsize,
                               Compress::GZIP_COMPRESS );
        outbufsz = (size_t) outsize;
    }

    /**
     *  Free a buffer allocated by functions in this module.
     */
    void free_gzip( const char *inbuf )
    {
        free( (void *) inbuf );
    }


}
