/*+
 *  Name:
 *     gaia::gzip

 *  Purpose:
 *     gzip wrapper functions for GAIA.

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

#ifndef GAIA_GZIP_H
#define GAIA_GZIP_H

namespace gaia {
    void decode_gzip( const char *inbuf, size_t inbufsz, size_t &outbufsz,
                      char *&outbuf );
    void free_gzip( const char *inbuf );
}
#endif
