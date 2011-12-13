/*+
 *  Name:
 *     dblEncode

 *  Purpose:
 *     Encode a double precision value as a string.

 *  Description:
 *     This routine encodes a double precision value as a string. It
 *     is used instead of the normal Fortran internal write (G24.17)
 *     as this is broken on Linux for numbers with exponents larger
 *     than E+99.

 *  Synopsis:
 *     CALL DENCODE( VALUE, STRING )

 *  Authors:
 *     PWD: Peter W. Draper (Starlink, Durham University)

 *  Copyright:
 *     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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

 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include "f77.h"

F77_SUBROUTINE(dencode)( DOUBLE(value), CHARACTER(fstring) TRAIL(fstring) )
{
    GENPTR_DOUBLE(value)
    GENPTR_CHARACTER(line)
    char *cstring;
    cstring = cnfCreat( fstring_length + 1 );
    sprintf( cstring, "%.17g", *value );
    cnfExprt( cstring, fstring, fstring_length );
    cnfFree( cstring );
}
