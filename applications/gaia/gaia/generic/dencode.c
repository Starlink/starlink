/*+
 *   Name:
 *      dblEncode
 *
 *   Purpose:
 *      Encode a double precision value as a string.
 *
 *   Description:
 *      This routine encodes a double precision value as a string. It
 *      is used instead of the normal Fortran internal write (G24.17)
 *      as this is broken on Linux for numbers with exponents larger
 *      than E+99.
 *
 *   Synopsis:
 *      CALL DENCODE( VALUE, STRING )
 *
 *   Authors:
 *      PWD: Peter W. Draper (Starlink, Durham University)
 *-
 */

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
