#include <string.h>
#include "f77.h"                 /* CNF macros and prototypes               */

void cnfExprt( const char *source_c, char *dest_f, int dest_len )

/*
*+
*  Name:
*     cnfExprt

*  Purpose:
*     Export a C string to a FORTRAN string

*  Language:
*     ANSI C

*  Invocation:
*     cnfExprt( source_c, dest_f, dest_len )

*  Description:
*     Export a C string to a FORTRAN string. If the C string is
*     shorter than the space allocated to the FORTRAN string, then pad
*     it with blanks. If the C string is longer than the space
*     allocated to the FORTRAN string, then truncate the string.

*  Arguments:
*     const char *source_c (Given)
*        A pointer to the input C string
*     char *dest_f (Returned via pointer)
*        A pointer to the output FORTRAN string
*     int dest_len (Given)
*        The length of the output FORTRAN string

*  Notes:
*     The input C string and output Fortran string can point
*     to the same string if the intention is to modify the
*     C string in situ.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council
*     Copyright (C) 1998 CCLRC
*     Copyright (C) 2005 Particle Physics and Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-MAR-1991 (PMA):
*        Original version.
*     24-SEP-1998 (AJC):
*        Specify const char * for input strings
*     19-SEP-2005 (TIMJ):
*        Use strncpy and memset
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

   size_t clen;                  /* Input String length                     */
   size_t ncpy;                  /* Number of bytes to copy                 */

/* Copy the characters of the input C string to the output FORTRAN string,  */
/* taking care not to go beyond the end of the FORTRAN string.		    */

   clen = strlen( source_c );
   if (dest_f != source_c)
     strncpy( dest_f, source_c, dest_len );

/* Fill the rest of the output FORTRAN string with blanks.		    */

   if (clen < dest_len) {
     ncpy = dest_len - clen + 1;
     memset( &dest_f[clen], ' ', ncpy );
   }
}
