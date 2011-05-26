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
*
*     If the C string is NULL then the destination string will be filled
*     with blanks.

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
*     Copyright (C) 2005-2006 Particle Physics and Engineering Research Council

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     27-MAR-1991 (PMA):
*        Original version.
*     24-SEP-1998 (AJC):
*        Specify const char * for input strings
*     19-SEP-2005 (TIMJ):
*        Use strncpy and memset
*     03-MAR-2006 (TIMJ):
*        Use memcpy
*     25-09-2006 (PWD):
*        Check for input NULL C pointer.
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
/* And fill the rest of the output FORTRAN string with blanks.		    */

/* First see how long the string actually is                                */
   if ( source_c != NULL && dest_f != NULL ) {
       clen = strlen( source_c );

/* Now decide whether to copy everything and pad, or truncate               */
/* Deliberately duplicate the memmove to minimize if-checks                 */
/* Do not do the memmove if the string pointers are identical               */

       if (clen < (size_t)dest_len) {
           /* Need to pad. So first copy in the correct number of           */
           /* characters.                                                   */
           if (dest_f != source_c) memcpy( dest_f, source_c, clen);
           ncpy = dest_len - clen;
           /* then set remainder to space */
           memset( &dest_f[clen], ' ', ncpy );
       } else {
           /* Fill the output (truncating) */
           if (dest_f != source_c) memcpy(dest_f, source_c, (size_t)dest_len);
       }
   } else {
       /* NULL input string, fill output string with blanks. */
       if ( dest_f != NULL && dest_len > 0 ) {
           memset( dest_f, ' ', dest_len );
       }
   }
}
