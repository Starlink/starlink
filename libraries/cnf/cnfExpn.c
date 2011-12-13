#include "f77.h"                 /* CNF macros and prototypes               */

void cnfExpn( const char *source_c, int max, char *dest_f, int dest_len )

/*
*+
*  Name:
*     cnfExpn

*  Purpose:
*     Export a C string to a FORTRAN string, copying a given maximum
*     number of characters

*  Language:
*     ANSI C

*  Invocation:
*     cnfExpn( source_c, max, dest_f, dest_len )

*  Description:
*     Export a C string to a FORTRAN string, copying a maximum of `max'
*     characters. If the C string is shorter than the space allocated
*     to the FORTRAN string, then pad it with blanks, even if the
*     whole source string was not copied as it had more than `max'
*     characters. If the C string is longer than the space allocated
*     to the FORTRAN string, then truncate the string.

*  Arguments:
*     const char *source_c (Given)
*        A pointer to the input C string
*     int max (Given)
*        The maximum number of character to be copied from source_c to
*        dest_f
*     char *dest_f (Returned via pointer)
*        A pointer to the FORTRAN output string
*     int dest_len (Given)
*        The length of the FORTRAN output string

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     27-MAR-1991 (PMA):
*        Original version.
*     24-SEP-1998 (AJC):
*        Specify const char * for input strings
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

   int i;			 /* Loop counter			    */


/* Set max to the lesser of the maximum number of characters to be copied   */
/* and the length of the destination FORTRAN string.			    */

   if( max > dest_len )
      max = dest_len;

/* Copy the characters of the input C string to the output FORTRAN string.  */

   for( i = 0 ; (i < max ) && (source_c[i] != '\0' ) ; i++ )
      dest_f[i] = source_c[i];

/* Fill the rest of the output FORTRAN string with blanks.		    */

   for(  ; i < dest_len ; i++ )
      dest_f[i] = ' ';
}
