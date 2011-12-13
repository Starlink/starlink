#include "f77.h"                 /* CNF macros and prototypes               */

void cnfImpbn( const char *source_f, int source_len, int max, char *dest_c )

/*
*+
*  Name:
*     cnfImpbn

*  Purpose:
*     Import no more than max characters from a FORTRAN string into a C
*     string, retaining trailing blanks

*  Language:
*     ANSI C

*  Invocation:
*     cnfImpbn( source_f, source_len, max, dest_c )

*  Description:
*     Import a FORTRAN string into a C string, up to a maximum of `max'
*     characters, retaining trailing blanks. The null character is
*     appended to the C string after all of the blanks in the input
*     string.

*  Arguments:
*     const char *source_f (Given)
*        A pointer to the input FORTRAN string
*     int source_len (Given)
*        The length of the input FORTRAN string
*     int max (Given)
*        The maximum number of characters to be copied from the input
*        FORTRAN string to the output C string
*     char *dest_c (Returned via pointer)
*        A pointer to the output C string

*  Notes:
*     -  No check is made that there is sufficient space allocated to
*        the C string to hold the FORTRAN string. It is the
*        responsibility of the programmer to check this.

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


/* Find the lesser of the maximum number of characters to be copied and the */
/* length of the input FORTRAN string.					    */

   if( max < source_len )
      i = max;
   else
      i = source_len;

/* Put a null character at the end of the output C string.		    */

   dest_c[i] = '\0';

/* Copy the characters from the input FORTRAN string to the output C	    */
/* string.								    */

   for( i-- ; i >= 0 ; i-- )
      dest_c[i] = source_f[i];
}
