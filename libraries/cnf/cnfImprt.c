#include "f77.h"                 /* CNF macros and prototypes               */
#include <stdlib.h>
#include <string.h>

void cnfImprt( const char *source_f, int source_len, char *dest_c )

/*
*+
*  Name:
*     cnfImprt

*  Purpose:
*     Import a FORTRAN string into a C string

*  Language:
*     ANSI C

*  Invocation:
*     cnfImprt( source_f, source_len, dest_c )

*  Description:
*     Import a FORTRAN string into a C string, discarding trailing
*     blanks. The NUL character is appended to the C string after
*     the last non-blank character. The input string and output string
*     pointers can point to the same location if the string is to be
*     modified in place (but care must be taken to allow for the additional
*     C terminator when allocating memory).

*  Arguments:
*     const char *source_f (Given)
*        A pointer to the input FORTRAN string
*     int source_len (Given)
*        The length of the input FORTRAN string
*     char *dest_c (Returned via pointer)
*        A pointer to the output C string. Can be same as source.

*  Notes:
*     -  No check is made that there is sufficient space allocated to
*        the C string to hold the FORTRAN string and a terminating null.
*        It is responsibility of the programmer to check this.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     27-MAR-1991 (PMA):
*        Original version.
*     22-MAY-1996 (AJC):
*        Correct description re trailing blanks
*     24-SEP-1998 (AJC):
*        Specify const char * for input strings
*     25-NOV-2005 (TIMJ):
*        Allow the strings to be identical
*     26-SEP-2006 (PWD):
*        Ignore NULL output pointer. Previously just crashed.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

   int i;			 /* Loop counter			    */


/* Find the last non blank character in the input FORTRAN string.	    */

   for( i = source_len - 1 ; ( i >= 0 ) && ( source_f[i] == ' ' ) ; i-- )
      ;

/* Put a null character at the end of the output C string.		    */
   if ( dest_c ) {
      dest_c[i+1] = '\0';

/* Copy the characters from the input FORTRAN string to the output C	    */
/* string if the strings are different.				      	    */

      if ( dest_c != source_f ) {
         memmove( dest_c, source_f, i+1 );
      }
   }
}

