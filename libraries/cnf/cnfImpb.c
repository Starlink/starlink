#include "f77.h"                 /* CNF macros and prototypes               */

void cnfImpb( const char *source_f, int source_len, char *dest_c )

/*
*+
*  Name:
*     cnfImpb

*  Purpose:
*     Import a FORTRAN string into a C string, retaining trailing blanks

*  Language:
*     ANSI C

*  Invocation:
*     cnfImpb( source_f, source_len, dest_c )

*  Description:
*     Import a FORTRAN string into a C string retaining trailing blanks.
*     The null character is appended to the C string after all of the
*     blanks in the input string.

*  Arguments:
*     const char *source_f (Given)
*        A pointer to the input FORTRAN string
*     int source_len (Given)
*        The length of the input FORTRAN string
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


/* Copy the characters of the input FORTRAN string to the output C string.  */

   for( i = 0 ; i < source_len ; i++ )
      dest_c[i] = source_f[i];

/* Add a trailing null character to the output C string.		    */

   dest_c[i] = '\0';
}
