#include "f77.h"                 /* CNF macros and prototypes               */

int cnfLenf( const char *source_f, int source_len )

/*
*+
*  Name:
*     cnfLenf

*  Purpose:
*     Find the length of a FORTRAN string

*  Language:
*     ANSI C

*  Invocation:
*     result = cnfLenf( source_f, source_len )

*  Description:
*     Find the length (i.e. position of the last non blank character)
*     in a FORTRAN string. This is not necessarily the same as the value
*     of source_len as trailing blanks are not counted.

*  Arguments:
*     const char *source_f (Given)
*        A pointer to the input FORTRAN string
*     int source_len (Given)
*        The length (including trailing blanks) of the input FORTRAN
*        string

*  Returned Value:
*     int cnfLenf
*        The length (excluding trailing blanks) of the input FORTRAN
*        string.

*  Notes:
*     -  This routine follows the FORTRAN convention of counting
*        positions from one, so with an input string of "ABCD" the
*        value returned would be 4.

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


/* Find the last non blank character in the input FORTRAN string.	    */

   for( i = source_len - 1 ; ( i >= 0 ) && ( source_f[i] == ' ' ) ; i-- )
      ;

   return( i + 1 );
}
