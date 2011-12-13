#include "f77.h"                 /* CNF macros and prototypes               */

int cnfLenc( const char *source_c )

/*
*+
*  Name:
*     cnfLenc

*  Purpose:
*     Find the length of a C string

*  Language:
*     ANSI C

*  Invocation:
*     result = cnfLenc( source_c )

*  Description:
*     Find the length (i.e. position of the last non blank character)
*     in a C string.

*  Arguments:
*     const char *source_c (Given)
*        A pointer to the input C string

*  Returned Value:
*     int cnfLenc
*        The length of the input C string

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


/* Find the position of the null character in the input C string.	    */

   for( i = 0 ; source_c[i] != '\0' ; i++ )
      ;

/* Find the position of the last non blank character in the input C string. */

   for( i-- ; ( i >= 0 ) && ( source_c[i] == ' ' ) ; i-- )
      ;

   return( i + 1 );
}

