#include "star/mem.h"            /* Starlink malloc routines */
#include "f77.h"                 /* CNF macros and prototypes               */

char *cnfCreim( const char *source_f, int source_len )

/*
*+
*  Name:
*     cnfCreim

*  Purpose:
*     Create a temporary C string and import a FORTRAN string into it
*     discarding trailing blanks

*  Language:
*     ANSI C

*  Invocation:
*     pointer = cnfCreim( source_f, source_len )

*  Description:
*     Create a temporary C string, import a FORTRAN string into it and
*     return a pointer to this C string.
*     Any trailing blanks in the FORTRAN string are discarded.
*     The length of the C string that is created is just long enough to
*     hold the FORTRAN string (less trailing blanks), plus the null
*     terminator.

*  Arguments:
*     const char *source_f (Given)
*        A pointer to the input FORTRAN string
*     int source_len (Given)
*        The length of the input FORTRAN string

*  Returned Value:
*     char *cnfCreim
*        A pointer to the storage space allocated by this function.
*        It should be freed after use using "cnfFree" (not "free").

*  Notes:
*     If the routine could not create the space, then it returns a null
*     pointer.

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
*     26-MAR-1991 (PMA):
*        Original version.
*     23-SEP-1998 (AJC):
*        Specify const char * for input strings
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   char *ptr;			 /* Pointer to storage allocated	    */


/* Locate the last non blank character in the input FORTRAN string.	    */

   for( i = source_len - 1 ; ( i >= 0 ) && ( source_f[i] == ' ' ) ; i-- )
      ;

/* Allocate enough space for a copy of the input string.		    */

   ptr = (char *)starMallocAtomic( (size_t)( i + 2 ) );

/* If the space was allocated successfully, copy the input FORTRAN string   */
/* to it.								    */

   if( ptr != 0 )
   {
      ptr[i+1] = '\0';

      for(  ; i >= 0 ; i-- )
         ptr[i] = source_f[i];
   }

   return( ptr );
}
