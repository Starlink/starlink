#include "star/mem.h"            /* Starlink malloc routines */
#include "f77.h"                 /* CNF macros and prototypes               */

F77_CHARACTER_ARG_TYPE *cnfCref( int length )

/*
*+
*  Name:
*     cnfCref

*  Purpose:
*     Create a Fortran string and return a pointer to it.

*  Language:
*     ANSI C

*  Invocation:
*     pointer = cnfCref( length )

*  Description:
*     Create a temporary Fortran string and return a pointer to it.
*     The space allocated is sufficient to contain a string with
*     "length" characters. The string is not initialised.
*

*  Arguments:
*     int length (Given)
*        The length of the string in characters.

*  Returned Value:
*     F77_CHARACTER_ARG_TYPE *cnfCref
*        A pointer to the storage that has been allocated by this routine.


*  Notes:
*     -  The pointer may not point directly at the string of characters.
*        It is the thing required to be passed with the CHARACTER_ARG
*        macro to a Fortran routine.
*     -  If the routine could not create the space, then it returns a
*        null pointer.
*     -  It should be freed after use using "cnfFreef" (not "free").

*  Copyright:
*     Copyright (C) 1996 CCLRC
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council

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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     12-JAN-1996 (AJC):
*        Original version.
*     20-SEP-2005 (TIMJ):
*        Use starmem.
*     26-SEP-2006 (PWD):
*        Deal with negative length requests as if zero, and return
*        a string of size one.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */
   F77_CHARACTER_ARG_TYPE *ptr;	 /* A pointer to the string allocated       */

/* Allocate the space.							    */
   ptr = (F77_CHARACTER_ARG_TYPE *)
       starMallocAtomic( (size_t)( ( length > 0 ) ? length : 1 ) );

/* Check for malloc returning a null value. If it does not, set the string  */
/* to the null character.						    */
   if ( ptr != 0 ) {
       ptr[0] = '\0';
   }

   return( ptr );
}
