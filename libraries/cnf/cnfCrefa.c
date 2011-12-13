#include "star/mem.h"            /* Starlink malloc routines */
#include "f77.h"                 /* CNF macros and prototypes               */

F77_CHARACTER_ARG_TYPE *cnfCrefa( int length, int ndims, const int *dims )

/*
*+
*  Name:
*     cnfCrefa

*  Purpose:
*     Create a Fortran CHARACTER array and return a pointer to it.

*  Language:
*     ANSI C

*  Invocation:
*     pointer = cnfCrefa( length, ndims, dims )

*  Description:
*     Create a temporary Fortran CHARACTER arry and return a pointer to it.
*     The space allocated is sufficient to contain an array with the given
*     dimensions, of strings "length" characters long. The strings are not
*     initialised.
*

*  Arguments:
*     int length (Given)
*        The maximum length of the strings in characters.
*     int ndims (Given)
*        The number of dimensions
*     const int *dims (Given)
*        A pointer to the dimensions

*  Returned Value:
*     F77_CHARACTER_ARG_TYPE *cnfCrefa
*        A pointer to the storage that has been allocated by this routine.

*  Notes:
*     -  The pointer may not point directly at the string of characters.
*        It is the thing required to be passed with the CHARACTER_ARRAY_ARG
*        macro to a Fortran routine.
*     -  If the routine could not create the space, then it returns a
*        null pointer.
*     -  It should be freed after use using "cnfFreef" (not "free").

*  Copyright:
*     Copyright (C) 1996 CCLRC

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
*     {enter_new_authors_here}

*  History:
*     22-MAY-1996 (AJC):
*        Original version.
*     24-SEP-1998 (AJC):
*        Specify const int * for dimensions
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */
   int size;         /* Space required */
   int i;            /* Loop counter */

/* Find the space required                                                  */
   size = length;
   for ( i=0; i<ndims; i++ ) size *= *(dims+i);

/* Allocate the space and return a pointer to it			    */
   return (F77_CHARACTER_ARG_TYPE *)starMallocAtomic( (size_t)( size ) );

}

