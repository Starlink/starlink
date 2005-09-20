#include <stdlib.h>		 /* Standard C run-time library		    */
#include "f77.h"                 /* CNF macros and prototypes               */

F77_LOGICAL_TYPE *cnfCrela( int ndims, const int *dims )

/*
*+
*  Name:
*     cnfCrela

*  Purpose:
*     Create a Fortran LOGICAL array and return a pointer to it.

*  Language:
*     ANSI C

*  Invocation:
*     pointer = cnfCrela( ndims, dims )

*  Description:
*     Create a temporary Fortran LOGICAL arry and return a pointer to it.
*     The space allocated is sufficient to contain an array with the given
*     dimensions. The array is not initialised.

*  Arguments:
*     int ndims (Given)
*        The number of dimensions
*     const int *dims (Given)
*        A pointer to the dimensions

*  Returned Value:
*     F77_LOGICAL_TYPE *cnfCrefa
*        A pointer to the storage that has been allocated by this routine.
*        It should be freed after use using "free" (not cnfFree).

*  Notes:
*     -  If the routine could not create the space, then it returns a
*        null pointer.

*  Copyright:
*     Copyright (C) 1996 CCLRC

*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     14-JUN-1996 (AJC):
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
   size = sizeof( F77_LOGICAL_TYPE ); 
   for ( i=0; i<ndims; i++ ) size *= *(dims+i);

/* Allocate the space and return a pointer to it			    */
   return (F77_LOGICAL_TYPE *)malloc( (size_t)( size ) );

}

