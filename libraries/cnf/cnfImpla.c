#include "f77.h"                 /* CNF macros and prototypes               */

void cnfImpla( const F77_LOGICAL_TYPE *source_f, int *dest_c,
                int ndims, const int *dims )
/*
*+
*  Name:
*     cnfImpla

*  Purpose:
*     Import a FORTRAN LOGICAL array into a C int array.

*  Language:
*     ANSI C

*  Invocation:
*     cnfImpla( source_f, dest_c, ndims, dims )

*  Description:
*     Import a FORTRAN LOGICAL array into a C int array setting appropriate
*     values for TRUE and FALSE.

*  Arguments:
*     const F77_LOGICAL_TYPE *source_f (Given)
*        A pointer to the input FORTRAN array.
*     int  *dest_c (Returned via pointer)
*        A pointer to the output C array.
*     int ndims (Given)
*        The number of dimensions of the FORTRAN array.
*     const int *dims (Given)
*        A pointer to an array specifying the dimensions of the FORTRAN array.

*  Copyright:
*     Copyright (C) 1996 Council for the Central Laboratory of the Research
*     Councils.

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
*     14-JUN-1996 (AJC):
*        Original version.
*     24-SEP-1998 (AJC):
*        Re-order headings
*        Use arguments as local pointers
*        specify const type * for input array and dimensions
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   int nels;                     /* Number of elements in the arrays        */
   int el;                       /* element number                          */
   F77_LOGICAL_TYPE *foffset;    /* Pointer to current F element  */
   int *coffset;                 /* Pointer to current C element  */

/* Find the size of the array                                               */
   nels = 1;
   for ( i=0; i<ndims; i++ )  nels *= *(dims+i);

/* Now for each element in the array, copy the FORTRAN string to a C string */
   for( el=0; el<nels; el++) *dest_c++ = F77_ISTRUE( *source_f++ );
}

