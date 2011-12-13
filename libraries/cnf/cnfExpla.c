#include "f77.h"                 /* CNF macros and prototypes               */

void cnfExpla( const int *source_c, F77_LOGICAL_TYPE *dest_f, int ndims,
                const int *dims )
/*
*+
*  Name:
*     cnfExpla

*  Purpose:
*     Export a C int array to a FORTRAN LOGICAL array

*  Language:
*     ANSI C

*  Invocation:
*     cnfExpla( source_c, dest_f, ndims, dims )

*  Description:
*     Export a C int array to a FORTRAN LOGICAL array

*  Arguments:
*     const int *source_c (Given)
*        A pointer to the input C array.
*     F77_LOGICAL_TYPE *dest_f (Returned via pointer)
*        A pointer to the output FORTRAN array.
*     int ndims (Given)
*        The number of dimensions of the FORTRAN array.
*     const int *dims (Given)
*        A pointer to a 1-D array specifying the dimensions of the FORTRAN
*        array.

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
*        Re-order heading
*        Remove declaration of unused local variables
*        Specify const int * for given array and dimensions
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/
{
/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   int nels;                     /* Number of elements                      */
   int el;                       /* Element number                          */

/* Find the size of the array                                               */
   nels = 1;
   for ( i=0; i<ndims; i++ )  nels *= *(dims+i);

/* Now for each element in the C array, set an appropriate value in the
 * Fortran array.
 */
   for( el=0; el<nels; el++)
      *dest_f++ = *source_c++ ? F77_TRUE : F77_FALSE;
}
