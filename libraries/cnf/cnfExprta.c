#include "f77.h"                 /* CNF macros and prototypes               */

void cnfExprta( const char *source_c, int source_len, char *dest_f,
                 int dest_len, int ndims, const int *dims )

/*
*+
*  Name:
*     cnfExprta

*  Purpose:
*     Export a C string string array to a FORTRAN CHARACTER array

*  Language:
*     ANSI C

*  Invocation:
*     cnfExprta( source_c, source_len, dest_f, dest_len, ndims, dims )

*  Description:
*     Export a C string array to a FORTRAN CHARACTER array. A null character
*     is assumed to terminate each C string - it will not be copied.
*     If the C string is shorter than the space allocated to the FORTRAN
*     strings, then pad it with blanks.
*     No more than 'dest_len' characters will be copied for each string.

*  Arguments:
*     const char *source_c (Given)
*        A pointer to the input C array.
*     int source_len (Given)
*        The maximum number of characters in a string of the C array
*        (including terminating null if required). This would be the last
*        declared dimension of a char array.
*     char *dest_f (Returned via pointer)
*        A pointer to the output FORTRAN array.
*     int dest_len (Given)
*        The declared maximum number of characters in a element of the
*        FORTRAN array.
*     int ndims (Given)
*        The number of dimensions of the FORTRAN array.
*     int *dims (Given)
*        A pointer to a 1-D array specifying the dimensions of the FORTRAN
*        array.

*  Notes:
*     -  The C array is treated as an array of strings but it will actually
*        be a char array with one more dimension than the FORTRAN array, the
*        last dimension being source_len. The other dimensions must be as for
*        the FORTRAN array.

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
*     29-MAY-1996 (AJC):
*        Original version.
*     24-SEP-1998 (AJC):
*        Specify const char * for input strings
*        and const int * for dimensions
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
   int foffset;                  /* Offset from start to current F element  */
   int coffset;                  /* Offset from start to current C element  */

/* Find the size of the array                                               */
   nels = 1;
   for ( i=0; i<ndims; i++ ) {
       nels *= *(dims+i);
   }

/* Now for each element in the array, copy the C string to a FORTRAN string */
/* taking care not to go beyond the end of the FORTRAN string.		    */
   foffset = 0;
   coffset = 0;
   for( el=0; el<nels; el++){
      for( i = 0 ; (i < dest_len ) && ( *(source_c+coffset+i) != '\0' ) ; i++ )
         *(dest_f+foffset+i) = *(source_c+coffset+i);

/* Fill the rest of the output FORTRAN string with blanks.		    */

      for(  ; i < dest_len ; i++ )
         *(dest_f+foffset+i) = ' ';

/* Get offsets to next elements                                             */
      foffset += dest_len;
      coffset += source_len;
   }
}
