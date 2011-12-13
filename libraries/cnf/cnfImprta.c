#include "f77.h"                 /* CNF macros and prototypes               */

void cnfImprta( const char *source_f, int source_len, char *dest_c,
                 int dest_len, int ndims, const int *dims )

/*
*+
*  Name:
*     cnfImprta

*  Purpose:
*     Import a FORTRAN CHARACTER array into a C string array.

*  Language:
*     ANSI C

*  Invocation:
*     cnfImprta( source_f, source_len, dest_c, dest_len, ndims, dims )

*  Description:
*     Import a FORTRAN CHARACTER array into a C string array, discarding
*     trailing blanks.
*     The null character is appended to each C string after the last non-blank
*     character copied from the Fortran string if there is room.
*     No more than 'dest_len' characters will be copied for each string.

*  Arguments:
*     const char *source_f (Given)
*        A pointer to the input FORTRAN array.
*     int source_len (Given)
*        The declared maximum number of characters in a element of the
*        FORTRAN array.
*     char *dest_c (Returned via pointer)
*        A pointer to the output C array.
*     int dest_len (Given)
*        The maximum number of characters in an element of the C array
*        (including terminating null if required). This would be the last
*        declared dimension of a char array.
*     int ndims (Given)
*        The number of dimensions of the FORTRAN array.
*     const int *dims (Given)
*        A pointer to an array specifying the dimensions of the FORTRAN array.

*  Notes:
*     -  The C array is treated as an array of strings but it will actually
*        be a char array with one more dimension than the FORTRAN array, the
*        last dimension being dest_len. The other dimensions must be as for
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
   int nels;                     /* Number of elements in the arrays        */
   int el;                       /* element number                          */
   int foffset;                  /* Offset from start to current F element  */
   int coffset;                  /* Offset from start to current C element  */

/* Find the size of the array                                               */
   nels = 1;
   for ( i=0; i<ndims; i++ ) {
       nels *= *(dims+i);
   }

/* Now for each element in the array, copy the FORTRAN string to a C string */

   foffset = 0;
   coffset = 0;
   for( el=0; el<nels; el++){

/* Find the last non blank character in the input FORTRAN string.	    */

      for(
      i = source_len - 1; ( i >= 0 ) && ( *(source_f+foffset+i) == ' ' ); i-- )
      ;

/* Put a null character at the end of the output C string if there's room.  */
/* Otherwise start copying what there is room for.                          */

      if ( i < (dest_len-1) ) {
         *(dest_c+coffset+i+1) = '\0';
      } else {
         i = dest_len-1;
      }

/* Copy the characters from the input FORTRAN string to the output C	    */
/* string.								    */

      for(  ; i >= 0 ; i-- )
         *(dest_c+coffset+i) = *(source_f+foffset+i);

/* Get offsets to next elements                                             */
      foffset += source_len;
      coffset += dest_len;
   }
}

