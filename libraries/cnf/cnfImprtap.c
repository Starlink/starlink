#include "f77.h"                 /* CNF macros and prototypes               */

void cnfImprtap( const char *source_f, int source_len, char *const *dest_c,
                  int dest_len, int ndims, const int *dims )

/*
*+
*  Name:
*     cnfImprtap

*  Purpose:
*     Import a FORTRAN CHARACTER array into a C array of pointers to char.

*  Language:
*     ANSI C

*  Invocation:
*     cnfImprtap( source_f, source_len, dest_c, dest_len, ndims, dims )

*  Description:
*     Import a FORTRAN CHARACTER array into a C array of pointers to char,
*     discarding trailing blanks. The pointers must each point to an area of
*     allocated memory at least dest_len characters long.
*     The null character is appended to each C string after the last non-blank
*     character copied from the Fortran string if there is room.
*     No more than 'dest_len' characters will be copied for each string.

*  Arguments:
*     char *source_f (Given)
*        A pointer to the input FORTRAN array.
*     int source_len (Given)
*        The declared maximum number of characters in a element of the
*        FORTRAN array.
*     char *const *dest_c (Returned via pointer)
*        A pointer to the output C array.
*     int dest_len (Given)
*        The maximum number of characters to be copied for each string
*        (including terminating null if required).
*     int ndims (Given)
*        The number of dimensions of the arrays.
*     int *dims (Given)
*        A pointer to an array specifying the dimensions of the arrays.

*  Notes:
*     The array of pointers and the FORTRAN character array must have the
*     same dimensions.

*  Copyright:
*     Copyright (C) 1998 Council for the Central Laboratory of the Research
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
*      4-MAR-1998 (AJC):
*        Original version.
*     24-SEP-1998 (AJC):
*        Specify const char * for input array
*        and char *const * for output aray
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

/* Find the size of the array                                               */
   nels = 1;
   for ( i=0; i<ndims; i++ ) {
       nels *= *(dims+i);
   }

/* Now for each element in the array, copy the FORTRAN string to a C string */

   foffset = 0;
   for( el=0; el<nels; el++){

/* Find the last non blank character in the input FORTRAN string.	    */

      for(
      i = source_len - 1; ( i >= 0 ) && ( *(source_f+foffset+i) == ' ' ); i-- )
      ;

/* Put a null character at the end of the output C string if there's room.  */
/* Otherwise start copying what there is room for.                          */

      if ( i < (dest_len-1) ) {
         *(*(dest_c+el)+i+1) = '\0';
      } else {
         i = dest_len-1;
      }

/* Copy the characters from the input FORTRAN string to the output C	    */
/* string.								    */

      for(  ; i >= 0 ; i-- )
          *(*(dest_c+el)+i)  = *(source_f+foffset+i);

/* Get offsets to next elements                                             */
      foffset += source_len;
   }
}

