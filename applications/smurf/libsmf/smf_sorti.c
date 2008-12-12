/*
*+
*  Name:
*     smf_sorti

*  Purpose:
*     Create a sorted index for an array of int values.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     index = smf_sorti( size_t nel, int *array, int *sorted, 
*                        int *status );

*  Arguments:
*     nel = size_t (Given)
*        Number of elements in "array"
*     array = int * (Given)
*        Pointer to an array holding the values that are to be index.
*     sorted = int * (Returned)
*        Indicates whether "array" was sorted on entry or not:
*           0 = the supplied "array" values were not monotonic
*           1 = the supplied "array" values were monotonic increasing
*           -1 = the supplied "array" values were monotonic decreasing
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Returned Value:
*     A pointer to a dynamically allocated array of "nel" integers. The
*     first element will hold the index within "array" of the smallest (most
*     negative) array value. The second element will hold the index within 
*     "array" of the second smallest array value, etc. This array should
*     be freed using astFree when no longer needed.

*  Description:
*     Returns an array of integer indices that access the supplied array
*     in monotonic increasing order (except for the possibility that
*     adjacent elements may have equal values).

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     7-NOV-2007 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "smf.h"

int *smf_sorti( size_t nel, int *array, int *sorted, int *status ){

/* Local Variables */
   int *dp = NULL;
   int *dq = NULL;
   int *p = NULL;
   int *q = NULL;
   int *r = NULL;
   int *result = NULL;
   int done;
   size_t i;
   int temp;

/* Check the inherited status. */
   if( *status != SAI__OK ) return NULL;

/* Determine if the initial array is sorted. */
   *sorted = ( array[ 1 ] > array[ 0 ] ) ? 1 : -1;
   dp = array + 1;
   dq = array + 2;

   for( i = 2; i < nel; i++,dp++,dq++ ) {
      if( *dp > *dq ) {
         if( *sorted == 1 ) {
            *sorted = 0;
            break;
         }      
      } else {
         if( *sorted == -1 ) {
            *sorted = 0;
            break;
         }      
      }
   }

/* Allocate the returned array. */
   result = astMalloc( sizeof( *result )*nel );
   if( result ) {

/* If the array is already sorted decreasing, set up a suitable index to
   swap the order to monotonic increasing. */
      if( *sorted == -1 ) {
         r = result + nel - 1;
         for( i = 0; i < nel; i++ ) *(r--) = i;

/* Otherwise, set up a unit index */
      } else {
         r = result;
         for( i = 0; i < nel; i++ ) *(r++) = i;

/* Leave this unit mapping unchanged if the array is already sorted
   increasing. */
         if( *sorted == 0 ) {

/* Arrive here if the index needs sorting. Use a bubble sort algorithm. */
            r = result + nel - 1;
            done = 0;
            while( ! done ) {
               done = 1;                   
               q = result + 1;
               for( p = result; p < r; p++, q++ ) {
                  if( array[ *p ] > array[ *q ] ) {
                     temp = *p;
                     *p = *q;
                     *q = temp;
                     done = 0;    
                  }   
               }
               r--;
            }
         }
      }
   }

   return result;

}
