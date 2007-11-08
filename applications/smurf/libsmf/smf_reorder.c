/*
*+
*  Name:
*     smf_reorder<x>

*  Purpose:
*     Re-orders hyper-planes in an array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_reorder<x>( <type> *in, int ndim, int *dims, int axis, 
*                     int *index, <type> *out, int *status );
*
*     smf_reorderc( char *in, int len, int ndim, int *dims, int axis, 
*                   int *index, char *out, int *status );

*  Arguments:
*     in = <type> * (Given)
*        Point to the vectorised input array. The elements are assumed to be
*        stored in fortran order (i.e. the first axis varies fastest).
*     len = int (Given)
*        Note, this argument is only included in the interface for
*        smf_reorderc. It is the length of each individual sub-string 
*        within the "in" and "out" arrays.
*     ndim = int (Given)
*        Number of array axes in "in" and "out".
*     dims = int * (Given)
*        Pointer to an array of "ndim" values, each being the length of
*        the corresponding dimension of the "in" and "out" arrays.
*     axis = int (Given)
*        The zero-based index of the dimension that is to be re-ordered.
*        The other dimensions are left unchanged.
*     index = int * (Given)
*        An array with one element for each pixel on the dimension that 
*        is being re-ordered (i.e. it should have "dims[axis]" elements).
*        This array lists the old planes in their sorted order. That is, 
*        if "index[i]" has a value "j", then "j" is an index along
*        dimension "axis" in the "in" array, and "i" is the corresponding 
*        index in the "out" array.
*     out = <type> * (Returned)
*        Point to the vectorised output array. The elements are assumed to be
*        stored in fortran order (i.e. the first axis varies fastest).
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function re-orders the value in a supplied array by shuffling 
*     the hyper-planes that are perpendicular to a specified axis. The
*     new order for the hyper-planes is specified by an "index" array that
*     lists the original hyper-plane indices in their new order. The
*     re-ordered data is written to an output array, leaving the input array
*     unchanged.

*  Notes:
*     - <x> in the function name should be replaced by one of "r", "i",
*     "d" and "c" for processing arrays of float, int, double and char
*     respectively. <type> above should also be changed to one of the
*     "float", "int", "double" or "char".
*     - The smf_reorderc function has an extra parameter "len" giving the
*     length of each sub-string within the array of chars gievn by "in"
*     and "out".

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2007 (DSB):
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

#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

void smf_reorderr( float *in, int ndim, int *dims, int axis, 
                   int *index, float *out, int *status ){

/* Local Variables */
   float *pout;
   int i;
   int plane_size;
   size_t nbyte;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* We can be more efficient if it is the last axis that is being
   re-ordered. */
   if( axis == ndim - 1 ) {

/* If the array is one-dimensional we can be even faster. */
      if( ndim == 1 ) {
         pout = out;
         for( i = 0; i < dims[ 0 ]; i++ ) {
            *(pout++) = in[ index[ i ] ];
         }

/* Now use a general algorithm for arrays with 2 or more axes. */
      } else {

/* Get the number of elements in one hyper-plane. */
         plane_size = 1;
         for( i = 0; i < axis; i++ ) plane_size *= dims[ i ];

/* Get the corresponding number of bytes. */
         nbyte = plane_size*sizeof( float );

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;

/* Loop round every hyper-plane in the output array. */
         for( i = 0; i < dims[ axis ]; i++, pout += plane_size ){

/* Copy the data from input to output. */
            memcpy( pout, in + index[ i ]*plane_size, nbyte );
         }
      }

/* The code for re-ordering axes that are not the last axis is not yet
   implemented. */
   } else {
      *status = SAI__ERROR;
      msgSeti( "AX", axis );
      msgSeti( "M", ndim );
      errRep( "", "SMF_REORDER: Cannot yet re-order axis ^AX in a ^N "
              "dimensional array.", status );
   }
}


void smf_reorderd( double *in, int ndim, int *dims, int axis, 
                   int *index, double *out, int *status ){

/* Local Variables */
   double *pout;
   int i;
   int plane_size;
   size_t nbyte;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* We can be more efficient if it is the last axis that is being
   re-ordered. */
   if( axis == ndim - 1 ) {

/* If the array is one-dimensional we can be faster. */
      if( ndim == 1 ) {
         pout = out;
         for( i = 0; i < dims[ 0 ]; i++ ) {
            *(pout++) = in[ index[ i ] ];
         }

/* Now use a general algorithm for arrays with 2 or more axes. */
      } else {

/* Get the number of elements in one hyper-plane. */
         plane_size = 1;
         for( i = 0; i < axis; i++ ) plane_size *= dims[ i ];

/* Get the corresponding number of bytes. */
         nbyte = plane_size*sizeof( double );

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;

/* Loop round every hyper-plane in the output array. */
         for( i = 0; i < dims[ axis ]; i++, pout += plane_size ){

/* Copy the data from input to output. */
            memcpy( pout, in + index[ i ]*plane_size, nbyte );
         }
      }

/* The code for re-ordering axes that are not the last axis is not yet
   implemented. */
   } else {
      *status = SAI__ERROR;
      msgSeti( "AX", axis );
      msgSeti( "M", ndim );
      errRep( "", "SMF_REORDER: Cannot yet re-order axis ^AX in a ^N "
              "dimensional array.", status );
   }
}


void smf_reorderi( int *in, int ndim, int *dims, int axis, 
                   int *index, int *out, int *status ){

/* Local Variables */
   int *pout;
   int i;
   int plane_size;
   size_t nbyte;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* We can be more efficient if it is the last axis that is being
   re-ordered. */
   if( axis == ndim - 1 ) {

/* If the array is one-dimensional we can be faster. */
      if( ndim == 1 ) {
         pout = out;
         for( i = 0; i < dims[ 0 ]; i++ ) {
            *(pout++) = in[ index[ i ] ];
         }

/* Now use a general algorithm for arrays with 2 or more axes. */
      } else {

/* Get the number of elements in one hyper-plane. */
         plane_size = 1;
         for( i = 0; i < axis; i++ ) plane_size *= dims[ i ];

/* Get the corresponding number of bytes. */
         nbyte = plane_size*sizeof( int );

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;

/* Loop round every hyper-plane in the output array. */
         for( i = 0; i < dims[ axis ]; i++, pout += plane_size ){

/* Copy the data from input to output. */
            memcpy( pout, in + index[ i ]*plane_size, nbyte );
         }
      }

/* The code for re-ordering axes that are not the last axis is not yet
   implemented. */
   } else {
      *status = SAI__ERROR;
      msgSeti( "AX", axis );
      msgSeti( "M", ndim );
      errRep( "", "SMF_REORDER: Cannot yet re-order axis ^AX in a ^N "
              "dimensional array.", status );
   }
}


void smf_reorderc( char *in, int len, int ndim, int *dims, int axis, 
                   int *index, char *out, int *status ){

/* Local Variables */
   char *pout;
   int i;
   int plane_size;
   size_t nbyte;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* We can be more efficient if it is the last axis that is being
   re-ordered. */
   if( axis == ndim - 1 ) {

/* Get the number of elements in one hyper-plane. */
      plane_size = 1;
      for( i = 0; i < axis; i++ ) plane_size *= dims[ i ];

/* Each element is a substring of "len" characters, so multiply the plane
   size by "len". */
      plane_size *= len;

/* Get the corresponding number of bytes. */
      nbyte = plane_size*sizeof( char );

/* Initialise a pointer to the start of the next hyper-plane in the output
   array. */
      pout = out;

/* Loop round every hyper-plane in the output array. */
      for( i = 0; i < dims[ axis ]; i++, pout += plane_size ){

/* Copy the data from input to output. */
         memcpy( pout, in + index[ i ]*plane_size, nbyte );
      }

/* The code for re-ordering axes that are not the last axis is not yet
   implemented. */
   } else {
      *status = SAI__ERROR;
      msgSeti( "AX", axis );
      msgSeti( "M", ndim );
      errRep( "", "SMF_REORDER: Cannot yet re-order axis ^AX in a ^N "
              "dimensional array.", status );
   }
}


