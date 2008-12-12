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
*     smf_reorder( type, void *in, int len, int ndim, int *dims_in, 
*                  int axis, int *index, int maxis, int *mask, void *out, 
*                  int *status );
*
*     smf_reorder<x>( <type> *in, int ndim, int *dims_in, int axis, 
*                     int *index, int maxis, int *mask, <type> *out, 
*                     int *status );
*
*     smf_reorderc( char *in, int len, int ndim, int *dims_in, int axis, 
*                   int *index, int maxis, int *mask, char *out, 
*                   int *status );

*  Arguments:
*     type = const char * (Given)
*        The HDS data type. Note, this argument is only included in the 
*        interface for the generic "smf_reorder" function.
*     in = <type> * (Given)
*        Point to the vectorised input array. The elements are assumed to be
*        stored in fortran order (i.e. the first axis varies fastest).
*     len = int (Given)
*        The length of each individual sub-string within the "in" and "out" 
*        arrays. Note, this argument is only included in the interface for
*        smf_reorder and smf_reorderc. The smf_reorder function ignores
*        the supplied value if "type" indicates a non-character data type.
*     ndim = int (Given)
*        Number of array axes in "in" and "out".
*     dims_in = int * (Given)
*        Pointer to an array of "ndim" values, each being the length of
*        the corresponding dimension of the "in" array. The dimensions of
*        the "out" array should be the same as those of the "in" array, 
*        except that, if a mask is supplied, the "maxis" axis should be
*        shorter in the "out" array, by the number of zero values in the 
*        mask.
*     axis = int (Given)
*        The zero-based index of the dimension that is to be re-ordered.
*        Ignored if "index" is NULL.
*     index = int * (Given)
*        An array with one element for each input pixel on the dimension that 
*        is being re-ordered (i.e. it should have "dims_in[axis]" elements).
*        This array lists the old planes in their sorted order. That is, 
*        if "index[i]" has a value "j", then "j" is an index along
*        dimension "axis" in the "in" array, and "i" is the corresponding 
*        index in the "out" array. A NULL pointer may be supplied, in
*        which case the data is copied without re-ordering.
*     maxis = int (Given)
*        The zero-based index of the dimension that is to be masked. Only
*        the last axis (if no index is supplied), or last but one axis
*        (if an axis was supplied) can be masked. Ignored if "mask" is NULL.
*     mask = int * (Given)
*        An array with one element for each input pixel on the dimension that 
*        is being masked (i.e. it should have "dims_in[maxis]" elements). This 
*        array should contain non-zero values for those hyper-rows that
*        are to be copied to "out". Other hyper-rows are not copied. If a
*        NULL pointer is supplied, no masking is performed. The number of
*        non-zero values in "mask" should equal the length of the "maxis" 
*        axis in "out".
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
*
*     In addition, another axis can be masked, so that only selected
*     values on that axis are copied form input to output.

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
*     31-MAR-2008 (DSB):
*        Added "mask" and "maxis" arguments.
*     3-APR-2008 (DSB):
*        Added generic smf_reorder function.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008 Science & Technology Facilities Council.
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
#include "smf.h"

void smf_reorder( const char *type, void *in, int len, int ndim, int *dims_in, 
                  int axis, int *index, int maxis, int *mask, void *out, 
                  int *status ){

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Call the correct function for the specified data type. */
   if( !strcmp( type, "_REAL" ) ) {
      smf_reorderr( (float *) in, ndim, dims_in, axis, index, maxis,
                    mask, (float *) out, status );

   } else if( !strcmp( type, "_DOUBLE" ) ) {
      smf_reorderd( (double *) in, ndim, dims_in, axis, index, maxis,
                    mask, (double *) out, status );

   } else if( !strcmp( type, "_INTEGER" ) ) {
      smf_reorderi( (int *) in, ndim, dims_in, axis, index, maxis,
                    mask, (int *) out, status );

   } else if( !strncmp( type, "_CHAR", 5 ) ) {
      smf_reorderc( (char *) in, len, ndim, dims_in, axis, index, maxis,
                    mask, (char *) out, status );

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetc( "TYPE", type );
      errRep( "", "SMF_REORDER: ^TYPE data type not yet supported.", status );
   }
}



void smf_reorderr( float *in, int ndim, int *dims_in, int axis, int *index, 
                   int maxis, int *mask, float *out, int *status ){

/* Local Variables */
   float *pin;
   float *pout;
   int array_size;
   int i;
   int j;
   int plane_size;
   int row_size = 0;
   size_t array_nbyte;
   size_t plane_nbyte;
   size_t row_nbyte = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If an index has been supplied, ensure it refers to the last axis. */
   if( index ) {
      if( axis != ndim - 1 ) {
         *status = SAI__ERROR;
         msgSeti( "AX", axis );
         msgSeti( "M", ndim );
         errRep( "", "SMF_REORDER: Cannot yet re-order axis ^AX in a ^N "
                 "dimensional array.", status );
      }
   }

/* If a mask has been supplied, ensure it refers to the last axis but one
   axis (if an index was supplid), or the last axis (if no index was
   supplied). */
   if( mask ) {
      if( ( index && maxis != ndim - 2 ) || ( !index && maxis != ndim - 1 ) ) {
         *status = SAI__ERROR;
         msgSeti( "AX", maxis );
         msgSeti( "M", ndim );
         errRep( "", "SMF_REORDER: Cannot yet mask axis ^AX in a ^N "
                 "dimensional array.", status );
      }
   }

/* Check the axes are OK. */
   if( *status == SAI__OK ) {

/* If the array is one-dimensional we can be faster. */
      if( ndim == 1 ) {
         pout = out;
         if( index ) {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               *(pout++) = in[ index[ i ] ];
            }

         } else if( mask ) {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               if( mask[ i ] ) *(pout++) = in[ i ];
            }

         } else {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               *(pout++) = in[ i ];
            }
         }                    

/* Now use a general algorithm for arrays with 2 or more axes. First deal 
   with cases where an index was supplied. */
      } else if( index ){

/* Get the number of elements in one hyper-plane, and get the corresponding 
   number of bytes. */
         plane_size = 1;
         for( i = 0; i < axis; i++ ) plane_size *= dims_in[ i ];
         plane_nbyte = plane_size*sizeof( float );

/* If a mask was supplied, get the number of elements in one hyper-row, and 
   get the corresponding number of bytes. */
         if( mask ) {
            row_size = 1;
            for( i = 0; i < maxis; i++ ) row_size *= dims_in[ i ];
            row_nbyte = row_size*sizeof( float );
         } 

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;

/* Loop round every hyper-plane in the input array. */
         for( i = 0; i < dims_in[ axis ]; i++ ){
            pin = in + index[ i ]*plane_size;

/* If masking, copy the hyper-plane from input to output in sections of
   one hyper-row. */
            if( mask ) {
               for( j = 0; j < dims_in[ maxis ]; j++, pin += row_size ){
                  if( mask[ j ] ) {
                     memcpy( pout, pin, row_nbyte );
                     pout += row_size;
                  }
               }   

/* If not masking, copy the whole hyper-plane from input to output. */
            } else {
               memcpy( pout, pin, plane_nbyte );
               pout += plane_size;
            }
         }

/* Now deal with cases where no index was supplied but a mask was supplied. */
      } else if( mask ){

/* Get the number of elements in one hyper-row, and get the corresponding 
   number of bytes. */
         row_size = 1;
         for( i = 0; i < maxis; i++ ) row_size *= dims_in[ i ];
         row_nbyte = row_size*sizeof( float );

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;
         pin = in;
         for( j = 0; j < dims_in[ maxis ]; j++, pin += row_size ){
            if( mask[ j ] ) {
               memcpy( pout, pin, row_nbyte );
               pout += row_size;
            }
         }   

/* Now deal with cases where no index or mask was supplied. */
      } else {

/* Get the total number of elements in an arrays. */
         array_size = 1;
         for( i = 0; i < ndim; i++ ) array_size *= dims_in[ i ];
         array_nbyte = array_size*sizeof( float );

/* Copy the array. */
         memcpy( out, in, array_nbyte );
      }
   }
}


void smf_reorderd( double *in, int ndim, int *dims_in, int axis, int *index, 
                   int maxis, int *mask, double *out, int *status ){

/* Local Variables */
   double *pin;
   double *pout;
   int array_size;
   int i;
   int j;
   int plane_size;
   int row_size = 0;
   size_t array_nbyte;
   size_t plane_nbyte;
   size_t row_nbyte = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If an index has been supplied, ensure it refers to the last axis. */
   if( index ) {
      if( axis != ndim - 1 ) {
         *status = SAI__ERROR;
         msgSeti( "AX", axis );
         msgSeti( "M", ndim );
         errRep( "", "SMF_REORDER: Cannot yet re-order axis ^AX in a ^N "
                 "dimensional array.", status );
      }
   }

/* If a mask has been supplied, ensure it refers to the last axis but one
   axis (if an index was supplid), or the last axis (if no index was
   supplied). */
   if( mask ) {
      if( ( index && maxis != ndim - 2 ) || ( !index && maxis != ndim - 1 ) ) {
         *status = SAI__ERROR;
         msgSeti( "AX", maxis );
         msgSeti( "M", ndim );
         errRep( "", "SMF_REORDER: Cannot yet mask axis ^AX in a ^N "
                 "dimensional array.", status );
      }
   }

/* Check the axes are OK. */
   if( *status == SAI__OK ) {

/* If the array is one-dimensional we can be faster. */
      if( ndim == 1 ) {
         pout = out;
         if( index ) {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               *(pout++) = in[ index[ i ] ];
            }

         } else if( mask ) {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               if( mask[ i ] ) *(pout++) = in[ i ];
            }

         } else {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               *(pout++) = in[ i ];
            }
         }                    

/* Now use a general algorithm for arrays with 2 or more axes. First deal 
   with cases where an index was supplied. */
      } else if( index ){

/* Get the number of elements in one hyper-plane, and get the corresponding 
   number of bytes. */
         plane_size = 1;
         for( i = 0; i < axis; i++ ) plane_size *= dims_in[ i ];
         plane_nbyte = plane_size*sizeof( double );

/* If a mask was supplied, get the number of elements in one hyper-row, and 
   get the corresponding number of bytes. */
         if( mask ) {
            row_size = 1;
            for( i = 0; i < maxis; i++ ) row_size *= dims_in[ i ];
            row_nbyte = row_size*sizeof( double );
         } 

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;

/* Loop round every hyper-plane in the input array. */
         for( i = 0; i < dims_in[ axis ]; i++ ){
            pin = in + index[ i ]*plane_size;

/* If masking, copy the hyper-plane from input to output in sections of
   one hyper-row. */
            if( mask ) {
               for( j = 0; j < dims_in[ maxis ]; j++, pin += row_size ){
                  if( mask[ j ] ) {
                     memcpy( pout, pin, row_nbyte );
                     pout += row_size;
                  }
               }   

/* If not masking, copy the whole hyper-plane from input to output. */
            } else {
               memcpy( pout, pin, plane_nbyte );
               pout += plane_size;
            }
         }

/* Now deal with cases where no index was supplied but a mask was supplied. */
      } else if( mask ){

/* Get the number of elements in one hyper-row, and get the corresponding 
   number of bytes. */
         row_size = 1;
         for( i = 0; i < maxis; i++ ) row_size *= dims_in[ i ];
         row_nbyte = row_size*sizeof( double );

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;
         pin = in;
         for( j = 0; j < dims_in[ maxis ]; j++, pin += row_size ){
            if( mask[ j ] ) {
               memcpy( pout, pin, row_nbyte );
               pout += row_size;
            }
         }   

/* Now deal with cases where no index or mask was supplied. */
      } else {

/* Get the total number of elements in an arrays. */
         array_size = 1;
         for( i = 0; i < ndim; i++ ) array_size *= dims_in[ i ];
         array_nbyte = array_size*sizeof( double );

/* Copy the array. */
         memcpy( out, in, array_nbyte );
      }
   }
}

void smf_reorderi( int *in, int ndim, int *dims_in, int axis, int *index, 
                   int maxis, int *mask, int *out, int *status ){

/* Local Variables */
   int *pin;
   int *pout;
   int array_size;
   int i;
   int j;
   int plane_size;
   int row_size = 0;
   size_t array_nbyte;
   size_t plane_nbyte;
   size_t row_nbyte = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If an index has been supplied, ensure it refers to the last axis. */
   if( index ) {
      if( axis != ndim - 1 ) {
         *status = SAI__ERROR;
         msgSeti( "AX", axis );
         msgSeti( "M", ndim );
         errRep( "", "SMF_REORDER: Cannot yet re-order axis ^AX in a ^N "
                 "dimensional array.", status );
      }
   }

/* If a mask has been supplied, ensure it refers to the last axis but one
   axis (if an index was supplid), or the last axis (if no index was
   supplied). */
   if( mask ) {
      if( ( index && maxis != ndim - 2 ) || ( !index && maxis != ndim - 1 ) ) {
         *status = SAI__ERROR;
         msgSeti( "AX", maxis );
         msgSeti( "M", ndim );
         errRep( "", "SMF_REORDER: Cannot yet mask axis ^AX in a ^N "
                 "dimensional array.", status );
      }
   }

/* Check the axes are OK. */
   if( *status == SAI__OK ) {

/* If the array is one-dimensional we can be faster. */
      if( ndim == 1 ) {
         pout = out;
         if( index ) {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               *(pout++) = in[ index[ i ] ];
            }

         } else if( mask ) {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               if( mask[ i ] ) *(pout++) = in[ i ];
            }

         } else {
            for( i = 0; i < dims_in[ 0 ]; i++ ) {
               *(pout++) = in[ i ];
            }
         }                    

/* Now use a general algorithm for arrays with 2 or more axes. First deal 
   with cases where an index was supplied. */
      } else if( index ){

/* Get the number of elements in one hyper-plane, and get the corresponding 
   number of bytes. */
         plane_size = 1;
         for( i = 0; i < axis; i++ ) plane_size *= dims_in[ i ];
         plane_nbyte = plane_size*sizeof( int );

/* If a mask was supplied, get the number of elements in one hyper-row, and 
   get the corresponding number of bytes. */
         if( mask ) {
            row_size = 1;
            for( i = 0; i < maxis; i++ ) row_size *= dims_in[ i ];
            row_nbyte = row_size*sizeof( int );
         } 

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;

/* Loop round every hyper-plane in the input array. */
         for( i = 0; i < dims_in[ axis ]; i++ ){
            pin = in + index[ i ]*plane_size;

/* If masking, copy the hyper-plane from input to output in sections of
   one hyper-row. */
            if( mask ) {
               for( j = 0; j < dims_in[ maxis ]; j++, pin += row_size ){
                  if( mask[ j ] ) {
                     memcpy( pout, pin, row_nbyte );
                     pout += row_size;
                  }
               }   

/* If not masking, copy the whole hyper-plane from input to output. */
            } else {
               memcpy( pout, pin, plane_nbyte );
               pout += plane_size;
            }
         }

/* Now deal with cases where no index was supplied but a mask was supplied. */
      } else if( mask ){

/* Get the number of elements in one hyper-row, and get the corresponding 
   number of bytes. */
         row_size = 1;
         for( i = 0; i < maxis; i++ ) row_size *= dims_in[ i ];
         row_nbyte = row_size*sizeof( int );

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;
         pin = in;
         for( j = 0; j < dims_in[ maxis ]; j++, pin += row_size ){
            if( mask[ j ] ) {
               memcpy( pout, pin, row_nbyte );
               pout += row_size;
            }
         }   

/* Now deal with cases where no index or mask was supplied. */
      } else {

/* Get the total number of elements in an arrays. */
         array_size = 1;
         for( i = 0; i < ndim; i++ ) array_size *= dims_in[ i ];
         array_nbyte = array_size*sizeof( int );

/* Copy the array. */
         memcpy( out, in, array_nbyte );
      }
   }
}


void smf_reorderc( char *in, int len, int ndim, int *dims_in, int axis, int *index, 
                   int maxis, int *mask, char *out, int *status ){

/* Local Variables */
   char *pin;
   char *pout;
   int array_size;
   int i;
   int j;
   int plane_size;
   int row_size = 0;
   size_t array_nbyte;
   size_t plane_nbyte;
   size_t row_nbyte = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If an index has been supplied, ensure it refers to the last axis. */
   if( index ) {
      if( axis != ndim - 1 ) {
         *status = SAI__ERROR;
         msgSeti( "AX", axis );
         msgSeti( "M", ndim );
         errRep( "", "SMF_REORDER: Cannot yet re-order axis ^AX in a ^N "
                 "dimensional array.", status );
      }
   }

/* If a mask has been supplied, ensure it refers to the last axis but one
   axis (if an index was supplid), or the last axis (if no index was
   supplied). */
   if( mask ) {
      if( ( index && maxis != ndim - 2 ) || ( !index && maxis != ndim - 1 ) ) {
         *status = SAI__ERROR;
         msgSeti( "AX", maxis );
         msgSeti( "M", ndim );
         errRep( "", "SMF_REORDER: Cannot yet mask axis ^AX in a ^N "
                 "dimensional array.", status );
      }
   }

/* Check the axes are OK. */
   if( *status == SAI__OK ) {

/* First deal with cases where an index was supplied. */
      if( index ){

/* Get the number of elements in one hyper-plane, and get the corresponding 
   number of bytes. */
         plane_size = len;
         for( i = 0; i < axis; i++ ) plane_size *= dims_in[ i ];
         plane_nbyte = plane_size*sizeof( char );

/* If a mask was supplied, get the number of elements in one hyper-row, and 
   get the corresponding number of bytes. */
         if( mask ) {
            row_size = len;
            for( i = 0; i < maxis; i++ ) row_size *= dims_in[ i ];
            row_nbyte = row_size*sizeof( char );
         } 

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;

/* Loop round every hyper-plane in the input array. */
         for( i = 0; i < dims_in[ axis ]; i++ ){
            pin = in + index[ i ]*plane_size;

/* If masking, copy the hyper-plane from input to output in sections of
   one hyper-row. */
            if( mask ) {
               for( j = 0; j < dims_in[ maxis ]; j++, pin += row_size ){
                  if( mask[ j ] ) {
                     memcpy( pout, pin, row_nbyte );
                     pout += row_size;
                  }
               }   

/* If not masking, copy the whole hyper-plane from input to output. */
            } else {
               memcpy( pout, pin, plane_nbyte );
               pout += plane_size;
            }
         }

/* Now deal with cases where no index was supplied but a mask was supplied. */
      } else if( mask ){

/* Get the number of elements in one hyper-row, and get the corresponding 
   number of bytes. */
         row_size = len;
         for( i = 0; i < maxis; i++ ) row_size *= dims_in[ i ];
         row_nbyte = row_size*sizeof( char );

/* Initialise a pointer to the start of the next hyperplane in the output
   array. */
         pout = out;
         pin = in;
         for( j = 0; j < dims_in[ maxis ]; j++, pin += row_size ){
            if( mask[ j ] ) {
               memcpy( pout, pin, row_nbyte );
               pout += row_size;
            }
         }   

/* Now deal with cases where no index or mask was supplied. */
      } else {

/* Get the total number of elements in an arrays. */
         array_size = len;
         for( i = 0; i < ndim; i++ ) array_size *= dims_in[ i ];
         array_nbyte = array_size*sizeof( char );

/* Copy the array. */
         memcpy( out, in, array_nbyte );
      }
   }
}




