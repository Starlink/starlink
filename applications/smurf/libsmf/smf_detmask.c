/*
*+
*  Name:
*     smf_detmask<x>

*  Purpose:
*     Re-orders hyper-planes in an array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_detmask( type, void *in, int len, int ndim, int *dims_in, 
*                  int axis, int *index, int maxis, int *mask, void *out, 
*                  int *status );
*
*     smf_detmask<x>( <type> *in, int ndim, int *dims_in, int axis, 
*                     int *index, int maxis, int *mask, <type> *out, 
*                     int *status );
*
*     smf_detmaskc( char *in, int len, int ndim, int *dims_in, int axis, 
*                   int *index, int maxis, int *mask, char *out, 
*                   int *status );

*  Arguments:
*     type = const char * (Given)
*        The HDS data type. Note, this argument is only included in the 
*        interface for the generic "smf_detmask" function.
*     in = <type> * (Given)
*        Point to the vectorised input array. The elements are assumed to be
*        stored in fortran order (i.e. the first axis varies fastest).
*     len = int (Given)
*        The length of each individual sub-string within the "in" and "out" 
*        arrays. Note, this argument is only included in the interface for
*        smf_detmask and smf_detmaskc. The smf_detmask function ignores
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
*        The zero-based index of the dimension that is to be masked.
*        Ignored if "mask" is NULL.
*     mask = int * (Given)
*        An array with one element for each input pixel on the dimension that 
*        is being masked (i.e. it should have "dims_in[maxis]" elements). This 
*        array should contain non-zero values for those hyper-rpows that
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
*     - The smf_detmaskc function has an extra parameter "len" giving the
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
*        Added generic smf_detmask function.
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

void smf_detmask( const char *type, void *in, int len, int ndim, int *dims_in, 
                  int maxis, int *mask, void *out, int *status ){

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Call the correct function for the specified data type. */
   if( !strcmp( type, "_REAL" ) ) {
      smf_detmaskr( (float *) in, ndim, dims_in, maxis, mask, 
                     (float *) out, status );

   } else if( !strcmp( type, "_DOUBLE" ) ) {
      smf_detmaskd( (double *) in, ndim, dims_in, maxis, mask, 
                    (double *) out, status );

   } else if( !strcmp( type, "_INTEGER" ) ) {
      smf_detmaski( (int *) in, ndim, dims_in, maxis, mask, 
                    (int *) out, status );

   } else if( !strncmp( type, "_CHAR", 5 ) ) {
      smf_detmaskc( (char *) in, len, ndim, dims_in, maxis, mask, 
                    (char *) out, status );

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetc( "TYPE", type );
      errRep( "", "SMF_DETMASK: ^TYPE data type not yet supported.", status );
   }
}

void smf_detmaskr( float *in, int ndim, int *dims_in, int maxis, int *mask, float *out, 
                   int *status ){

/* Local Variables */
   float *pin;
   float *pout;
   int i;
   size_t ic[ NDF__MXDIM ];
   size_t iel;
   size_t nel;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the total number of elements in the input array, and initialise
   the indices of the first pixel. */
   nel = 1;
   for( i = 0; i < ndim; i++ ) {
      nel *= dims_in[ i ];
      ic[ i ] = 0;
   }

/* Initialise pointers to the next input and output values. */
   pin = in;    
   pout = out;

/* Loop round all pixels in the input array. */
   for( iel = 0; iel < nel; iel++, pin++ ) {

/* If the current input element has a zero value in the mask, ignore it.
   Otherwise copy it to the output. */
      if( mask[ ic[ maxis ] ] ) *(pout++) = *pin;

/* Get the indices of the next input element. */
      i = 0;
      ic[ i ]++;
      while( ic[ i ] == dims_in[ i ] ) {
         ic[ i++ ] = 0;
         if( i < ndim ) {
            ic[ i ]++;
         } else {
            break;
         }          
      }
   }
}

void smf_detmaskd( double *in, int ndim, int *dims_in, int maxis, int *mask, double *out, 
                   int *status ){

/* Local Variables */
   double *pin;
   double *pout;
   int i;
   size_t ic[ NDF__MXDIM ];
   size_t iel;
   size_t nel;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the total number of elements in the input array, and initialise
   the indices of the first pixel. */
   nel = 1;
   for( i = 0; i < ndim; i++ ) {
      nel *= dims_in[ i ];
      ic[ i ] = 0;
   }

/* Initialise pointers to the next input and output values. */
   pin = in;    
   pout = out;

/* Loop round all pixels in the input array. */
   for( iel = 0; iel < nel; iel++, pin++ ) {

/* If the current input element has a zero value in the mask, ignore it.
   Otherwise copy it to the output. */
      if( mask[ ic[ maxis ] ] ) *(pout++) = *pin;

/* Get the indices of the next input element. */
      i = 0;
      ic[ i ]++;
      while( ic[ i ] == dims_in[ i ] ) {
         ic[ i++ ] = 0;
         if( i < ndim ) {
            ic[ i ]++;
         } else {
            break;
         }          
      }
   }
}


void smf_detmaski( int *in, int ndim, int *dims_in, int maxis, int *mask, int *out, 
                   int *status ){

/* Local Variables */
   int *pin;
   int *pout;
   int i;
   size_t ic[ NDF__MXDIM ];
   size_t iel;
   size_t nel;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the total number of elements in the input array, and initialise
   the indices of the first pixel. */
   nel = 1;
   for( i = 0; i < ndim; i++ ) {
      nel *= dims_in[ i ];
      ic[ i ] = 0;
   }

/* Initialise pointers to the next input and output values. */
   pin = in;    
   pout = out;

/* Loop round all pixels in the input array. */
   for( iel = 0; iel < nel; iel++, pin++ ) {

/* If the current input element has a zero value in the mask, ignore it.
   Otherwise copy it to the output. */
      if( mask[ ic[ maxis ] ] ) *(pout++) = *pin;

/* Get the indices of the next input element. */
      i = 0;
      ic[ i ]++;
      while( ic[ i ] == dims_in[ i ] ) {
         ic[ i++ ] = 0;
         if( i < ndim ) {
            ic[ i ]++;
         } else {
            break;
         }          
      }
   }
}

void smf_detmaskc( char *in, int len, int ndim, int *dims_in, int maxis, int *mask, 
                   char *out, int *status ){

/* Local Variables */
   char *pin;
   char *pout;
   int i;
   size_t ic[ NDF__MXDIM ];
   size_t iel;
   size_t nel;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the total number of elements in the input array, and initialise
   the indices of the first pixel. */
   nel = 1;
   for( i = 0; i < ndim; i++ ) {
      nel *= dims_in[ i ];
      ic[ i ] = 0;
   }

/* Initialise pointers to the next input and output values. */
   pin = in;    
   pout = out;

/* Loop round all pixels in the input array. */
   for( iel = 0; iel < nel; iel++, pin += len ) {

/* If the current input element has a zero value in the mask, ignore it.
   Otherwise copy it to the output. */
      if( mask[ ic[ maxis ] ] ) {
         memcpy( pout, pin, len*sizeof( char ) );
         pout += len;
      }

/* Get the indices of the next input element. */
      i = 0;
      ic[ i ]++;
      while( ic[ i ] == dims_in[ i ] ) {
         ic[ i++ ] = 0;
         if( i < ndim ) {
            ic[ i ]++;
         } else {
            break;
         }          
      }
   }
}

