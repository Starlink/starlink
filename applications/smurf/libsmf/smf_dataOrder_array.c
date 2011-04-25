/*
*+
*  Name:
*     smf_dataOrder_array

*  Purpose:
*     Low-level data buffer re-ordering function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:

*     newbuf = smf_dataOrder_array( void * oldbuf, smf_dtype dtype,
*                                   size_t ndata, size_t ntslice,
*                                   size_t nbolo, size_t tstr1, size_t bstr1,
*                                   size_t tstr2, size_t bstr2, int inPlace,
*                                   int freeOld, int * status )

*  Arguments:
*     oldbuf = void * (Given and Returned)
*        Pointer to the data buffer to be re-ordered. Also contains the
*        re-ordered data if inPlace=1
*     dtype = smf_dtype (Given)
*        Data type of the buffer
*     ndata = size_t (Given)
*        Number of elements in oldbuf
*     ntslice = size_t (Given)
*        Number of time slices in oldbuf
*     nbolo = size_t (Given)
*        Number of bolometers in oldbuf
*     tstr1 = size_t (Given)
*        Time stride of oldbuf
*     bstr1 = size_t (Given)
*        Bolo stride of oldbuf
*     tstr2 = size_t (Given)
*        Time stride of re-ordered buffer
*     bstr2 = size_t (Given)
*        Bolo stride of re-ordered buffer
*     inPlace = int (Given)
*        If set replace the contents of oldbuf with the re-ordered data.
*     freeOld = int (Given)
*        If set, and inPlace=0, free oldbuf before returning.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Provide pointer to void. Reorder that array in some new
*     workspace then either copy everything back into the supplied
*     array and return it or free the memory (assumed to be malloced
*     and not mmapped) and return the new workspace.

*  Returned Value:
*     newbuf = void *
*        Pointer to the re-ordered data.

*  Notes:
*     Will do nothing if the input and output strides are the same
*     and the inPlace flag is true.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-08-31 (EC):
*        Move private routine written by TIMJ out of smf_dataOrder.c
*     2010-09-17 (EC):
*        Add freeOrder flag in case routine is being used for copy
*     2011-04-25 (TIMJ):
*        Trap case when input and output strides are the same.

*  Notes:

*  Copyright:
*     Copyright (C) 2010-2011 Science & Technology Facilities Council.
*     Copyright (C) 2010 University of British Columbia.
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

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_dataOrder_array"

void * smf_dataOrder_array( void * oldbuf, smf_dtype dtype, size_t ndata,
                            size_t ntslice, size_t nbolo,
                            size_t tstr1, size_t bstr1,
                            size_t tstr2, size_t bstr2, int inPlace,
                            int freeOld, int * status ) {
  size_t sz = 0;        /* Size of data type */
  void * newbuf = NULL; /* Space to do the reordering */
  void * retval = NULL; /* Return value with reordered buffer */

  retval = oldbuf;
  if (*status != SAI__OK) return retval;
  if (!retval) return retval;

  /* Special case the inPlace variant with no reordering */
  if ( inPlace && tstr1 == tstr2 && bstr1 == bstr2 ) {
    return retval;
  }

  /* Size of data type */
  sz = smf_dtype_sz(dtype, status);

  /* Allocate buffer */
  newbuf = astCalloc( ndata, sz, 0 );

  if( *status == SAI__OK ) {

    /* if the input and output strides are the same we just memcpy */
    if ( tstr1 == tstr2 && bstr1 == bstr2 ) {

      memcpy( newbuf, oldbuf, sz*ndata );

    } else {
      size_t j;
      size_t k;

      /* Loop over all of the elements and re-order the data */
      switch( dtype ) {
      case SMF__INTEGER:
        for( j=0; j<ntslice; j++ ) {
          for( k=0; k<nbolo; k++ ) {
            ((int *)newbuf)[j*tstr2+k*bstr2] =
              ((int *)oldbuf)[j*tstr1+k*bstr1];
          }
        }
        break;

      case SMF__FLOAT:
        for( j=0; j<ntslice; j++ ) {
          for( k=0; k<nbolo; k++ ) {
            ((float *)newbuf)[j*tstr2+k*bstr2] =
              ((float *)oldbuf)[j*tstr1+k*bstr1];
          }
        }
        break;

      case SMF__DOUBLE:
        for( j=0; j<ntslice; j++ ) {
          for( k=0; k<nbolo; k++ ) {
            ((double *)newbuf)[j*tstr2+k*bstr2] =
              ((double *)oldbuf)[j*tstr1+k*bstr1];
          }
        }
        break;

      case SMF__USHORT:
        for( j=0; j<ntslice; j++ ) {
          for( k=0; k<nbolo; k++ ) {
            ((unsigned short *)newbuf)[j*tstr2+k*bstr2] =
              ((unsigned short *)oldbuf)[j*tstr1+k*bstr1];
          }
        }
        break;

      case SMF__UBYTE:
        for( j=0; j<ntslice; j++ ) {
          for( k=0; k<nbolo; k++ ) {
            ((unsigned char *)newbuf)[j*tstr2+k*bstr2] =
              ((unsigned char *)oldbuf)[j*tstr1+k*bstr1];
          }
        }
        break;

      default:
        msgSetc("DTYPE",smf_dtype_str(dtype, status));
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME
                ": Don't know how to handle ^DTYPE type.", status);
      }
    }

    if( inPlace ) {
      /* Copy newbuf to oldbuf */
      memcpy( oldbuf, newbuf, ndata*sz );
      /* Free newbuf */
      newbuf = astFree( newbuf );

      retval = oldbuf;
    } else {

      if( freeOld ) {
        /* Free oldbuf */
        oldbuf = astFree( oldbuf );
      }

      /* Set pntr to newbuf */
      retval = newbuf;
    }
  }

  return retval;
}
