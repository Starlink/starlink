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

*     newbuf = smf_dataOrder_array( void * oldbuf, smf_dtype oldtype,
*                                   smf_dtype newtype,
*                                   size_t ndata, size_t ntslice,
*                                   size_t nbolo, size_t tstr1, size_t bstr1,
*                                   size_t tstr2, size_t bstr2, int inPlace,
*                                   int freeOld, int * status )

*  Arguments:
*     oldbuf = void * (Given and Returned)
*        Pointer to the data buffer to be re-ordered. Also contains the
*        re-ordered data if inPlace=1
*     oldtype = smf_dtype (Given)
*        Data type of the original buffer
*     newtype = smf_dtype (Given)
*        Data type of the target buffer
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
*     Will do nothing if the input and output strides are the same,
*     the inPlace flag is true, and the data types are the same.

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
*     2011-06-22 (EC):
*        Add ability to typecast while copying (oldtype/newtype)

*  Notes:

*  Copyright:
*     Copyright (C) 2010-2011 Science & Technology Facilities Council.
*     Copyright (C) 2010-2011 University of British Columbia.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

void * smf_dataOrder_array( void * oldbuf, smf_dtype oldtype, smf_dtype newtype,
                            size_t ndata, size_t ntslice, size_t nbolo,
                            size_t tstr1, size_t bstr1,
                            size_t tstr2, size_t bstr2, int inPlace,
                            int freeOld, int * status ) {
  size_t szold = 0;        /* Size of old data type */
  size_t sznew = 0;        /* Size of new data type */
  void * newbuf = NULL;    /* Space to do the reordering */
  void * retval = NULL;    /* Return value with reordered buffer */

  retval = oldbuf;
  if (*status != SAI__OK) return retval;
  if (!retval) return retval;

  /* Can't do inPlace without realloc'ing if data types don't match...
     so generate bad status for now */
  if( (newtype != oldtype) && inPlace ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": inPlace not supported if newtype != oldtype",
            status );
    return retval;
  }

  /* For now the only data conversion that is supported is from SMF__INTEGER
     to SMF__DOUBLE */
  if( (oldtype!=newtype) &&
      ((newtype!=SMF__DOUBLE) || (oldtype!=SMF__INTEGER)) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME
            ": type conversion only supported from integer -> double",
            status );
    return retval;
  }

  /* Special case the inPlace variant with no reordering / typecasting */
  if ( inPlace && tstr1 == tstr2 && bstr1 == bstr2 && oldtype == newtype ) {
    return retval;
  }

  /* Size of data type */
  sznew = smf_dtype_sz(newtype, status);
  szold = smf_dtype_sz(oldtype, status);

  /* Allocate buffer */
  newbuf = astMalloc( ndata*sznew );

  if( *status == SAI__OK ) {

    /* if the input and output strides are the same, and the data types are
       the same,  we just memcpy */
    if ( tstr1 == tstr2 && bstr1 == bstr2 && oldtype == newtype ) {

      memcpy( newbuf, oldbuf, sznew*ndata );

    } else {
      size_t j;
      size_t k;

      /* Loop over all of the elements and re-order the data */
      switch( oldtype ) {
      case SMF__INTEGER:
        if( newtype == SMF__DOUBLE ) {
          /* Convert integer to double */
          for( j=0; j<ntslice; j++ ) {
            for( k=0; k<nbolo; k++ ) {
              int val = ((int *)oldbuf)[j*tstr1+k*bstr1];
              if( val != VAL__BADI ) {
                ((double *)newbuf)[j*tstr2+k*bstr2] = val;
              } else {
                ((double *)newbuf)[j*tstr2+k*bstr2] = VAL__BADD;
              }
            }
          }

        } else {
          /* integer to integer */
          for( j=0; j<ntslice; j++ ) {
            for( k=0; k<nbolo; k++ ) {
              ((int *)newbuf)[j*tstr2+k*bstr2] =
                ((int *)oldbuf)[j*tstr1+k*bstr1];
            }
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
        msgSetc("DTYPE",smf_dtype_str(oldtype, status));
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME
                ": Don't know how to handle ^DTYPE type.", status);
      }
    }

    if( inPlace ) {
      /* Copy newbuf to oldbuf */
      memcpy( oldbuf, newbuf, ndata*sznew );
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
