/*
*+
*  Name:
*     smf_dataOrder_array

*  Purpose:
*     Low-level N-dim data re-ordering function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:

*     newbuf =  smf_dataOrder_ndims( void * oldbuf, smf_dtype dtype,
*                                    dim_t ndata, dim_t ndims,
*                                    const dim_t dims[], const dim_t perm[],
*                                    int inPlace, int freeOld,
*                                    int * status )

*  Arguments:
*     oldbuf = void * (Given and Returned)
*        Pointer to the data buffer to be re-ordered. Also contains the
*        re-ordered data if inPlace=1
*     dtype = smf_dtype (Given)
*        Data type of the buffer
*     ndata = dim_t (Given)
*        Number of elements in oldbuf
*     ndims = dim_t (Given)
*        Number of dimensions
*     dims = const dim_t [] (Given)
*        Number of pixels along each dimension in input data. Array
*        of dimension ndims.
*     perm = const dim_t [] (Given)
*        Permutation array: new order of dimensions in terms
*        of old dimensions (1.n). E.g. 3,1,2. Array of dimension
*        ndims.
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

*  Authors:
*     Remo Tilanus (JAC, Hawaii)
*     David S Berry (JAC, Hawaii) (KPS1_PRMX)
*     Tim Jennes (JAC, Hawaii)    (smf_dataOrder_array.c)
*     {enter_new_authors_here}

*  History:
*     2010-10-06 (RPT):
*        Created as general version of smf_dataOrder_array.c using
*        KPS1_PRMX

*  Notes:

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.

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

#define FUNC_NAME "smf_dataOrder_ndims"

void * smf_dataOrder_ndims( void * oldbuf, smf_dtype dtype, dim_t ndata,
                            dim_t ndims, const dim_t dims[], const dim_t perm[],
                            int inPlace, int freeOld, int * status ) {

  dim_t sz = 0;        /* Size of data type */
  void * newbuf = NULL; /* Space to do the reordering */
  void * retval = NULL; /* Return value with reordered buffer */

  dim_t i;             /* Counter */
  dim_t j;             /* Counter */
  dim_t k;             /* Counter */

  dim_t iout[ndims+1]; /* Output pixel indices */
  dim_t odim[ndims+1]; /* Copy of o/p dimensions plus dummy */
  dim_t stepi[ndims];  /* Input steps indexed by input axis */
  dim_t step[ndims+1]; /*  Input steps indexed by output axis */

  retval = oldbuf;
  if (*status != SAI__OK) return retval;
  if (!retval) return retval;


  /*
  ** msgOutiff( MSG__DEBUG, "",
  **           "%s dtype: %s, ndata: %d, ndims: %d, inPlace: %d, freeOld %d",
  **           status, FUNC_NAME, smf_dtype_str(dtype, status), (int) ndata,
  **           (int) ndims, (int) inPlace, (int) freeOld );
  */

  char str1[24] = "";
  char str2[24] = "";
  size_t nc1 = 0;
  size_t nc2 = 0;

  for ( i=0; i<ndims; i++ ) {
    if ( perm[i] < 1 || perm[i] > ndims ) {
      msgOutf( "","Permutation axis %d value %d not in range 1..%d",
               status, (int) (i+1), (int) perm[i], (int) ndims );
      *status = SAI__ERROR;
    }
    nc1 += sprintf( str1 + nc1, ",%d", (int) dims[(perm[i]-1)] );
    nc2 += sprintf( str2 + nc2, ",%d", (int) perm[i] );
  }
  msgOutiff(  MSG__DEBUG, "","%s: new cube dims [%s] with original axes [%s]",
	      status, FUNC_NAME, str1+1, str2+1);

  if (*status != SAI__OK) {
    errRep( FUNC_NAME,": Error.", status ) ;
    return retval;
  }

  /* Size of data type */
  sz = smf_dtype_sz(dtype, status);

  /* Allocate buffer */
  newbuf = astMalloc( ndata*sz );

  if( *status == SAI__OK ) {

    /* Find the vector steps within the input array for unit increment along
       each input axis, and  initialise the indices of the current output
       pixel. */

    j = 1;
    for ( i = 0; i < ndims; i++ ) {
      stepi[i] = j;
      j *= dims[i];
      iout[i] = 1;
      odim[i] = dims[(perm[i]-1)];
    }
    iout[ndims] = 1;
    odim[ndims] = 1;

    /* Permute the step array so that it is indexed by output axis instead
       of input axis. */
    for ( i = 0; i < ndims; i++ ) {
      step[i] = stepi[(perm[i]-1) ];
    }
    step[ndims] = ndata;

    /* initialise the vector index within the input array of the current
       output pixel. */
    k = 0;

    /* Loop over all output elements and re-order the data */
    for ( j = 0; j < ndata; j++ ) {

      /* Copy the data value.*/

      switch( dtype ) {
      case SMF__INTEGER:
        ((int *)newbuf)[j] = ((int *)oldbuf)[k];
        break;
      case SMF__FLOAT:
        ((float *)newbuf)[j] = ((float *)oldbuf)[k];
        break;
      case SMF__DOUBLE:
        ((double *)newbuf)[j] = ((double *)oldbuf)[k];
        break;
      case SMF__USHORT:
        ((unsigned short *)newbuf)[j] =
          ((unsigned short *)oldbuf)[k];
        break;
      case SMF__UBYTE:
        ((unsigned char *)newbuf)[j] =
          ((unsigned char *)oldbuf)[k];
        break;
      default:
        msgSetc("DTYPE",smf_dtype_str(dtype, status));
        *status = SAI__ERROR;
        errRep( FUNC_NAME,
                ": Don't know how to handle ^DTYPE type.", status);
      }

      /* Increment the indices of the current output pixel, and update the
         vector index of the corresponding input pixel. */
      i = 0;
      iout[0] += 1;
      k += step[0];
      while( (iout[i] > odim[i]) && (j < ndata-1) ) {
        iout[i] = 1;
        k -= (odim[i] * step[i]);
        i++;
        iout[i] += 1;
        k += step[i];
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
