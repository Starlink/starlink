/*
*+
*  Name:
*     smf_subtract_dark

*  Purpose:
*     Subtract supplied darks from supplied data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_subtract_dark( smfData *indata, const smfData * dark1,
*        const smfData * dark2, smf_dark_sub_meth method, int * status );

*  Arguments:
*     indata = smfData * (Given & Returned)
*        Data to be dark subtracted.
*     dark1 = const smfData * (Given)
*        Previous dark.
*     dark2 = const smfData * (Given)
*        Following dark.
*     method = smf_dark_sub_meth (Given)
*        Method to use for calculating dark at each time slice.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Subtracts dark information from supplied input data. A number of
*     different schemes are available.

*  Notes:

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-JUL-2008 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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


#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par.h"

#include "smf.h"
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_subtract_dark"

void smf_subtract_dark ( smfData * indata, const smfData * dark1, 
  const smfData * dark2, smf_dark_sub_meth method, int *status ) {

  int *dark = NULL;    /* pointer to dark frame for this slice */
  int *dkbuf = NULL;   /* malloced buffer for dark */
  int *inptr = NULL;   /* pointer to input data */
  size_t i;            /* loop counter */
  size_t nbols;        /* number of bolometers */


  if (*status != SAI__OK) return;

  /* request "do nothing" */
  if (method == SMF__DKSUB_NONE) return;

  if (dark1 == NULL && dark2 == NULL) {
    *status = SAI__ERROR;
    errRep(" ", "No valid dark frames given for dark subtraction", status);
    return;
  }

  if (indata->dtype != SMF__INTEGER) {
    const char * dt = smf_dtype_str( indata->dtype, status );
    msgSetc( "DT", dt );
    *status = SAI__ERROR;
    errRep( " ", "Attempting to dark subtract flatfielded data "
            "(^DT not _INTEGER)!", status );
    return;
  }

  /* calculate number of bolometers */
  nbols = (indata->dims)[0] * (indata->dims)[1];

  /* now do some sanity checks */
  switch (method) {

  case SMF__DKSUB_PREV:
    if (dark1 == NULL) {
      *status = SAI__ERROR;
      errRep( " ", "Requested subtraction of previous dark but it is NULL",
              status );
      return;
    }
    
    dark = (dark1->pntr)[0];

    break;
  case SMF__DKSUB_NEXT:
    if (dark1 == NULL) {
      *status = SAI__ERROR;
      errRep( " ", "Requested subtraction of following dark but it is NULL",
              status );
      return;
    }
    
    dark = (dark2->pntr)[0];

    break;

  case SMF__DKSUB_INTERP:
      *status = SAI__ERROR;
      errRep(" ", "Interpolation not yet supported for dark subtract", status);
      return;
      break;
  case SMF__DKSUB_MEAN:
    if (!dark1 || !dark2) {
      *status = SAI__ERROR;
      errRep( " ", "Requested interpolated dark but only one dark available",
             status );
      return;
    }
    dark = NULL;
    break;

  default:
    *status = SAI__ERROR;
    msgSeti( "MTH", method );
    errRep(" ", "Unrecognized dark subtraction method (^MTH)", status );
    return;
  }

  /* validated input, now need to do something */

  /* for mean we need to allocate a buffer */
  if (method == SMF__DKSUB_MEAN) {
    int * dkp1 = (dark1->pntr)[0];
    int * dkp2 = (dark2->pntr)[0];
    dkbuf = smf_malloc( nbols, sizeof(*dkbuf), 0, status );
    dark = dkbuf;

    for (i = 0; i < nbols; i++) {
      dkbuf[i] = (dkp1[i] + dkp2[i]) / 2;
    }
  }


  switch (method) {
    case SMF__DKSUB_PREV:
    case SMF__DKSUB_NEXT:
    case SMF__DKSUB_MEAN:

    inptr = (indata->pntr)[0];
    for (i = 0; i < (indata->dims)[2]; i++) {
       size_t j;
       size_t startidx = i * nbols;
       int * slice = &(inptr[startidx]);
       for (j = 0; j < nbols; j++) {
         slice[j] -= dark[j];
       }
    }    

    break;
    default:
      if (*status == SAI__OK) {
        *status = SAI__ERROR;
        errRep( " ", "Not supposed to be able to get here", status);
        return;
      }
  }

  if (dkbuf) dkbuf = smf_free( dkbuf, status );

  return;
}
