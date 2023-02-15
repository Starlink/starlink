/*
*+
*  Name:
*     smf_reduce_dark

*  Purpose:
*     Process a dark time series if necessary.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_reduce_dark( const smfData * indark, smf_dtype dtype,
*                      smfData **outdark, int * status );

*  Arguments:
*     indark = const smfData * (Given)
*        Dark to be processed.
*     dtype = smf_dtype (Given)
*        Data type of output smfData. SMF__NULL will return the
*        dark using the same data type as indark uses.
*     outdark = smfData ** (Returned)
*        Pointer to return processed dark. *outdark is NULL
*        if the input dark has already been reduced (is 2d).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine reduces the input dark time series by calculating
*     a mean signal for each bolometer. A new smfData is created with the
*     result. If the dark is already 2D the routine returns without action.
*     The caller should check for this condition by making seeing if
*     *outdark is non-NULL. This is done for efficiency.

*  Notes:
*     - Use smf_close_file to free the reduced dark memory.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-17 (TIMJ):
*        Initial version
*     2008-08-28 (TIMJ):
*        Use the flatfield to mask out bolometers
*     2010-03-09 (TIMJ):
*        Change type of flatfield method in smfDA

*  Copyright:
*     Copyright (C) 2008, 2010 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"

void smf_reduce_dark( const smfData *indark, smf_dtype dtype,
                      smfData **outdark,
                      int *status ) {

  float clip[1];      /* Sigma clip levels for statistics */

  if (*status != SAI__OK) return;

  /* check we have a dark (inefficient if the caller has already
     done so but we can not guarantee) */
  if (!smf_isdark( indark, status)) {
    *status = SAI__ERROR;
    errRep( " ", "Attempting to reduce an observation as if it is a dark"
            " but this file is not a dark", status);
    return;
  }

  /* see if we have a 2d dark input (likely reduced) */
  if (indark->ndims == 2 ||
      (indark->ndims == 3 && (indark->dims)[2] == 1) ) {
    *outdark = NULL;
    return;
  }

  /* now calculate the average and standard deviation. Retaining the result
     as integers. Flag any bolometers that have constant signal or a signal
     to noise less than 1. */
  clip[0] = 3.0;
  smf_collapse_tseries( indark, 1, clip, 1.0, 1, dtype, outdark, status );

  /* The flatpar array contains the flat parameters in the order
     matching the bolometer data - ie colsize*rowsize for first
     parameter then colsize*rowsize for second parameter and so on. A
     dead pixel will have VAL__BADD as its first parameter. Mark this
     in the mean array. Do not do this for actual flatfield observations. */

  if (indark->da && indark->da->flatcal
      && indark->hdr->obstype != SMF__TYP_FLATFIELD
      && indark->da->flatmeth != SMF__FLATMETH_NULL ) {
    dim_t nbols = (indark->dims)[0] * (indark->dims)[1];
    dim_t i;
    double *flatcal = indark->da->flatcal;
    int *idata = NULL;
    int *ivar  = NULL;
    double *ddata = NULL;
    double *dvar  = NULL;

    switch ((*outdark)->dtype) {
    case SMF__DOUBLE:
      ddata = ((*outdark)->pntr)[0];
      dvar  = ((*outdark)->pntr)[1];

      for (i=0; i<nbols; i++) {
        if (flatcal[i] == VAL__BADD) {
          ddata[i] = VAL__BADD;
          dvar[i] = VAL__BADD;
        }
      }

      break;

    case SMF__INTEGER:
      idata = ((*outdark)->pntr)[0];
      ivar  = ((*outdark)->pntr)[1];

      for (i=0; i<nbols; i++) {
        if (flatcal[i] == VAL__BADD) {
          idata[i] = VAL__BADI;
          ivar[i] = VAL__BADI;
        }
      }

      break;

    default:

      if (*status == SAI__OK) {
        msgSetc( "DT", smf_dtype_string( *outdark, status ));
        *status = SAI__ERROR;
        errRep( " ", "Unsupported data type for masking (^DT)",
                status );
      }
    }


  }


  return;
}
