/*
*+
*  Name:
*     smf_collapse_tseries

*  Purpose:
*     Collapse the data array in the time direction

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_collapse_tseries( const smfData * indata, int nclip,
*                           const float clip[],
*                           double snrlim, int flatconst, smf_dtype dtype,
*                           smfData **outdata, int * status );

*  Arguments:
*     indata = const smfData * (Given)
*        3D data to be processed.
*     nclip = int (Given)
*        Number of K-sigma clipping iterations to apply (number of elements
*        in "clip").
*     clip = const float[] (Given)
*        N-sigma clip levels to use. Expressed as standard deviations.
*     snrlim = double (Given)
*        If non-zero, data will be set to bad value if the signal-to-noise
*        ratio is below the supplied value.
*     flagconst = int (Given)
*        If true, elements with a standard deviation of zero but ngood
*        greater than 1 will be flagged bad.
*     dtype = smf_dtype (Given)
*        Data type of output smfData. SMF__NULL will use the data type of
*        indata.
*     outdata = smfData ** (Returned)
*        Pointer to returned data. Will be 2d.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine collapses the time series into a 2D image. It can
*     use either a mean or clipped mean. If the dark is already 2D the
*     routine returns without action. The caller should check for this condition
*     seeing if *outdata is non-NULL. This is done for efficiency.

*  Notes:
*     - Use smf_close_file to free the returned smfData
*     - see also smf_reduce_dark, smf_average_dataD

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2008-08-21 (TIMJ):
*        Initial version
*     2008-08-25 (TIMJ):
*        Should write variance not standard deviation.
*     2010-09-17 (COBA):
*        Updated smf_construct_smfData which now contains smfFts

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
#include <math.h>

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

#define FUNC_NAME "smf_collapse_tseries"

void smf_collapse_tseries( const smfData *indata, int nclip, const float clip[],
                           double snrlim, int flagconst, smf_dtype dtype,
                           smfData **outdata,
                           int *status ) {

  /* Per type pointers */
  double *avg_d = NULL;
  double *var_d = NULL;
  int *avg_i = NULL;
  int *var_i = NULL;

  dim_t dims[2];      /* dimensions of data array */
  smfHead *hdr = NULL; /* copy of header */
  AstKeyMap * history = NULL; /* history */
  dim_t nbperel;  /* Number of bytes in dtype */
  dim_t nelem;   /* number of elements in mean image */
  void *pntr[] = { NULL, NULL }; /* pointers to data */

  if (*status != SAI__OK) return;

  /* see if we have a 2d input (likely reduced) */
  if (indata->ndims == 2 ||
      (indata->ndims == 3 && (indata->dims)[2] == 1) ) {
    *outdata = NULL;
    return;
  }

  /* Trap SMF__NULL */
  if (dtype == SMF__NULL) dtype = indata->dtype;

  /* Get some memory of the right type - data and variance */
  dims[0] = (indata->dims)[0];
  dims[1] = (indata->dims)[1];
  nelem = dims[0] * dims[1];
  nbperel = smf_dtype_sz(dtype, status);
  pntr[0] = astMalloc( nelem*nbperel );
  pntr[1] = astMalloc( nelem*nbperel );


  /* Assign the pointers */
  smf_select_pntr( pntr, dtype, &avg_d, &var_d, &avg_i, &var_i, status );

  if (*status == SAI__OK) {
    dim_t i,j;

    /* get statistics for each bolometer */
    for (i = 0; i < dims[0]; i++) {
      for (j = 0; j < dims[1]; j++) {
        double mean = VAL__BADD;
        double stdev = VAL__BADD;
        double variance = VAL__BADD;
        dim_t index;
        index = ( j * dims[0] ) + i;

        smf_calc_stats( indata, "b", index, 0, 0, nclip, clip,
                        &mean, &stdev, status );

        if (flagconst && stdev == 0.0) {
          mean = VAL__BADD;
          stdev = VAL__BADD;
        }
        if (snrlim > 0 && mean != VAL__BADD && stdev != VAL__BADD
            && stdev != 0.0) {
          double snr;
          snr = fabs(mean/stdev);
          if (snr < snrlim) {
            mean = VAL__BADD;
            stdev = VAL__BADD;
          }
        }
        if (stdev != VAL__BADD) {
          variance = stdev * stdev;
        }

        switch (dtype) {
        case SMF__DOUBLE:
          avg_d[index] = mean;
          var_d[index] = variance;
          break;
        case SMF__INTEGER:
          if (isnan(mean) || mean == VAL__BADD ) {
            avg_i[index] = VAL__BADI;
          } else {
            avg_i[index] = (int)mean;
          }
          if (isnan(stdev) || stdev == VAL__BADD) {
            var_i[index] = VAL__BADI;
          } else if ( variance > (double)VAL__MAXI ) {
            /* overflow. Convert to BAD */
            var_i[index] = VAL__BADI;
            avg_i[index] = VAL__BADI;
          } else {
            var_i[index] = (int)variance;
          }
          break;
        default:
          *status = SAI__ERROR;
          errRep( " ", "Should be impossible to get here", status );
          goto L999;
        }

      }
    }
  }

 L999:

  /* now create a new smfData - we need to copy the header info */
  hdr = smf_deepcopy_smfHead( indata->hdr, status );
  if (indata->history) history = astCopy( indata->history );

  *outdata = smf_construct_smfData( NULL, NULL,  hdr, NULL, NULL,
                                    dtype, pntr, NULL, SMF__QFAM_TSERIES,
                                    NULL, 0, 1, dims, indata->lbnd, 2, 0, 0,
                                    NULL, history, status );

  /* must free the data if outdata is null */
  if (*outdata == NULL) {
    pntr[0] = astFree( pntr[0] );
    pntr[1] = astFree( pntr[1] );
  }

  return;
}
