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
*     smf_collapse_tseries( const smfData * indata, smfData **outdata,
*                      int * status );

*  Arguments:
*     indata = const smfData * (Given)
*        3D data to be processed.
*     dtype = smf_dtype (Given)
*        Data type of output smfData. SMF__NULL will use the data type of
*        indata.
*     doclip = int (Given)
*        Do a clipped mean, else if false, calculate the mean.
*     snrlim = double (Given)
*        If non-zero, data will be set to bad value if the signal-to-noise
*        ratio is below the supplied value.
*     flagconst = int (Given)
*        If true, elements with a standard deviation of zero but ngood
*        greater than 1 will be flagged bad.
*     outtype = smf_dtype (Given)
*        Data type of "outdata". 
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
*     {enter_new_authors_here}

*  History:
*     2008-08-21 (TIMJ):
*        Initial version

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

void smf_collapse_tseries( const smfData *indata, int doclip, 
                           double snrlim, int flagconst, smf_dtype dtype,
                           smfData **outdata, 
                           int *status ) {

  /* Per type pointers */
  double *avg_d = NULL;
  double *stdev_d = NULL;
  int *avg_i = NULL;
  int *stdev_i = NULL;

  dim_t dims[2];      /* dimensions of data array */
  smfHead *hdr = NULL; /* copy of header */
  AstKeyMap * history = NULL; /* history */
  size_t nbperel;  /* Number of bytes in dtype */
  size_t nelem;   /* number of elements in mean image */
  void *pntr[3] = { NULL, NULL, NULL }; /* pointers to data */

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
  pntr[0] = smf_malloc( nelem, nbperel, 0, status );
  pntr[1] = smf_malloc( nelem, nbperel, 0, status );


  /* Assign the pointers */
  switch (dtype) {
  case SMF__DOUBLE:
    avg_d = pntr[0];
    stdev_d = pntr[1];
    break;
  case SMF__INTEGER:
    avg_i = pntr[0];
    stdev_i = pntr[1];
    break;
  default:
    msgSetc( "TYP", smf_dtype_string( indata, status ));
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME " Unsupported data type ^TYP",
           status);
  }

  if (*status == SAI__OK) {
    dim_t i,j;

    /* get statistics for each bolometer */
    for (i = 0; i < dims[0]; i++) {
      for (j = 0; j < dims[1]; j++) {
        double mean = VAL__BADD;
        double stdev = VAL__BADD;
        dim_t index;
        index = ( j * dims[0] ) + i;

        smf_calc_stats( indata, "b", index, 0, 0, &mean, &stdev, status );

        if (flagconst && stdev == 0.0) {
          mean = VAL__BADD;
          stdev = VAL__BADD;
        }
        if (snrlim > 0 && mean != VAL__BADD && stdev != VAL__BADD) {
          double snr;
          snr = fabs(mean/stdev);
          if (snr < snrlim) {
            mean = VAL__BADD;
            stdev = VAL__BADD;
          }
        }

        switch (dtype) {
        case SMF__DOUBLE:
          avg_d[index] = mean;
          stdev_d[index] = stdev;
          break;
        case SMF__INTEGER:
          if (isnan(mean) || mean == VAL__BADD ) {
            avg_i[index] = VAL__BADI;
          } else {
            avg_i[index] = (int)mean;
          }
          if (isnan(stdev) || stdev == VAL__BADD) {
            stdev_i[index] = VAL__BADI;
          } else {
            stdev_i[index] = (int)stdev;
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

  *outdata = smf_construct_smfData( NULL, NULL,  hdr, NULL,
                                    dtype, pntr, 1, dims, 2, 0, 0,
                                    NULL, history, status );

  /* must free the data if outdata is null */
  if (*outdata == NULL) {
    pntr[0] = smf_free( pntr[0], status );
    pntr[1] = smf_free( pntr[1], status );
  }

  return;
}
