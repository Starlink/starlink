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
*     - Works with _INTEGER and _DOUBLE input "indata". Assumes that
*       _DOUBLE does not mean the data have been flatfielded.
*     - Actually a general purpose "subtract 2d image from time series"
*       function. Now has a bad name.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-JUL-2008 (TIMJ):
*        Initial version.
*     25-JUL-2008 (TIMJ):
*        More robust error checking.
*     25-AUG-2008 (TIMJ):
*        Forgot to trap for bad values in darks.
*     27-AUG-2008 (TIMJ):
*        Darks and indata can be _DOUBLE.
*     02-SEP-2008 (TIMJ):
*        Logic tidy up
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

  int *idark = NULL;    /* Pointer to int dark frame to subtract */
  double *ddark = NULL; /* Pointer to double frame to subtract */

  void *dark = NULL;    /* pointer to dark frame for this slice */
  double *dkbuf = NULL;   /* malloced buffer for dark */
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

  if ( (indata->pntr)[0] == NULL) {
    *status = SAI__ERROR;
    errRep( " ", "Input data array for dark subtraction is a null pointer",
            status);
    return;
  }

  if (dark1 && dark2 && dark1->dtype != dark2->dtype) {
    msgSetc("DT1", smf_dtype_string(dark1, status));
    msgSetc("DT2", smf_dtype_string(dark2, status));
    *status = SAI__ERROR;
    errRep( " ","Both darks defined but with differing types (^DT1 != ^DT2)",
            status);
    return;
  }

  if (dark1 && (dark1->dtype != SMF__INTEGER && dark1->dtype != SMF__DOUBLE)) {
    *status = SAI__ERROR;
    errRep( " ","Dark 1 is neither INTEGER nor DOUBLE", status);
    return;
  }
  if (dark2 && (dark2->dtype != SMF__INTEGER && dark2->dtype != SMF__DOUBLE)) {
    *status = SAI__ERROR;
    errRep( " ","Dark 2 is neither INTEGER nor DOUBLE", status);
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

    smf_select_pntr( dark1->pntr, dark1->dtype, &ddark, NULL,
                     &idark, NULL, status );
    break;

  case SMF__DKSUB_NEXT:
    if (dark1 == NULL) {
      *status = SAI__ERROR;
      errRep( " ", "Requested subtraction of following dark but it is NULL",
              status );
      return;
    }

    smf_select_pntr( dark2->pntr, dark2->dtype, &ddark, NULL,
                     &idark, NULL, status );
    break;

  case SMF__DKSUB_INTERP:
      *status = SAI__ERROR;
      errRep(" ", "Interpolation not yet supported for dark subtract", status);
      return;
      break;
  case SMF__DKSUB_MEAN:
    if (!dark1 || !dark2) {
      *status = SAI__ERROR;
      errRep( " ", "Requested mean dark but only one dark available",
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

  /* for mean we need to allocate a buffer. Use a double */
  if (method == SMF__DKSUB_MEAN) {
    int *idkp1;
    double *ddkp1;
    int *idkp2;
    double *ddkp2;
    
    smf_select_pntr( dark1->pntr, dark1->dtype, &ddkp1, NULL,
                     &idkp1, NULL, status );
    smf_select_pntr( dark2->pntr, dark2->dtype, &ddkp2, NULL,
                     &idkp2, NULL, status );
    dkbuf = smf_malloc( nbols, sizeof(*dkbuf), 0, status );
    ddark = dkbuf;

    if (ddkp1 && ddkp2) {
      for (i = 0; i < nbols; i++) {
        if (ddkp1[i] != VAL__BADD && ddkp2[i] != VAL__BADD) {
          /* worry about numerical overflow */
          dkbuf[i] = (0.5 * ddkp1[i]) + (0.5 * ddkp2[i]);
        } else {
          dkbuf[i] = VAL__BADD;
        }
      }
    } else if (idkp1 && idkp1) {
      for (i = 0; i < nbols; i++) {
        if (idkp1[i] != VAL__BADI && idkp2[i] != VAL__BADI) {
          /* worry about numerical overflow */
          dkbuf[i] = (0.5 * (double)idkp1[i]) +
            (0.5 * (double)idkp2[i]);
        } else {
          dkbuf[i] = VAL__BADD;
        }
      }
    } else {
      if (*status == SAI__OK) {
        *status = SAI__ERROR;
        errRep(" ",FUNC_NAME ": darks differ in type for mean calculation",
               status);
      }
    }
  }


  switch (method) {
    case SMF__DKSUB_PREV:
    case SMF__DKSUB_NEXT:
    case SMF__DKSUB_MEAN:

      if ((idark == NULL && ddark == NULL) && *status == SAI__OK) {
        *status = SAI__ERROR;
        errRep(" ","Selected dark frame has a null pointer for data array",
               status );
      }

      if (*status == SAI__OK) {
        double * ddata;
        int * idata;
        size_t j;
        size_t startidx;

        /* get the correct input data pointer */
        smf_select_pntr( indata->pntr, indata->dtype, &ddata, NULL,
                         &idata, NULL, status);

        if (ddata) {
          double * slice;
          for (i = 0; i < (indata->dims)[2]; i++) {
            startidx = i * nbols;
            slice = &(ddata[startidx]);
            for (j=0;  j < nbols; j++) {
              if (slice[j] != VAL__BADD) {
                double darkval = VAL__BADD;

                /* Get the relevant dark value */
                if (ddark) {
                  darkval = ddark[j];
                } else if (idark && idark[j] != VAL__BADI) {
                  darkval = (double)idark[j];
                }

                /* subtract it if non-bad */
                if (darkval != VAL__BADD ) {
                  slice[j] -= darkval;
                } else {
                  slice[j] = VAL__BADD;
                }

              }
            }
          }
        } else if (idata) {
          int *slice;

          for (i = 0; i < (indata->dims)[2]; i++) {
            startidx = i * nbols;
            slice = &(idata[startidx]);
            for (j=0;  j < nbols; j++) {
              if (slice[j] != VAL__BADI) {
                int darkval = VAL__BADI;

                /* Get the relevant dark value */
                if (idark) {
                  darkval = idark[j];
                } else if (ddark && ddark[j] != VAL__BADD) {
                  darkval = (int)ddark[j];
                }

                /* subtract it if non-bad */
                if (darkval != VAL__BADI) {
                  slice[j] -= darkval;
                } else {
                  slice[j] = VAL__BADI;
                }

              }
            }
          }

        } else {
          if (*status == SAI__OK) {
            *status = SAI__ERROR;
            errRep( " ", FUNC_NAME ": Should not be possible to get here",
                    status);
          }

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
