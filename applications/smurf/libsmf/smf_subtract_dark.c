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
*     - Propogates variance

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
*     03-SEP-2008 (TIMJ):
*        Fix issue with 2d input files
*     11-NOV-2008 (TIMJ):
*        Add CHOOSE method.
*     03-DEC-2008 (TIMJ):
*        Add additional sanity check for time ordered data.
*     2010-01-22 (TIMJ):
*        Propogate variance
*     2010-01-25 (TIMJ):
*        Make sure that the intput and output smfData have variance.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2010 Science and Technology Facilities Council.
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
#include "smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_subtract_dark"

void smf_subtract_dark ( smfData * indata, const smfData * dark1,
  const smfData * dark2, smf_dark_sub_meth method, int *status ) {

  int *idark = NULL;    /* Pointer to int dark frame to subtract */
  double *ddark = NULL; /* Pointer to double frame to subtract */
  int *ivdark = NULL;   /* Pointer to int dark frame variance to subtract */
  double *dvdark = NULL;/* Pointer to double frame variance to subtract */

  double *dkbuf = NULL; /* malloced buffer for dark */
  double *dkvbuf = NULL; /* malloced buffer for dark variance */

  dim_t i;             /* loop counter */
  dim_t nbols;         /* number of bolometers */
  dim_t nslices;       /* number of time slices in input data */


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

  if ( ! indata->isTordered ) {
    *status = SAI__ERROR;
    errRep( " ", "Can not subtract dark from bolometer ordered data",
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

  /* darks must be 2d */
  if (dark1 && ( dark1->ndims != 2 ||
                 (dark1->ndims == 3 && (dark1->dims)[2] != 1) ) ) {
    *status = SAI__ERROR;
    errRep(" ", "Dark 1 is not 2d", status );
    return;
  }
  if (dark2 && ( dark2->ndims != 2 ||
                 (dark2->ndims == 3 && (dark2->dims)[2] != 1) ) ) {
    *status = SAI__ERROR;
    errRep(" ", "Dark 2 is not 2d", status );
    return;
  }

  /* calculate number of bolometers */
  nbols = (indata->dims)[0] * (indata->dims)[1];

  /* if we are choosing based on dark1 and dark2 existence then do it
     here */
  if (method == SMF__DKSUB_CHOOSE) {
    if (dark1 && dark2) {
      method = SMF__DKSUB_MEAN;
      msgSetc( "METH", "mean");
    } else if (dark1) {
      method = SMF__DKSUB_PREV;
      msgSetc( "METH", "previous");
    } else if (dark2) {
      method = SMF__DKSUB_NEXT;
      msgSetc( "METH", "following");
    } else {
      *status = SAI__ERROR;
      errRep(" ", "Could not choose a valid subtraction method", status );
      return;
    }
    msgOutif( MSG__VERB, " ", "Choosing to subtract the ^METH dark",
              status );
  }


  /* now do some sanity checks */
  switch (method) {

  case SMF__DKSUB_PREV:
    if (dark1 == NULL) {
      *status = SAI__ERROR;
      errRep( " ", "Requested subtraction of previous dark but it is NULL",
              status );
      return;
    }

    smf_select_pntr( dark1->pntr, dark1->dtype, &ddark, &dvdark,
                     &idark, &ivdark, status );
    break;

  case SMF__DKSUB_NEXT:
    if (dark2 == NULL) {
      *status = SAI__ERROR;
      errRep( " ", "Requested subtraction of following dark but it is NULL",
              status );
      return;
    }

    smf_select_pntr( dark2->pntr, dark2->dtype, &ddark, &dvdark,
                     &idark, &ivdark, status );
    break;

  case SMF__DKSUB_INTERP:
      *status = SAI__ERROR;
      errRep(" ", "Interpolation not yet supported for dark subtract", status);
      return;
  case SMF__DKSUB_MEAN:
    if (!dark1 || !dark2) {
      *status = SAI__ERROR;
      errRep( " ", "Requested mean dark but only one dark available",
             status );
      return;
    }
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
    int *idkpv1;
    double *ddkpv1;
    int *idkpv2;
    double *ddkpv2;

    smf_select_pntr( dark1->pntr, dark1->dtype, &ddkp1, &ddkpv1,
                     &idkp1, &idkpv1, status );
    smf_select_pntr( dark2->pntr, dark2->dtype, &ddkp2, &ddkpv2,
                     &idkp2, &idkpv2, status );
    dkbuf = astMalloc( nbols*sizeof(*dkbuf) );
    dkvbuf = astMalloc( nbols*sizeof(*dkvbuf) );
    ddark = dkbuf;
    dvdark = dkvbuf;

    if (ddkp1 && ddkp2) {
      for (i = 0; i < nbols; i++) {
        if (ddkp1[i] != VAL__BADD && ddkp2[i] != VAL__BADD) {
          /* worry about numerical overflow */
          dkbuf[i] = (0.5 * ddkp1[i]) + (0.5 * ddkp2[i]);
        } else {
          dkbuf[i] = VAL__BADD;
        }
        if (ddkpv1 && ddkpv2 && ddkpv1[i] != VAL__BADD && ddkpv2[i] != VAL__BADD) {
          /* worry about numerical overflow.. */
          dkvbuf[i] = ddkpv1[i] + ddkpv2[i];
        } else {
          dkvbuf[i] = VAL__BADD;
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
        if (idkpv1 && idkpv2 && idkpv1[i] != VAL__BADI && idkpv2[i] != VAL__BADI) {
          /* worry about numerical overflow */
          dkvbuf[i] = (double)idkpv1[i] + (double)idkpv2[i];
        } else {
          dkvbuf[i] = VAL__BADD;
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

  /* See how many time slices are in the data */
  if ( indata->ndims == 2 ) {
    nslices = 1;
  } else {
    nslices = (indata->dims)[2];
  }

  /* Now subtract the dark */
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
        double * dvar;
        int * idata;
        int * ivar;
        dim_t j;
        dim_t startidx;

        /* get the correct input data pointer */
        smf_select_pntr( indata->pntr, indata->dtype, &ddata, &dvar,
                         &idata, &ivar, status);

        if (ddata) {
          double * slice;
          double * vslice = NULL;

          for (i = 0; i < nslices; i++) {
            startidx = i * nbols;
            slice = &(ddata[startidx]);
            if (dvar) vslice = &(dvar[startidx]);
            for (j=0;  j < nbols; j++) {
              if (slice[j] != VAL__BADD) {
                double darkval = VAL__BADD;
                double darkvar = VAL__BADD;

                /* Get the relevant dark value */
                if (ddark) {
                  darkval = ddark[j];
                } else if (idark && idark[j] != VAL__BADI) {
                  darkval = (double)idark[j];
                }
                if (dvdark) {
                  darkvar = dvdark[j];
                } else if (ivdark && ivdark[j] != VAL__BADI) {
                  darkvar = (double)ivdark[j];
                }

                /* subtract it if non-bad */
                if (darkval != VAL__BADD ) {
                  slice[j] -= darkval;
                } else {
                  slice[j] = VAL__BADD;
                }
                /* add if non-bad */
                if (vslice) {
                  if (darkvar != VAL__BADD && vslice[j] != VAL__BADD) {
                    vslice[j] += darkvar;
                  } else {
                    vslice[j] = VAL__BADD;
                  }
                }
              }
            }
          }
        } else if (idata) {
          int *slice;
          int * vslice = NULL;

          for (i = 0; i < nslices; i++) {
            startidx = i * nbols;
            slice = &(idata[startidx]);
            if (ivar) vslice = &(ivar[startidx]);
            for (j=0;  j < nbols; j++) {
              if (slice[j] != VAL__BADI) {
                int darkval = VAL__BADI;
                int darkvar = VAL__BADI;

                /* Get the relevant dark value */
                if (idark) {
                  darkval = idark[j];
                } else if (ddark && ddark[j] != VAL__BADD) {
                  darkval = (int)ddark[j];
                }
                if (ivdark) {
                  darkvar = ivdark[j];
                } else if (dvdark && dvdark[j] != VAL__BADD) {
                  darkvar = (int)dvdark[j];
                }

                /* subtract it if non-bad */
                if (darkval != VAL__BADI) {
                  slice[j] -= darkval;
                } else {
                  slice[j] = VAL__BADI;
                }
                if (vslice) {
                  if (darkvar != VAL__BADI && vslice[j] != VAL__BADI) {
                    vslice[j] += darkvar;
                  } else {
                    vslice[j] = VAL__BADI;
                  }
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

  if (dkbuf) dkbuf = astFree( dkbuf );
  if (dkvbuf) dkvbuf = astFree( dkvbuf );

  return;
}
