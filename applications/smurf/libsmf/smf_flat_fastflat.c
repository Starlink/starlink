/*
*+
*  Name:
*     smf_flat_fastflat

*  Purpose:
*     Convert a fast flat ramp to a standard flatfield data set

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     void smf_flat_fastflat( const smfData * fflat,
*                smfData **bolvald, int *status );

*  Arguments:
*     fflat = const smfData * (Given)
*        smfData containing flatfield ramp data.
*     bolvald = smfData ** (Returned)
*        Collapsed flatfield data. First two dimensions match "fflat"
*        and the 3rd dimension will match the number of distinct heater
*        measurements. Data have not been shifted to standard power values.
*        The heater values themselves will be returned in the "heatval"
*        entry of the associated smfDA struct.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a fast flatfield ramp, select all the data with identical
*     heater settings, calculate the mean and standard deviation (without
*     correcting for resistance) and stores the result and the heater
*     settings in a standard form suitable for processing by the flatfield
*     routines.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - To simplify the API the heater settings for each flatfield
*       reading are returned in the smfDa component of "bolvald" using
*       the "heatval" slot.
*     - See also smf_flat_standardpow and smf_flat_polyfit.

*  History:
*     2010-03-03 (TIMJ):
*        Initial version
*     2010-03-11 (TIMJ):
*        Compensate for sky variation during ramp by subtracting a
*        polynomial fit to the reference heater values.
*     2010-03-12 (TIMJ):
*        Allow a single ramp of a single measurment to propogate through.
*        Variance on the single is not used even if present.

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "ast.h"
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

int smf__sort_ints ( const void * a, const void * b );

void smf_flat_fastflat( const smfData * fflat, smfData **bolvald, int *status ) {

  size_t bstride = 0;         /* Bolometer stride */
  smfHead * hdr = NULL;       /* Local header of fflat */
  AstKeyMap * heatmap = NULL; /* KeyMap of heater settings */
  int heatref = 0;            /* Reference heater setting */
  char heatstr[20];           /* Buffer for heater settings as strings */
  int * heatval = NULL;       /* Heater values in sorted order */
  size_t i = 0;
  size_t maxfound = 0;        /* Maximum number found */
  dim_t nbols = 0;            /* number of bolometers */
  size_t nheat = 0;           /* Number of distinct heater settings */
  dim_t nframes = 0;          /* Total number of frames in fflat */
  const size_t skyorder = 3;  /* Order to use for sky correction */

  size_t tstride = 0;         /* Time stride */

  if (*status != SAI__OK) return;

  if (!smf_validate_smfData( fflat, 1, 0, status) ) return;

  if ( ! fflat->isTordered ) {
    *status = SAI__ERROR;
    errRep( "", "Flatfield ramp data must be time ordered", status );
    return;
  }

  if (fflat->ndims != 3 || (fflat->ndims == 3 && (fflat->dims)[2] < 3)) {
    *status = SAI__ERROR;
    errRep( "", "A flatfield ramp must be 3 dimensional and have at least 3 measurements",
            status );
    return;
  }

  /* Check that we are dealing with integers */
  if ( !smf_dtype_check_fatal( fflat, NULL, SMF__INTEGER, status ) ) return;

  /* Local copies of pointers */
  hdr = fflat->hdr;

  /* get properties of data array */
  smf_get_dims( fflat, NULL, NULL, &nbols, &nframes, NULL, &bstride, &tstride, status );

  /* Find the reference heater setting */
  smf_fits_getI( hdr, "PIXHEAT", &heatref, status );
  msgOutiff( MSG__NORM, " ", "Reference heater setting: %d", status, heatref );

  /* First analyse the heater settings to see how many distinct values we have. We are going
     to use a KeyMap and treat this whole thing like we would a perl hash. Store the indices
     of each repeat of a given heater setting in a vector . */
  heatmap = astKeyMap( " " );
  maxfound = 0;
  for ( i = 0; i < nframes; i++ ) {
    JCMTState * tmpstate = &(hdr->allState)[i];
    size_t counter = 0;
    sprintf( heatstr, "%d", tmpstate->sc2_heat );

    /* push the current index on to the end */
    astMapPutElemI( heatmap, heatstr, -1, i );

    /* and ask how many elements we have now */
    counter = astMapLength( heatmap, heatstr );

    /* and record the largest number */
    if (counter > maxfound) maxfound = counter;

  }

  msgOutiff( MSG__VERB, "", "Fast flatfield ramp used %d heater settings, each of %zd measurements",
             status, astMapSize( heatmap ), maxfound );

  /* See how many distinct heater values we have */
  nheat = astMapSize( heatmap );

  /* sort the heater settings in an integer array */
  heatval = smf_malloc( nheat, sizeof(*heatval), 1, status );
  if (*status == SAI__OK) {
    for (i = 0; i < nheat; i++ ) {
      int h;
      const char * key = astMapKey( heatmap, i );
      /* this must work, we can miss this step if we had another
         keymap that had this key and an integer value ! */
      h = strtol( key, NULL, 0 );

      /* should never get a 0 for heater value */
       if ( h == 0 ) {
        *status = SAI__ERROR;
        errRepf("", "Unexpected failure to parse '%s' to a integer", status,
                key );
        break;
      }

      heatval[i] = h;

    }

    /* sort */
    qsort( heatval, nheat, sizeof( *heatval ), smf__sort_ints );


  }

  /* Now we need to create a bolval smfData to match the standard
     flatfield data measurements */
  smf_flat_malloc( nheat, fflat, NULL, bolvald, status );

  if (*status == SAI__OK) {
    double * bolval = (*bolvald)->pntr[0];
    double * bolvalvar = (*bolvald)->pntr[1];
    double *coeff = NULL;
    double *coeffvar = NULL;
    int * indices = NULL;
    int * idata = NULL;
    int * ffdata = (fflat->pntr)[0];
    JCMTState * instate = hdr->allState;
    JCMTState * outstate = NULL;
    double * skycoeffs = NULL;
    double * skycoeffsvar = NULL;
    int nind = 0;
    size_t bol;

    /* get some memory for the indices */
    indices = smf_malloc( maxfound, sizeof(*indices), 1, status );

    /* and equivalent memory for the readings at each index */
    idata = smf_malloc( maxfound, smf_dtype_sz( fflat->dtype, status ),
                       1, status );

    /* Need some memory for the JCMTSTATE information. */
    outstate = smf_malloc( nheat, sizeof(*outstate), 0, status );
    (*bolvald)->hdr->allState = outstate;

    /* First need to compensate for any drift in the DC sky level.
       We do this by looking at the heater measurements for the reference
       heater as a function of time (index) for each bolometer and then
       fitting it with a polynomial. */
    skycoeffs = smf_malloc( nbols * (skyorder+1), sizeof(*skycoeffs), 1, status );
    skycoeffsvar = smf_malloc( nbols * (skyorder+1), sizeof(*skycoeffs), 1, status );

    /* temp memory to hold the coefficients for a single bolometer */
    coeff = smf_malloc( skyorder + 1, sizeof(*coeff), 1, status );
    coeffvar = smf_malloc( skyorder + 1, sizeof(*coeffvar), 1, status );

    /* check status after memory allocation */
    if (*status == SAI__OK) {
      double * ddata = NULL;
      double * dindices = NULL;

      /* get the key based on the reference heater integer */
      sprintf( heatstr, "%d", heatref );

      /* and hence we can get all the relevant indices */
      astMapGet1I( heatmap, heatstr, maxfound, &nind, indices );

      /* and _DOUBLE versions because smf_fit_poly1d takes doubles */
      ddata = smf_malloc( maxfound, sizeof(*ddata), 1, status);
      dindices = smf_malloc( maxfound, sizeof(*dindices), 1, status );

      if (*status == SAI__OK) {
        for (bol = 0; bol < nbols; bol++) {
          int idx;
          size_t nused;

          for (idx = 0; idx < nind; idx++) {
            size_t slice = indices[idx];
            ddata[idx] = ffdata[ bol * bstride + slice*tstride ];
            dindices[idx] = slice;
          }

          if (nind > 1) {
            smf_fit_poly1d( skyorder, nind, 0, dindices, ddata, NULL,
                            coeff, coeffvar, NULL, &nused, status );

          } else {
            coeff[0] = ddata[0];
            coeffvar[0] = 0.0;
            for ( idx = 1; idx <= skyorder; idx++) {
              coeff[idx] = 0.0;
              coeffvar[idx] = 0.0;
            }
          }

          /* copy coefficients into array */
          for (idx = 0; idx <= skyorder; idx++) {
            skycoeffs[ bol + idx*nbols ] = coeff[idx];
            skycoeffsvar[ bol + idx*nbols ] = coeffvar[idx];
          }
        }
      }

      if (ddata) ddata = smf_free( ddata, status );
      if (dindices) dindices = smf_free( dindices, status );

      /* for each heater value we now need to calculate the measured signal */
      for ( i = 0; i < nheat; i++) {

        /* get the key based on this heater integer */
        sprintf( heatstr, "%d", heatval[i] );

        /* and hence we can get all the relevant indices */
        astMapGet1I( heatmap, heatstr, maxfound, &nind, indices );

        /* Copy state from the first entry */
        if (*status == SAI__OK) memcpy( &(outstate[i]), &(instate[indices[0]]), sizeof(*outstate));

        /* now we need to loop over each bolometer to calculate the statistics. */
        for ( bol = 0; bol < nbols; bol++) {
          double mean = VAL__BADD;
          double sigma = VAL__BADD;
          size_t ngood = 0;
          int idx;

          /* Get the polynomial data */
          for (idx = 0; idx <= skyorder; idx++) {
            coeff[idx] = skycoeffs[ bol + idx*nbols ];
            coeffvar[idx] = skycoeffsvar[ bol + idx * nbols ];
          }

          /* Obtain the measurements for that bolometer */
          for (idx = 0; idx < nind; idx++ ) {
            size_t slice = indices[idx];
            double poly = 0.0;
            idata[idx] = ffdata[ bol*bstride + slice*tstride ];

            if (idata[idx] != VAL__BADI) {
              /* calculate the reference value */
              EVALPOLY( poly, slice, skyorder, coeff );
              if (poly != VAL__BADD) idata[idx] -= poly;
              ngood++;
            }
          }

          if (nind > 1) {
            smf_stats1I( idata, 1, nind, NULL, 0, 0, &mean, &sigma, &ngood, status );
          } else {
            mean = idata[0];
            /* put in a small value rather than zero so that everything will be
               equally weighted. Currently the polynomial fitter thinks that 0
               variance is a bad point */
            sigma = 0.1;
          }

          /* store the answer */
          idx = bol + i*nbols;
          if (sigma == VAL__BADD || sigma == 0.0) {
            bolval[ idx ] = VAL__BADD;
            bolvalvar[ idx ] = VAL__BADD;
          } else {
            bolval[ idx ] = mean;
            bolvalvar[ idx ] = sigma * sigma;
          }
        }

      }
    }

    if (idata) idata = smf_free( idata, status );
    if (indices) indices = smf_free( indices, status );
    if (coeff) coeff = smf_free( coeff, status );
    if (coeffvar) coeffvar = smf_free( coeffvar, status );
    if (skycoeffs) skycoeffs = smf_free( skycoeffs, status );
    if (skycoeffsvar) skycoeffsvar = smf_free( skycoeffsvar, status );

  }

  /* Create a smfDA struct to store the heater settings. */
  if (*status == SAI__OK) {
    smfDA * da = NULL;
    double * dheatval = smf_malloc( nheat, sizeof(*dheatval), 0, status );

    for (i = 0; i < nheat; i++) {
      dheatval[i] = heatval[i];
    }

    da = smf_construct_smfDA( NULL, NULL, NULL, NULL,
                              SMF__FLATMETH_NULL, 0, dheatval, nheat, status );

    (*bolvald)->da = da;
  }


  if (heatval) heatval = smf_free( heatval, status );
  heatmap = astAnnul( heatmap );

}


int smf__sort_ints ( const void * a, const void * b ) {
  const int * ia;
  const int * ib;
  int temp;
  ia = a;
  ib = b;
  temp = *ia - *ib;
  if ( temp > 0 ) {
    return 1;
  } else if ( temp < 0 ) {
    return -1;
  } else {
    return 0;
  }
}
