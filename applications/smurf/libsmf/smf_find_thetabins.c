/*
*+
*  Name:
*     smf_find_thetabins

*  Purpose:
*     Identify predominant scan angles for a smfData and return ranges

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_find_thetabins( const smfData *data, int nosign, double **bins,
*                         double **bincen, dim_t *nbin, dim_t **whichbin, status )

*  Arguments:
*     data = const smfData * (Given)
*        smfData containing a theta component giving the scan directions
*     nosign = int (Given)
*        If set, don't consider the sign of the scan along a given direction
*        (e.g. scans at an angle of +pi will be considered the same as -pi)
*     bins = double * (Returned)
*        Address of pointer that will be set to newly created array of
*        "nbin" bin edges -- note that the starting edge of the first bin
*        is the same as the terminating edge of the last bin (radians)
*     bincen = double * (Returned)
*        Address of pointer that will be set to newly created array of
*        "nbin" bin centres (the peak scan directions) (radians)
*     nbin = dim_t * (Returned)
*        Address of pointer that will be set to the number of bins.
*     whichbin = dim_t ** (Returned)
*        Address of pointer that will be set to newly created array of
*        length ntslices indicating which bin a given tslice falls in
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     In order to produce maps for each predominant scan angle in the
*     data, it is first necessary to identify what those scan angles
*     are, and then define ranges centered over these values to ensure
*     that every sample lies in one of the bins. This is accomplished
*     by producing a histogram of the data, and identifying peaks (the
*     predominant scan directions), and then defining bin edges that
*     are equidistant between these peaks.  To avoid producing maps
*     for very small subsets of the data at strange angles
*     (e.g. turnarounds), a threshold peak value that is a fraction of
*     the maximum peak is applied. If nosign is set, the sign of the
*     scan direction is ignored (e.g. a scan at an angle of +pi will
*     be considered the same as a scan at -pi). If whichbin is requested,
*     a new array will the allocated containing an integer index for each
*     time slice indicating to which theta bin it belongs.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)

*  History:
*     2010-11-12 (EC):
*        Initial Version
*     2010-11-15 (EC):
*        At whichbins argument

*  Copyright:
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_find_thetabins"

#define THETABINS_MAXBINS 64    /* Fixed size of initial histogram */
#define THETABINS_THRESH 0.1    /* Require at least this fraction of max */

void smf_find_thetabins( const smfData *data, int nosign, double **bins,
                         double **bincen, dim_t *nbin, dim_t **whichbin,
                         int *status ) {

  /* Local Variables */
  double *bc=NULL;              /* Bin centres */
  double *bin=NULL;             /* Bin edges */
  double dist;                  /* distance between local maxima */
  dim_t i;                      /* Loop counter */
  dim_t j;                      /* Loop counter */
  hdsdim hist[THETABINS_MAXBINS];  /* Histogram of peak scan directions */
  dim_t maxcount;               /* Maximum value of histogram */
  double max;                   /* Range of histogram */
  double min;                   /* Range of histogram */
  dim_t nb;                     /* Number of output bins */
  dim_t ntslice;                /* Number of time slices */
  double range;                 /* range of angles */
  double step;                  /* step size in histogram */
  double *theta=NULL;           /* Work array of scan angles */

  if( *status != SAI__OK ) return;

  if( !data || !data->theta ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": null data, or data do not contain theta array",
            status );
    return;
  }

  smf_get_dims( data, NULL, NULL, NULL, &ntslice, NULL, NULL, NULL, status );

  if( nosign ) {
    /* If nosign set, make a copy of theta, and map scan angles between -pi
       and 0 on to the interval 0 to +pi */

    min = 0;
    max = AST__DPI;

    theta = astCalloc( ntslice, sizeof(*theta) );
    for( i=0; i<ntslice; i++ ) {
      if( data->theta[i] == VAL__BADD ) {
        theta[i] = VAL__BADD;
      } else {
        theta[i] = fmod(data->theta[i] + AST__DPI, AST__DPI);
      }
    }

  } else {
    /* Otherwise use theta as-is and full range of angles */

    min = -AST__DPI;
    max = AST__DPI;

    theta = data->theta;
  }

  range = max - min;

  /* Calculate the histogram */
  kpg1Ghst8d( 1, ntslice, theta, NULL, 0.0, THETABINS_MAXBINS, 0, &max, &min, hist,
              status );

  step = (max-min)/THETABINS_MAXBINS;

  /* Identify the maximum */
  maxcount = 0;
  for( i=0; i<THETABINS_MAXBINS; i++ ) {
    if( hist[i] > maxcount ) maxcount = hist[i];
  }

  /* Find all of the local maxima above (THETABINS_THRESH * maxcount). When
     doing this, account for wraparound in the angle. */

  bc = astCalloc( THETABINS_MAXBINS, sizeof(*bc) );
  nb = 0;

  for( i=0; (*status==SAI__OK) && (i<THETABINS_MAXBINS); i++ ) {
    if( (hist[i] >= THETABINS_THRESH * maxcount) &&
        (hist[i] > hist[ (i==0) ? (THETABINS_MAXBINS-1) : (i-1) ]) &&
        (hist[i] > hist[ (i==(THETABINS_MAXBINS-1)) ? 0 : (i+1) ]) ) {

      bc[nb] = min + ((double) i + 0.5)*step;
      nb++;
    }
  }

  /* if there were zero bins found (maybe all angles are bad) then
     we have to abort. Do not complain though. */
  if (nb == 0) goto CLEANUP;

  bc = astRealloc( bc, nb*sizeof(*bc) );

  /* Then calculate bin edges that are equidistant between the peaks (again,
     account for wraparound */

  bin = astCalloc( nb, sizeof(*bin) );

  if( *status == SAI__OK) {
    /* The first bin deals with wraparound */
    dist = bc[0] + range - bc[nb-1];

    bin[0] = fmod(bc[0] + range - dist/2, range);

    /* Remaining bins -- remember, last bin edge is same as the first
       one due to wraparound */
    for( i=1; i<nb; i++ ) {
      dist = bc[i] - bc[i-1];
      bin[i] = bc[i] - dist/2;
    }
  }

  /* Finally, assign each time slice to the appropriate bin */

  if( whichbin && (*status==SAI__OK) ) {
    dim_t which;
    int wrapstart=0;   /* flag if first bin is wrapped. Otherwise last */

    *whichbin = astCalloc( ntslice, sizeof(**whichbin) );

    if( nb == 1 ) {
      /* Special case: if only 1 bin, all samples go into it */

      for( i=0; i<ntslice; i++ ) {
        if( theta[i] == VAL__BADD ) {
          (*whichbin)[i] = VAL__BADK;
        } else {
          (*whichbin)[i] = 0;
        }
      }
    } else {
      /* Otherwise full treatment */

      if( bin[0] > bin[1] ) {
        wrapstart = 1;
      }

      if( *status == SAI__OK ) {
        for( i=0; i<ntslice; i++ ) {

          if( theta[i] == VAL__BADD ) {
            (*whichbin)[i] = VAL__BADK;
          } else {

            which = -1;

            /* Try to find within non-wrapped bin */
            for( j=wrapstart; j<(nb - !wrapstart); j++ ) {
              if( (theta[i] >= bin[j]) && (theta[i] < bin[(j+1)%nb]) ) {
                which = j;
                break;
              }
            }

            /* If we get here without finding a bin, it's the wrapped one */
            if( which == -1 ) which = (nb-1)*(!wrapstart);

            (*whichbin)[i] = which;
          }
        }
      }
    }
  }

 CLEANUP:
  /* Free theta if it was allocated here */
  if( nosign && theta ) theta = astFree( theta );

  /* Return values and/or free */
  if( bincen ) {
    *bincen = bc;
  } else {
    bc = astFree( bc );
  }

  if( bins ) {
    *bins = bin;
  } else {
    bin = astFree( bin );
  }

  if( nbin ) *nbin = nb;
}
