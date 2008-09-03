/*
*+
*  Name:
*     smf_flat_responsivity

*  Purpose:
*     Calculate power relations for all bolometers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     void smf_flat_responsivity ( smfData *respmap, size_t nheat,
*                                  double *powval, double *bolval,  
*                                  int *status );

*  Arguments:
*     respmap = smfData * (Given & Returned)
*        smfData to receive the responsivity map. Must be _DOUBLE.
*        Will be assumed to be the correct size as compared to bolval.
*     nheat = size_t (Given)
*        Number of measurements. 3rd dimension of bolval. Size of powval.
*     powval = double [] (Given)
*        Resistance input powers. Must be nheat elements.
*     bolval = double [] (Given)
*        Response of each bolometer to powval. Dimensioned as number of 
*        number of bolometers (size of respmap) time nheat.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     For each bolometer calculate the current in amps equivalent to its data
*     numbers and so estimate the responsivity in Amps/Watt for each step in
*     power. Assume the response for a good bolometer should be nearly linear,
*     and so analyse the set of responsivities to determine the mean and
*     evaluate the quality. Write the results to a text file.

*  Notes:
*     powval and bolval are calculated by smf_flat_standardpow.

*  Authors:
*     BDK: Dennis Kelly (UKATC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-08-31 (BDK):
*        Original version
*     2007-09-04 (BDK):
*        Adjust conversion factor to amps.
*     2008-08-27 (TIMJ):
*        Rewrite for SMURF from sc2flat.c
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
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

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"

void smf_flat_responsivity ( smfData *respmap, size_t nheat,
                             double *powval, double *bolval,  
                             int *status ) {

  size_t dim0;                 /* first dimension in data array */
  size_t dim1;                 /* second dimension in data array */
  size_t i;                    /* loop counter */
  size_t j;                    /* loop counter */
  size_t k;                    /* loop counter */
  const double mcepass = 3.3;  /* factor for MCE low-pass filter */
  double mean;                 /* mean responsivity of a bolometer */
  size_t nbol;                 /* number of bolometers */
  size_t ngood = 0;            /* number of valid responsivities */
  int nrgood;                  /* number of good responsivities for bolo */
  double *respdata = NULL;     /* responsivity data */
  double *resps = NULL;        /* responsivities for a bolometer at each step */
  double *respvar = NULL;      /* responsivity variance */
  double stdev;                /* standard deviation of bolometer responsivity */

  int imin;                   /* Index where the pixel with the lowest value was 
                                 (first) found before clipping */
  double dmin;                /* Minimum pixel value in the array before clipping */
  int imax;                   /* Index where the pixel with the highest value was 
                                 (first) found before clipping*/
  double dmax;                /* Maximum pixel value in the array before clipping */
  double sum;                 /* Sum of valid pixels before clipping */
  int nrgoodc;                /* Number of valid pixels in the array after clipping */
  int iminc;                  /* Index where the pixel with the lowest value was 
                                 (first) found after clipping */
  double dminc;               /* Minimum pixel value in the array after clipping */
  int imaxc;                  /* Index where the pixel with the highest value was 
                                 (first) found after clipping */
  double dmaxc;               /* Maximum pixel value in the array after clipping */
  double sumc;                /* Sum of valid pixels after clipping */
  double meanc;               /* Mean of valid pixels after clipping */
  double stdevc;              /* Standard deviation of the above*/


  if (*status != SAI__OK) return;

  smf_dtype_check_fatal( respmap, NULL, SMF__DOUBLE, status );
  
  if (*status != SAI__OK) return;

  respdata = (respmap->pntr)[0];
  respvar  = (respmap->pntr)[1];

  resps = smf_malloc( nheat, sizeof(*resps), 0, status );

  dim0 = (respmap->dims)[0];
  dim1 = (respmap->dims)[1];
  nbol = dim0 * dim1;

  /* dim1 must change slower than dim0 */
  for (j=0; j < dim1; j++) {
    size_t frameoffset = j * dim0;

    for (i=0; i< dim0; i++) {
      /* offset into nbol array */
      size_t boloffset = frameoffset + i;

      /* Calculate the responsivity of this bolometer at each power step
         - note the use of k+1 */
      for ( k=0; k<nheat-1; k++ ) {
        if (bolval[(k+1)*nbol+boloffset] != VAL__BADD &&
            bolval[k*nbol+boloffset] != VAL__BADD &&
            powval[k+1] != VAL__BADD &&
            powval[k] != VAL__BADD) {
          resps[k] = mcepass * 1.52e-13 * 
            ( bolval[(k+1)*nbol+boloffset] - 
              bolval[k*nbol+boloffset] ) / 
            ( powval[k+1] - powval[k] );
        } else {
          resps[k] = VAL__BADD;
        }
      }

      /* Get statistics */
      kpgStatd(1, nheat-1, resps, 0, 0,
               &nrgood, &imin, &dmin, &imax, &dmax, &sum, &mean, &stdev,
               &nrgoodc, &iminc, &dminc, &imaxc, &dmaxc, &sumc, &meanc, &stdevc,
               status );

      /* store the responsivity, setting bad results to bad */

      if ( mean == VAL__BADD || stdev == VAL__BADD ||
           ( fabs(mean) > 5.0e6 ) || ( fabs(mean) < 0.1e6 ) || 
           ( stdev > 0.2*fabs(mean) ) ) {
        respdata[boloffset] = VAL__BADD;
      } else {
        respdata[boloffset] = mean;
        ngood++;
      }

      /* always store the sigma as variance */
      if (respvar) {
        double variance = VAL__BADD;
        if (stdev != VAL__BADD && stdev >= 0) {
          variance = stdev * stdev;
        }
        respvar[boloffset] = variance;
      }

    }

  }

  msgSeti( "NG", ngood );
  msgOut( " ", "Number of good responsivities: ^NG", status );

  if (resps) smf_free( resps, status );

}
