/*
*+
*  Name:
*     smf_flat_precondition

*  Purpose:
*     Removes bad data from flatfield observations

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_flat_precondition( int allbad, smfData *powref, smfData *bolref,
*                            int *status );

*  Arguments:
*     allbad = int (Given)
*        Used when doing the gradient check. If true when a bad gradient
*        is detected the entire bolometer is masked as bad. If false, the
*        bad data point will be set bad but the rest of the bolometer will
*        remain. Should be true for TABLE mode and false for POLYNOMIAL
*        mode.
*     powref = smfData* (Given)
*        Power supplied for each flatfield measurement.
*     bolref = smfData* (Given)
*        Response of each bolometer to the supplied power. Dimensioned
*        as number of bolometers times number of flatfield measurements.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*      Analyses the flatfield time series for anomalous readings and remove
*      them. Currently checks for consistent gradient in all measurements
*      for each bolometer.

*  Notes:
*      Do not call this routine with the POLYNOMIAL flatfield solution.
*      It should be called with the output of smf_flat_standardpow
*      not with the output of smf_flat_fitpoly.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-02-05 (TIMJ):
*        Original version.
*     {enter_further_changes_here}

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "prm_par.h"
#include "sae_par.h"

void
smf_flat_precondition( int allbad, smfData * powvald, smfData * bolvald, int *status ) {

  dim_t bol;                  /* bolometer index */
  double *grad = NULL;         /* calculated gradient for each point */
  dim_t k;                    /* loop counter */
  dim_t nbol;                 /* number of bolometers */
  dim_t nheat;                /* number of heater measurements */

  double * powval = NULL;      /* pointer to data in smfData */
  double * bolval = NULL;      /* pointer to data in smfData */

  if (*status != SAI__OK) return;

  /* Extract relevant information from the smfData */
  powval = (powvald->pntr)[0];
  bolval = (bolvald->pntr)[0];

  nheat = (powvald->dims)[0];
  nbol = (bolvald->dims)[0] * (bolvald->dims)[1];

  /* store the gradient points somewhere */
  grad = astCalloc( nheat, sizeof(*grad) );

  /* naive gradient validator */
  for (bol=0; bol < nbol; bol++) {
    double refgrad;
    double firstbol = VAL__BADD;
    double firstpow = VAL__BADD;
    double lastbol = VAL__BADD;
    double lastpow = VAL__BADD;

    for ( k = 0; k<nheat-1; k++ ) {
      if ( bolval[k*nbol+bol] != VAL__BADD ) {
        if ( firstpow == VAL__BADD ) {
          firstpow = powval[k];
          firstbol = bolval[k*nbol+bol];
        }
        if (bolval[(k+1)*nbol+bol] != VAL__BADD) {
          lastpow = powval[k+1];
          lastbol = bolval[k*nbol+bol];

          /* calculate gradient */
          grad[k] = ( bolval[(k+1)*nbol+bol] - bolval[k*nbol+bol] ) /
            ( powval[k+1] - powval[k] );

          /* ..but only store its sign */
          grad[k] /= fabs(grad[k]);

        }
      } else {
        grad[k] = VAL__BADD;
      }
    }

    /* now we have gradients so calculate a reference gradient */
    if (lastbol != VAL__BADD && firstbol != VAL__BADD ) {
      refgrad = (lastbol - firstbol ) / (lastpow - firstpow );

      /* only care about the sign */
      refgrad /= fabs(refgrad);

      if (refgrad == 0.0 ) {
        /* flat so do not trust it  */
        for (k = 0; k<nheat; k++) {
          bolval[k*nbol+bol] = VAL__BADD;
        }
      } else {
        int flagged = 0;

        for (k=0; k<nheat; k++) {
          /* check for different sign */
          if ( refgrad * grad[k] < 0 ) {

            /* but we have to know whether to mask k or k+1.
               If the previous point is good then the gradient from k-1 to k
               was actually fine so that probably means that k should be retained.
             */
            if (k > 0 && bolval[(k-1)*nbol+bol] != VAL__BADD) {
              bolval[(k+1)*nbol+bol] = VAL__BADD;
            } else {
              bolval[k*nbol+bol] = VAL__BADD;
            }
            flagged = 1;
          }
        }

        /* turn off whole bolometer if required */
        if (allbad && flagged) {
          for (k=0; k<nheat; k++) {
            bolval[k*nbol+bol] = VAL__BADD;
          }
        }
      }

    } else {
      /* bolometer must be bad already */

    }
  }

  if (grad) grad = astFree( grad );

}
