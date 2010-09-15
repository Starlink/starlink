/*
*+
*  Name:
*     smf_fit_poly

*  Purpose:
*     Low-level polynomial fitting routine for bolometer data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_fit_poly( const smfData *data, const int order, double *poly,
*                   int *status )

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to input data struct
*     order = int (Given)
*        Order of polynomial fit
*     poly = double * (Returned)
*        Polynomial fit coefficients.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine fits a polynomial of arbitrary order to each
*     bolometer time stream of N timeslices. Execution is halted with
*     an error if the polynomial order is greater than N-1. Fitting
*     itself is done with smf_fit_poly1d.

*  Notes:
*     This routine will fail if there is no associated QUALITY component.
*     No sigma-clipping is carried out to refine the fit. This
*     accounts for any differences between this method and
*     sc2math_fitsky (for order = 1).

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-05-15 (AGG):
*        Initial test version
*     2006-05-15 (AGG):
*        Add check for non-NULL poly pointer
*     2008-03-17 (EC):
*        - Use QUALITY in addition to VAL__BADD to ignore bad data
*        - handle both time- and bolo-ordered data
*     2008-04-03 (EC):
*        - Added quality to interface
*     2008-12-03 (TIMJ):
*        Use smf_get_dims
*     2010-09-15 (EC):
*        Switch to using smf_fit_poly1d for fitting each bolo
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006-2008,2010 University of British Columbia.
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

/* Standard includes */
#include <stdio.h>
#include <string.h>

/* GSL includes */
#include "gsl/gsl_multifit.h"

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* SC2DA includes */
#include "sc2da/sc2math.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fit_poly"

void smf_fit_poly( const smfData *data,
                   const size_t order, double *poly, int *status) {

  /* Local variables */
  size_t bstride;             /* bolo strides */
  double *curbolo=NULL;       /* pointer to current bolo data */
  double *curpoly=NULL;       /* pointer to poly coeffs fit to curbolo */
  dim_t i;                    /* Loop counter */
  double *indata=NULL;        /* Pointer to data array */
  dim_t j;                    /* Loop counter */
  dim_t k;                    /* Loop counter */
  dim_t nbolo=0;              /* Number of bolometers */
  size_t ncoeff = 2;          /* Number of coefficients to fit for; def. line */
  dim_t ntslice = 0;          /* Number of time slices */
  size_t nused;               /* Number of samples used in fit */
  const smf_qual_t *qual=NULL;/* Pointer to QUALITY component */
  size_t tstride;             /* time strides */


  /* Check status */
  if (*status != SAI__OK) return;

  /* Should check data type for double */
  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status)) return;

  if ( smf_history_check( data, FUNC_NAME, status) ) {
    msgSetc("F", FUNC_NAME);
    msgOutif(MSG__VERB," ",
             "^F has already been run on these data, returning to caller",
             status);
    return;
  }

  /* Get the dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                &tstride, status);

  /* Check that poly is not a NULL pointer */
  if ( poly == NULL ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Input pointer for storing coefficients is NULL. "
             "Possible programming error.", status);
    }
    return;
  }

  /* Assign pointer to input data array */
  /* of course, check status on return... */
  indata = (data->pntr)[0];

  /* Return with error if there is no QUALITY component */
  qual = smf_select_cqualpntr( data, NULL, status );

  if( !qual && (*status == SAI__OK) ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Data doesn't have a QUALITY component.", status );
    return;
  }

  /* Return with error if order is greater than the number of data
     points */
  if ( order >= ntslice ) {
    if ( *status == SAI__OK) {
      msgSeti("O",order);
      msgSeti("NF",ntslice);
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Requested polynomial order, ^O, greater than or "
              "equal to the number of points, ^NF. Unable to fit polynomial.",
              status );
    }
    return;
  }

  ncoeff = order + 1;

  /* If time-ordered need to copy bolometer into contiguous arrays.
     Otherwise we can use the bolometer data in-place. Polynomial
     data per-bolo also needs to be re-ordered so allocate a temp array. */

  if( data->isTordered ) {
    curbolo = astCalloc( ntslice, sizeof(*curbolo), 0 );
  }
  curpoly = astCalloc( ncoeff, sizeof(*curpoly), 0 );

  /* Loop over bolometers. Only fit this bolometer if it is not
     flagged SMF__Q_BADB */
  for ( j=0; (j<nbolo) && (*status==SAI__OK); j++) {
    if( !(qual[j*bstride] & SMF__Q_BADB) ) {

      /* Pointer to current bolometer data */
      if( data->isTordered ) {
        for( i=0; i<ntslice; i++ ) {
          curbolo[i] = indata[i*tstride + j*bstride];
        }
      } else {
        curbolo = indata + j*bstride;
      }

      smf_fit_poly1d( order, ntslice, 0, NULL, curbolo, NULL, curpoly, NULL,
                      NULL, &nused, status );

      /* Copy the poly coefficients for this bolometer into poly */
      for ( k=0; k<ncoeff; k++) {
        poly[j + k*nbolo] = curpoly[k];
      }
    }
  }

  /* Free up temp space */
  if( data->isTordered ) curbolo = astFree( curbolo );
  curpoly = astFree( curpoly );

}
