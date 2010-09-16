/*
*+
*  Name:
*     smf_scanfit

*  Purpose:
*     Open files and apply flatfield correction as necessary

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_scanfit( smfData *data, int order, int *status );

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to a pointer to smfData struct containing flatfielded data.
*        Will be created by this routine, or NULL on error.
*     order = int (Given)
*        Order of polynomial to be fitted
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This task carries out and stores a polynomial fit to the
*     bolometer time streams, which can then be used for sky
*     subtraction. Errors are printed if the polynomial order is
*     either negative or greater than the number of points (minus 1).

*  Notes:
*     Non-integer values for the order are truncated.

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-02 (AGG):
*        Initial version, stripped out code from original simulator
*        version
*     2006-05-15 (AGG):
*        - Checks for valid values of the polynomial order
*        - Now calls new smf_fit_poly routine
*     2008-03-25 (EC):
*        - Remove history check for extinction
*        - Handle both time and bolo-ordered data
*        - If smfData unassociated with file, just malloc poly buffer
*        - on exit, data->poly contains fitted coeff
*     2008-04-03 (EC):
*        - Added quality to interface
*     2008-12-03 (TIMJ):
*        Use smf_get_dims
*     2009-01-07 (EC)
*        Fix for bolo-ordered data
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006-2009 University of British Columbia. All Rights
*     Reserved.

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

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "star/ndg.h"
#include "ndf.h"
#include "ast.h"
#include "mers.h"
#include "star/kaplibs.h"
#include "kpg_err.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smf_err.h"
#include "smf_typ.h"

/* SC2DA includes */
#include "sc2da/sc2store.h"
#include "sc2da/sc2math.h"

#define FUNC_NAME "smf_scanfit"

void smf_scanfit( smfData *data, size_t order, int *status ) {

  int cliptype;             /* Type of sigma clipping */
  int i;                    /* Loop counter */
  int lbnd[3];              /* Lower bound for coeff array (poly) */
  dim_t nbol = 0;           /* Number of bolometers */
  int ncoeff;               /* Number of coefficients in baseline fit */
  dim_t ncols;              /* Number of cols */
  dim_t nframes = 1;        /* Number of frames in a scan */
  dim_t nrows;              /* Number of rows */
  int npts;                 /* Number of data points in coefficient array */
  HDSLoc *ploc = NULL;      /* Locator for SCANFIT coeffs */
  void *pntr[3];            /* Pointers to mapped data */
  int pndf;                 /* NDF identifier for SCANFIT */
  double *poly = NULL;      /* Array of polynomial coefficients */
  int ubnd[3];              /* Upper bound for coeff array (poly) */

  if ( *status != SAI__OK ) return;

  /* Set the number of coefficients */
  ncoeff = order + 1;

  /* Check history for scanfit */
  if ( smf_history_check( data, FUNC_NAME, status ) ) {
    msgOutif(MSG__VERB," ", "Data have been fitted already: "
             "assuming that a recalculation is desired", status );
  }

  /* Also check for sky removal and extinction correction since it
     makes no sense to carry out a fit to these data */
  if ( smf_history_check( data, "smf_subtract_poly", status) ||
       smf_history_check( data, "smf_subtract_plane", status) ) {
    msgOutif(MSG__VERB," ",
	     "Data have been sky-subtracted already, will not perform fit",
	     status );
    return;
  }

  /* Get the dimensions */
  smf_get_dims( data,  &nrows, &ncols, &nbol, &nframes, NULL, NULL, NULL,
                status);

  /* Return with error if order is greater than the number of data
     points */
  if ( order >= nframes ) {
    if ( *status == SAI__OK) {
      msgSeti("O",order);
      msgSeti("NF",nframes);
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Requested polynomial order, ^O, greater than or "
              "equal to number of points, ^NF. Unable to fit polynomial.",
              status );
      return;
    }
  }

  cliptype = 0;

  /* Regardless of whether the smfData is associated with file or not --
     on output smfData.poly is always a malloc'd buffer. First free
     it to remove any old values, and then malloc new space */

  data->poly = astFree( data->poly );
  data->poly = astCalloc( nbol*ncoeff, sizeof(*(data->poly)), 1 );
  if( *status == SAI__OK ) {
    data->ncoeff = ncoeff;
  }

  /* If file associated with NDF, map a poly array */
  if( data->file && (data->file->ndfid != NDF__NOID) && (*status == SAI__OK)) {

    /* Obtain the HDS locator for the SCU2RED extension */
    ploc = smf_get_xloc( data, "SCU2RED", "SCUBA2_MAP_ARR", "WRITE", 0, 0,
			 status);
    if ( ploc == NULL ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to obtain an HDS locator", status);
    }

    /* Set the lower and upper bounds of the 3-d array for the new
       SCANFIT NDF if necessary */
    for (i=0; i<3; i++) {
      lbnd[i] = 1;
    }
    ubnd[0] = nrows;
    ubnd[1] = ncols;
    ubnd[2] = ncoeff;

    /* Open SCANFIT extension - note if SCANFIT exists, opening it with
       WRITE access will overwrite the current contents */
    pndf = smf_get_ndfid( ploc, "SCANFIT", "WRITE", "UNKNOWN", "_DOUBLE", 3,
			  lbnd, ubnd, status );

    /* Check the returned NDF identifier */
    if ( pndf == NDF__NOID ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME,
	     "Unable to obtain an NDF identifier for SCANFIT coefficients",
	     status );
    } else {
      /* Map the pointer for polynomial coefficients */
      ndfMap( pndf, "DATA", "_DOUBLE", "WRITE", pntr, &npts, status );
      poly = pntr[0];
    }
  } else {
    /* Otherwise smfData not associated with file. Simply point poly to
       the malloc'd array */

    poly = data->poly;
  }


  /* Carry out fit, check status on return */
  /*    sc2math_fitsky ( cliptype, nbol, nframes, ncoeff, (data->pntr)[0],
	poly, status );*/

  smf_fit_poly ( data, order, 0, poly, status );
  if ( *status != SAI__OK ) {
    errRep(FUNC_NAME, "Unable to carry out scanfit", status);
  }


  /* Release the NDF resources if required, remembering to first memcopy
     over the fitted coefficients to the malloc'd buffer from the map'd
     buffer */

  if(data->file && (data->file->ndfid != NDF__NOID) && (*status == SAI__OK)) {
    memcpy( data->poly, poly, nbol*ncoeff*sizeof(*(data->poly)) );
    ndfAnnul( &pndf, status );
    datAnnul( &ploc, status );
  }

  /* Add history entry */
  smf_history_add( data, FUNC_NAME, status );
}
