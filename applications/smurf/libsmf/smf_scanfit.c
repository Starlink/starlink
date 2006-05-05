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
*     subtraction.

*  Notes:
*      - Polynomial orders other than 1 (ie a straight line) are not yet
*        supported

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-02 (AGG):
*        Initial version, stripped out code from original simulator
*        version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

#include "sae_par.h"
#include "star/ndg.h"
#include "ndf.h"
#include "ast.h"
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "mers.h"
#include "star/kaplibs.h"
#include "kpg_err.h"

#include "smf.h"
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "smf_err.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2math.h"

#define FUNC_NAME "smf_scanfit"

void smf_scanfit ( smfData *data, int order, int *status) {

  int cliptype;             /* Type of sigma clipping */
  double *poly;             /* Array of polynomial coefficients */
  smfFile *file;            /* Pointer to input file struct */
  int i;                    /* Loop counter */
  int indf;                 /* NDF identifier for input file */
  int lbnd[3];              /* Lower bound for coeff array (poly) */
  int nbol;                 /* Number of bolometers */
  int ncoeff;               /* Number of coefficients in baseline fit */
  int nframes = 1;          /* Number of frames in a scan */
  int npts;                 /* Number of data points in coefficient array */
  int place;                /* Place holder for SCANFIT extension */
  HDSLoc *ploc = NULL;      /* Locator for SCANFIT coeffs */
  int pndf;                 /* NDF identifier for SCANFIT */
  int scu2red;              /* NDF identifier for SCU2RED extension */
  int ubnd[3];              /* Upper bound for coeff array (poly) */

  if ( *status != SAI__OK ) return;

  /* HACK: check value of order, issue warning and reset to 1 until we
     have a polynomial fitting routine */
  if ( order != 1 ) {
    msgSeti("O",order);
    msgOut(FUNC_NAME, "Sorry polynomial of order ^O is not yet supported. Reverting to a linear fit (order = 1).", status);
    order = 1;
  }
  /* Set the number of coefficients */
  ncoeff = order + 1;

  /* Check history for scanfit */
  if ( smf_history_check( data, FUNC_NAME, status ) ) {
    msgOutif( MSG__VERB, FUNC_NAME, "Data have been fitted already: assuming that a recalculation is desired", status );
  }

  /* Also check for sky removal and extinction correction since it
     makes no sense to carry out a fit to these data */
  if ( smf_history_check( data, "smf_subtract_poly", status) ||
       smf_history_check( data, "smf_subtract_plane", status) ) {
    msgOutif( MSG__VERB, FUNC_NAME, "Data have been sky-subtracted already, will not perform fit", status );
    return;
  }
  if ( smf_history_check( data, "smf_correct_extinction", status) ) {
    msgOutif( MSG__VERB, FUNC_NAME, "Data have been extinction corrected already, will not perform fit", status);
    return;
  }

  /* Check we have 3-d timeseries data */
  if ( data->ndims == 3) {
    nframes = (data->dims)[2];
  } else {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti("N", data->ndims );
      errRep(FUNC_NAME, "Input data does ^N dimensions, should be 3", status );
    }
  }

  /* Check if scanfit cpt is already present by presence of poly
     pointer in smfData */
  if ( data->poly != NULL ) {
    msgOutif( MSG__VERB, FUNC_NAME, "Data have been fitted already: assuming that a recalculation is desired", status );
  }

  nbol = (data->dims)[0] * (data->dims)[1];
  cliptype = 0;

  /* Retrieve NDF identifier from smfFile */
  file = data->file;
  indf = file->ndfid;
  /* See if SCU2RED extension exists */
  ndfXstat( indf, "SCU2RED", &scu2red, status );
  if ( scu2red ) {
    /* OK, get a locator for it */
    ndfXloc( indf, "SCU2RED", "WRITE", &ploc, status );
    if ( ploc == NULL ) {
      if ( *status == SAI__OK) {
	    *status = SAI__ERROR;
	    errRep(FUNC_NAME, "Unable to obtain an HDS locator to the SCU2RED extension, despite its existence", status);
      }
    }

    /* Open SCANFIT extension */
    /* Note: write access clears the contents of the NDF */
    ndfOpen( ploc, "SCANFIT", "WRITE", "UNKNOWN", &pndf, &place, status );
    /* Open new SCANFIT extension */

    /* Set the lower and upper bounds of the 3-d array */
    for (i=0; i<3; i++) {
      lbnd[i] = 1;
    }
    ubnd[0] = (data->dims)[0];
    ubnd[1] = (data->dims)[1];
    ubnd[2] = ncoeff;
    
    /* Define properties of new extension */
    ndfNew( "_DOUBLE", 3, lbnd, ubnd, &place, &pndf, status );

    /* Map the pointer for polynomial coefficients */
    ndfMap( pndf, "DATA", "_DOUBLE", "WRITE", &poly, &npts, status );

  } else {
    msgOutif(MSG__VERB, FUNC_NAME, 
	     "Warning: no SCU2RED extension. Creating a new one.", status);
    /* Create scu2red extension */
    /* Create scanfit extension inside scu2red */
  }

  /* Carry out fit, check status on return */
  sc2math_fitsky ( cliptype, nbol, nframes, ncoeff, (data->pntr)[0],
		   poly, status );
  if ( *status != SAI__OK ) {
    errRep(FUNC_NAME, "Unable to carry out scanfit", status);
  }

  /* Release the NDF resources */
  ndfAnnul( &pndf, status );
  datAnnul( &ploc, status );

  /* Add history entry */
  smf_history_add( data, FUNC_NAME, "Scanfit completed successfuly", status );
}
