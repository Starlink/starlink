/*
*+
*  Name:
*     smf_construct_smfDream

*  Purpose:
*     Populate a smfDream structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_construct_smfDream( smfData *data, int * status );

*  Arguments:
*     data = smfData* (Given)
*        smfData struct for the input raw DREAM data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_construct_smfDream = smfDream*
*        Pointer to newly created smfDream. NULL on error.

*  Description:
*     This function allocates memory for a smfDream structure and
*     copies in the supplied values. All of the necessary information
*     must be in the header of the supplied file.

*  Notes:
*     - Free this memory using smf_close_smfDream

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-26 (AGG):
*        Initial version
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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "dat_par.h"
#include "par.h"
#include "star/kaplibs.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* SC2DA includes */
#include "sc2da/sc2math.h"

#define FUNC_NAME "smf_construct_smfDream"

#define SZFITSCARD 81

smfDream *smf_construct_smfDream( smfData *data, int nvert, int npath, 
				  int *jigvert, double *jigpath, 
				  int * status ) {

  /* Local variables */
  int i;                     /* Loop counter */
  smfHead *hdr = NULL;       /* Header info */
  smfDream *dream = NULL;    /* DREAM struct to be filled */
  char obsmode[SZFITSCARD];  /* Observing mode */
  int genint;                /* Generic interger used multiple times */

  char wtfile[SZFITSCARD];   /* Name of weights file */
  smfData *griddata = NULL;
  double *invmatx = NULL;
  double *gridwts = NULL;
  size_t nelem;
  int ix;
  int jy;
  size_t k;
  int ndfid;
  double *gridptr = NULL;
  int *gridiptr = NULL;
  Grp *wtgrp = NULL;
  smfData *wtdata = NULL;
  HDSLoc *drmloc;
  int gridxmin;
  int gridxmax;
  int gridymin;
  int gridymax;

  if (*status != SAI__OK) return NULL;

  /* Check that we have time series data */
  if ( !(data->file->isTstream) ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Input data not time series unable to construct smfDream", status);
      return NULL;
    }
  }

  /* Check we have DREAM data */
  hdr = data->hdr;
  smf_fits_getS( hdr, "OBSMODE", obsmode, SZFITSCARD, status );
  if ( strncmp( obsmode, "DREAM", 5) == 0 ) {
    /* Check we have valid pointers to the SMU path and jiggle vertices */
    if ( jigpath != NULL && jigvert != NULL ) {
      dream = smf_create_smfDream( status );
      if ( dream != NULL ) {
	dream->nvert = (size_t)nvert;
	dream->nsampcycle = (size_t)npath;
	for (i=0; i<nvert; i++) {
	  (dream->jigvert)[i][0] = jigvert[i];
	  (dream->jigvert)[i][1] = jigvert[i+nvert];
	}
	for (i=0; i<npath; i++) {
	  (dream->jigpath)[i][0] = jigpath[i];
	  (dream->jigpath)[i][1] = jigpath[i+npath];
	}
	smf_fits_getD( hdr, "JIGSTEP", &(dream->jigstep), status );
	smf_fits_getI( hdr, "NJIGLCYC", &genint, status );
	dream->ncycles = (size_t)genint;
	smf_fits_getI( hdr, "JIGL_CNT", &genint, status );
	if ( genint != nvert ) {
	  if ( *status == SAI__OK ) {
	    *status = SAI__ERROR;
	    msgSeti("J",genint);
	    msgSeti("M",nvert);
	    errRep( FUNC_NAME, "Internal consistency check failed: "
		    "number of jiggle points in FITS header (^J) "
		    "not equal to number of mapped jiggle points (^M)", 
		    status);
	  }
	}

	/* Now read reconstruction grid parameters */
	/* Get weights file name from hdr */
	smf_fits_getS( hdr, "DRMWGHTS", wtfile, SZFITSCARD, status );
	if ( *status == SAI__OK ) {
	  /* Open file: place it in a Grp */
	  wtgrp = grpNew("DREAM weights", status);
	  if ( wtgrp != NULL ) {
	    grpPut1( wtgrp, wtfile, 0, status );
	    smf_open_file( wtgrp, 1, "READ", 0, &wtdata, status );
	    if ( *status == SAI__OK ) {
	      /* Get locator to DREAM parameters */
	      drmloc = smf_get_xloc( wtdata, "DREAM", "DREAM_WEIGHTS", "READ", 0, 
				     NULL, status);
	      if ( drmloc != NULL ) {
		/* Open each NDF, copy contents into smfDream */
		/* First, the grid weights array */
		ndfid = smf_get_ndfid(drmloc, "GRIDWTS", "READ", "OLD", "", 0, 
				      NULL, NULL, status);
		smf_open_ndf( ndfid, "READ", wtfile, SMF__DOUBLE, &griddata, status);
		if ( griddata != NULL ) {
		  nelem = (size_t)((griddata->dims)[0] * (griddata->dims)[1]);
		  gridwts = smf_malloc( nelem, sizeof(double), 1, status );
		  if ( gridwts != NULL ) {
		    gridptr = (griddata->pntr)[0];
		    memcpy( gridwts, gridptr, nelem*sizeof(double) );
		    dream->gridwts = gridwts;
		  }
		  smf_close_file( &griddata, status);
		}

		/* Then the inverse matrix */
		ndfid = smf_get_ndfid(drmloc, "INVMATX", "READ", "OLD", "", 0, 
				      NULL, NULL, status);
		smf_open_ndf( ndfid, "READ", wtfile, SMF__DOUBLE, &griddata, status);
		if ( griddata != NULL ) {
		  nelem = (size_t)((griddata->dims)[0]);
		  invmatx = smf_malloc( nelem, sizeof(double), 1, status );
		  if ( invmatx != NULL ) {
		    gridptr = (griddata->pntr)[0];
		    memcpy( invmatx, gridptr, nelem*sizeof(double) );
		    dream->invmatx = invmatx;
		  }
		  smf_close_file( &griddata, status);
		}

		/* Now for the gridpts array */
		ndfid = smf_get_ndfid(drmloc, "GRIDEXT", "READ", "OLD", "", 0, 
				      NULL, NULL, status);
		smf_open_ndf( ndfid, "READ", wtfile, SMF__INTEGER, &griddata, status);
		if ( griddata != NULL ) {
		  gridiptr = (griddata->pntr)[0];
		  gridxmin = gridiptr[0];
		  gridxmax = gridiptr[1];
		  gridymin = gridiptr[2];
		  gridymax = gridiptr[3];
		  dream->ngrid = (gridxmax - gridxmin + 1) * 
		    (gridymax - gridymin + 1);
		  k = 0;
		  for ( jy=gridymin; jy<=gridymax; jy++ ) {
		    for ( ix=gridxmin; ix<=gridxmax; ix++ ) {
		      (dream->gridpts)[k][0] = ix;
		      (dream->gridpts)[k][1] = jy;
		      k++;
		    }
		  }
		  smf_close_file( &griddata, status);
		}
		/* Annul locator, close file */
		datAnnul( &drmloc, status );
	      } else {
		if ( *status == SAI__OK ) {
		  *status = SAI__ERROR;
		  errRep(FUNC_NAME, 
			 "Unable to obtain locator to DREAM extension in weights file", 
			 status);
		}
	      }
	      smf_close_file( &wtdata, status );
	    }
	  }
	  if ( wtgrp != NULL ) {
	    grpDelet( &wtgrp, status );
	  }
	}
      } else {
	/* Add error message to stack */
	errRep(FUNC_NAME, "Error creating smfDream for raw data", status);
      }
    } else {
      if ( *status == SAI__OK ) {
	*status = SAI__ERROR;
	if ( jigpath == NULL) {
	  errRep(FUNC_NAME, "Error reading DREAM parameters from raw data: jigpath is returned NULL", status);
	} else if ( jigvert == NULL ) {
	  errRep(FUNC_NAME, "Error reading DREAM parameters from raw data: jigvert is returned NULL", status);
	}
      }
    }
  } else {
    dream = NULL;
  }


  return dream;
}
