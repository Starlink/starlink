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
*     dream = smf_construct_smfDream( smfData *data, int nvert,
*                                     dim_t nsampcycle, const int *jigvert,
*                                     const double *jigpath, int * status );

*  Arguments:
*     data = smfData* (Given)
*        smfData struct for the input raw DREAM data
*     nvert = int (Given)
*        Number of vertices in the DREAM jiggle pattern
*     nsampcycle = dim_t (Given)
*        Number of data samples in a single DREAM cycle
*     jigvert = const int * (Given)
*        Array of positions in the DREAM jiggle pattern
*     jigpath = const double * (Given)
*        Array of interpolated SMU positions over a single DREAM cycle
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_construct_smfDream = smfDream*
*        Pointer to newly created smfDream. NULL on error.

*  Description:
*     This function allocates memory for a smfDream structure and
*     copies in the supplied values. All of the necessary information
*     must be in the header of the supplied data file. This routine is
*     responsible for opening the weights file and retrieving all the
*     necessary information from it.

*  Notes:
*     - Free this memory using smf_close_smfDream
*     - This routine will not issue an error if the DREAM weights file
*       does not exist. Currently the caller should check that the
*       smfDream contains information from the weights file and issue
*       its own error.
*     - The smfData is verified to ensure that we are in DREAM mode.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-26 (AGG):
*        Initial version
*     2006-11-10 (AGG):
*        Update prologue, read GRID_SIZE from weights file
*     2007-04-05 (AGG):
*        Change OBSMODE to SAM_MODE
*     2007-10-19 (TIMJ):
*        Absence of SAM_MODE no longer fatal.
*     2007-10-29 (EC):
*        Modified interface to smf_open_file.
*     2007-10-31 (TIMJ):
*        Consistently use dim_t
*     2008-07-11 (AGG):
*        Allow for lower-case SAM_MODE
*     2008-07-24 (TIMJ):
*        Use hdr->obsmode instead of SAM_MODE.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2008 University of British Columbia.
*     Copyright (C) 2007 Science and Technology Facilities Council.
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

smfDream *smf_construct_smfDream( smfData *data, int nvert,
				  dim_t nsampcycle, const int *jigvert,
				  const double *jigpath, int * status ) {

  /* Local variables */
  smfDream *dream = NULL;    /* DREAM struct to be filled */
  HDSLoc *drmloc;            /* Locator to DREAM extension in weights file */
  smfData *griddata = NULL;  /* smfData for storing components in weights file */
  int *gridiptr = NULL;      /* Pointer to integer griddata data array */
  double *gridptr = NULL;    /* Pointer to double griddata data array  */
  double *gridwts = NULL;    /* Pointer to grid weights */
  int gridxmax;              /* Maximum pixel value in X for reconstruction grid */
  int gridxmin;              /* Minimum pixel value in X for reconstruction grid */
  int gridymax;              /* Maximum pixel value in Y for reconstruction grid */
  int gridymin;              /* Minimum pixel value in Y for reconstruction grid */
  smfHead *hdr = NULL;       /* Header info */
  double *invmatx = NULL;    /* Pointer to the inverse matrix */
  dim_t i;                  /* Loop counter */
  int ix;                    /* X pixel counter */
  int jy;                    /* Y pixel counter */
  dim_t k;                  /* Index into gridpts array */
  int ndfid;                 /* NDf identifier for current component */
  dim_t nelem;              /* Size of data array to be allocated */
  char weightsfile[SZFITSTR]; /* Name of weights file */
  smfData *wtdata = NULL;    /* smfData for weights file */
  smfFile *wtfile = NULL;    /* smfFile for weights file */
  Grp *wtgrp = NULL;         /* Grp containing name of weights file */

  if (*status != SAI__OK) return NULL;

  /* Check that we have time series data */
  if ( !(data->file->isTstream) ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
	      "Input data not time series unable to construct smfDream", status);
      return NULL;
    }
  }

  /* Check we have DREAM data */
  hdr = data->hdr;

  if ( hdr->obsmode == SMF__OBS_DREAM) {

    /* Check we have valid pointers to the SMU path and jiggle vertices */
    if ( jigpath != NULL && jigvert != NULL ) {
      dream = smf_create_smfDream( status );
      if ( dream != NULL ) {
	dream->nvert = nvert;
	dream->nsampcycle = (dim_t)nsampcycle;
	dream->ncycles = (dim_t)(data->dims)[2] / dream->nsampcycle;
	/* Retrieve jiggle positions */
	for (i=0; i<nvert; i++) {
	  (dream->jigvert)[i][0] = jigvert[i];
	  (dream->jigvert)[i][1] = jigvert[i+nvert];
	}
	/* Store SMU path over DREAM cycle */
	for (i=0; i<nsampcycle; i++) {
	  (dream->jigpath)[i][0] = jigpath[i];
	  (dream->jigpath)[i][1] = jigpath[i+nsampcycle];
	}
	/* Retrieve remaining parameters from the FITS header */
	smf_fits_getD( hdr, "JIG_SCAL", &(dream->jigscal), status );

	/* Now read reconstruction grid parameters */
	/* Get weights file name from hdr */
	smf_fits_getS( hdr, "DRMWGHTS", weightsfile, sizeof(weightsfile), status );
	if ( *status == SAI__OK ) {
	  /* Open file: place it in a Grp */
	  wtgrp = grpNew("DREAM weights", status);
	  if ( wtgrp != NULL ) {
	    grpPut1( wtgrp, weightsfile, 0, status );
	    /* Note that this call to smf_open_file should not result
	       in a circular loop because the weights data do not
	       satisfy the condition for reading and storing them in a
	       smfDream. */
	    smf_open_file( NULL, wtgrp, 1, "READ", SMF__NOCREATE_HEAD, &wtdata,
			   status );
	    if ( *status == SAI__OK ) {
	      /* Get locator to DREAM parameters */
	      drmloc = smf_get_xloc( wtdata, "DREAM", "DREAM_WEIGHTS", "READ", 0,
				     NULL, status);
	      if ( drmloc != NULL ) {
		/* Open each NDF, copy contents into smfDream */
		/* First, the grid weights array */
		ndfid = smf_get_ndfid(drmloc, "GRIDWTS", "READ", "OLD", "", 0,
				      NULL, NULL, status);
		smf_open_ndf( ndfid, "READ", SMF__DOUBLE,
                              &griddata, status);
		if ( griddata != NULL ) {
		  nelem = (dim_t)((griddata->dims)[0] * (griddata->dims)[1]);
		  gridwts = astCalloc( nelem, sizeof(*gridwts) );
		  if ( gridwts != NULL ) {
		    gridptr = (griddata->pntr)[0];
		    memcpy( gridwts, gridptr, nelem*sizeof(*gridwts) );
		    dream->gridwts = gridwts;
		  }
		  smf_close_file( NULL, &griddata, status);
		}

		/* Then the inverse matrix */
		ndfid = smf_get_ndfid(drmloc, "INVMATX", "READ", "OLD", "", 0,
				      NULL, NULL, status);
		smf_open_ndf( ndfid, "READ", SMF__DOUBLE, &griddata, status);
		if ( griddata != NULL ) {
		  nelem = (dim_t)((griddata->dims)[0]);
		  invmatx = astCalloc( nelem, sizeof(*invmatx) );
		  if ( invmatx != NULL ) {
		    gridptr = (griddata->pntr)[0];
		    memcpy( invmatx, gridptr, nelem*sizeof(*invmatx) );
		    dream->invmatx = invmatx;
		  }
		  smf_close_file( NULL, &griddata, status);
		}

		/* Now for the gridpts array */
		ndfid = smf_get_ndfid(drmloc, "GRIDEXT", "READ", "OLD", "", 0,
				      NULL, NULL, status);
		smf_open_ndf( ndfid, "READ", SMF__INTEGER, &griddata, status);
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
		  smf_close_file( NULL, &griddata, status);
		}
		/* And finally, read the size of the grid step */
		wtfile = wtdata->file;
		ndfXgt0d( wtfile->ndfid, "DREAM", "GRID_SIZE", &dream->gridstep,
			  status );
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
	      smf_close_file( NULL, &wtdata, status );
	    } else {
	      /* If we fail to open the file, can we assume we're trying
		 to create it? */
	      msgOutif(MSG__VERB," ",
		       "Could not open the weights file - maybe we're creating it?",
		       status);
	      errAnnul( status );
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
