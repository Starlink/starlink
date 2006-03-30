/*
*+
*  Name:
*     smf_flatfield

*  Purpose:
*     Mid-level FLATFIELD implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_flatfield( smfData *idata, smfData **odata, int *status );

*  Arguments:
*     idata = smfData* (Given)
*        Pointer to a smfData struct
*     odata = smfData** (Given and Returned)
*        Pointer to a smfData struct. If *odata is NULL a new smfData*
*        will be created and stored in this location.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is a handler routine for determining if the lower-level
*     FLATFIELD task needs to be run. 

*  Notes:
*     - If an NDF file is involved then at the very least, the pointer
*     to the DATA array (pntr[0]) and its type (dtype) must be
*     defined.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-12-01 (AGG):
*        Initial test version.
*     2005-12-12 (AGG):
*        Add checks on data type and dimensions for the case that the
*        data need flatfielding and odata exists
*     2005-12-14 (AGG):
*        Now calls smf_clone_data for when the data are already flatfielded
*     2005-12-16 (AGG):
*        All combinations of flatfield & *odata statuses covered
*     2005-12-20 (AGG):
*        Further checks are now carried out to see if the WCS
*        component contains a sky frame
*     2005-12-21 (AGG):
*        Now deals with time series data correctly by not attempting
*        to copy the non-existent WCS info (which is added by
*        smf_tslice_ast)
*     2006-01-25 (AGG):
*        Add check on whether input header is null for unflatfielded data
*     2006-01-25 (TIMJ):
*        Replace malloc with smf_malloc
*     2006-01-27 (TIMJ):
*        - No longer have xloc member in smfFile
*        - use smf_create_smfHead rather than smf_malloc
*     2006-03-23 (AGG):
*        Store number of frames (timeslices) in smfData
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 University of British Columbia &
*     Particle Physics and Astronomy Research Council.  All
*     Rights Reserved.

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

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "ast.h"
#include "ndf.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_flatfield"

void smf_flatfield ( const smfData *idata, smfData **odata, int *status ) {

  int checkflatstatus = 0;    /* Local variable for storing status */
  int i;                      /* Loop counter */
  int indims;                 /* Number of dimensions in input data */
  int j;                      /* Loop counter */
  int nbol;                   /* Number of bolometers */
  int ndims;                  /* Number of dimensions in output data */
  int nframes;                /* Number of time slices */
  int npts;                   /* Total number of data points */
  int nbytes;

  smfDA *ida = NULL;          /* da struct for input data */
  smfDA *da = NULL;           /* da struct for input data */
  smfFile *file = NULL;       /* file struct for input data */
  smfHead *hdr = NULL;        /* hdr struct for input data */

  AstFrame *skyframe = NULL;
  AstFrameSet *owcs;
  AstFrameSet *iwcs;
  smfHead *ihdr = NULL;        /* hdr struct for input data */

  void *ipntr[3];             /* Input D, Q and V arrays */
  void *opntr[3];             /* Output D, Q and V arrays */
  double *outdata;            /* Pointer to output DATA */
  int *tstream;               /* Pointer to raw time series data */
  smf_dtype dtype;            /* Data type */

  if ( *status != SAI__OK ) return;

  smf_check_flat( idata, status );
  /* Store status and annul for messaging system */
  checkflatstatus = *status;

  /* Data are flatfielded if status set to SMF__FLATN */
  /*  if ( checkflatstatus == SMF__FLATN ) {*/
  if ( *status == SMF__FLATN ) {
    errAnnul(status);
    /* check *odata */
    if ( *odata == NULL) {
      msgOutif(MSG__VERB, FUNC_NAME, 
	       "OK, data are flatfielded but odata is NULL", status);
      /* If NULL then we need to clone idata to odata i.e. copy the
	 pointer ONLY */
      smf_clone_data( idata, odata, status );
    } else {
      msgOutif(MSG__VERB, FUNC_NAME,
	       "OK, data are flatfielded and odata exists", status);
      /* Check dimensions and type */
      /* If it's not been set already, set dtype to DOUBLE */
      if ( (*odata)->dtype == SMF__NULL ) {
	(*odata)->dtype = SMF__DOUBLE;
      }
      dtype = (*odata)->dtype;

      indims = idata->ndims;
      if ( !(*odata)->ndims ) {
	(*odata)->ndims = indims;
      }
      ndims = (*odata)->ndims;
      nbol = ((*odata)->dims)[0]*((*odata)->dims)[1];
      nframes = ((*odata)->dims)[2];
      npts = nbol * nframes;
      /* Check the data type */
      if ( dtype != SMF__DOUBLE ) {
        *status = SAI__ERROR;
        errRep( FUNC_NAME, 
		"Output data type is not set to _DOUBLE, possible programming error",
		status);
      } else if ( ndims != indims ) {
        msgSeti( "NDIMS", ndims);
        msgSeti( "IDIMS", indims);
        *status = SAI__ERROR;
        errRep( FUNC_NAME, 
		"Number of dimensions in output, ^NDIMS, is not equal to number in input, ^IDIMS", status);
      } else {
	/* Check if the data array(s) exist */
	opntr[0] = ((*odata)->pntr)[0];
	if (opntr[0] == NULL) {
	  /* Allocate space for output data, copy over input data */
	  ipntr[0] = (idata->pntr)[0];
	  opntr[0] = smf_malloc( npts, sizeof( double ), 0, status );
	  memcpy( opntr[0], ipntr[0], npts*sizeof( double ) );
	}
	/* All's well so start populating the rest of the *odata
	   struct if necessary */
	/* Does the header (hdr) exist? */
	hdr = (*odata)->hdr;
	ihdr = idata->hdr;
	if ( hdr == NULL) {
	  hdr = smf_deepcopy_smfHead( ihdr, status);
	} else {
	  /* OK, we have a header. Do we have WCS? */
	  iwcs = ihdr->wcs;
	  /* If INPUT WCS is null then we have time series data and we
	     can forget about the WCS info for now */
	  if (iwcs == NULL) {
	    msgOutif(MSG__VERB, FUNC_NAME, 
		     "Aha, we must have time series data: don't copy WCS as it is created later", 
		     status);
	  } else {
	    owcs = hdr->wcs;
	    if ( owcs == NULL ) {
	      msgOutif(MSG__VERB, FUNC_NAME, "Output data struct has no WCS, copying from input", status);
	      /* Copy over WCS from input */
	      owcs = astCopy(iwcs);
	      hdr->wcs = owcs;
	    } else {
	      /* Check if we have a sky frame */
	      /* astFindFrame returns AST__NULL if a skyframe is not present */
	      skyframe = astFindFrame( owcs, astSkyFrame(""), "");
	      /* If no sky frame, overwrite current WCS with a copy of
		 the input WCS info using astCopy */
	      if (skyframe == AST__NULL) {
		msgOutif(MSG__VERB, FUNC_NAME, 
			 "Output FrameSet exists but does not have a SKYFRAME; copying WCS info from input data", 
			 status);
		owcs = astCopy(ihdr->wcs);
		hdr->wcs = owcs;
	      } else {
		msgOutif(MSG__VERB, FUNC_NAME, 
			 "Output FrameSet has a SKYFRAME", status);
	      }
	    }
	    hdr->nframes = nframes;
	  }
	}

	/* Does the flatfield (smfDA) struct exist? */
	da = (*odata)->da;
	ida = idata->da;
	if ( da == NULL && ida != NULL ) {
	  da = smf_malloc( 1, sizeof( smfDA ), 0, status );
	  /* Copy flatfield info into *odata */
	  memcpy( da, idata->da, sizeof( smfDA ) );
	  (*odata)->da = da;
	}

	/* Does the file (smfFile) struct exist? */
	file = (*odata)->file;
	if ( file == NULL ) {
	  file = smf_malloc( 1, sizeof( smfFile ), 0, status );
	  /* Copy file info */
	  memcpy( file, idata->file, sizeof( smfFile ) );
	  (*odata)->file = file;
	}
      }
    }
  } else if ( *status == SAI__OK ) { 

    /* OK data are not flatfielded: create smfData based on input and
       apply flatfield */

    /* Check if *odata exists */
    if ( *odata == NULL) {

      msgOutif(MSG__VERB, FUNC_NAME,"Data not FF, no odata", status);
      /* If NULL then we need create odata not associated with a file
	 (i.e. leave smfFile NULL) */
      /* Allocate space for *odata and all necessary cpts */
      /* Set the rawconvert flag to return doubles in the DATA array */
      *odata = smf_deepcopy_smfData( idata, 1, status );

      /* Apply flatfield calibration */
      smf_flatten( *odata, status);

    } else {

      /* OK, *odata exists */
      msgOutif(MSG__VERB, FUNC_NAME,"Data not FF, odata exists", status);

      /* To proceed we must know that the data type is correct */
      dtype = (*odata)->dtype;
      /* And that the input and output dimensions are equal */
      ndims = (*odata)->ndims; 
      indims = idata->ndims; 

      if ( dtype != SMF__DOUBLE ) {
        *status = SAI__ERROR;
        errRep( FUNC_NAME, "Output data type is not set to _DOUBLE", status);
      } else if ( ndims != indims ) {
        msgSeti( "NDIMS", ndims);
        msgSeti( "IDIMS", indims);
        *status = SAI__ERROR;
        errRep( FUNC_NAME, "Number of dimensions in output, ^NDIMS, is not equal to number in input, ^IDIMS", status);
      } else {

	/* All's well so start populating the *odata struct */
	/* Does the header (hdr) exist? */
	hdr = (*odata)->hdr;
	ihdr = idata->hdr;
	if ( hdr == NULL ) {
	  hdr = smf_deepcopy_smfHead( ihdr, status);
	  (*odata)->hdr = hdr; 
	} else if (ihdr != NULL) {
	  /* OK, we have a header. Do we have WCS? */
	  iwcs = ihdr->wcs;
	  /* If INPUT WCS is null then we have time series data and we
	     can forget about the WCS info for now */
	  if (iwcs == NULL) {
	    msgOutif(MSG__VERB, "", 
		     "Aha, we must have time series data: don't copy WCS as it is created later", 
		     status);
	  } else {
	    owcs = hdr->wcs;
	    if ( owcs == NULL ) {
	      msgOutif(MSG__VERB, FUNC_NAME, 
		       "Output data has no WCS, copying from input", status);
	      /* Copy over WCS from input */
	      owcs = astCopy(iwcs);
	      hdr->wcs = owcs;
	    } else {
	      /* Check if the output WCS has a sky frame */
	      /* astFindFrame returns AST__NULL if a skyframe is not present */
	      skyframe = astFindFrame( owcs, astSkyFrame(""), "");
	      /* If no sky frame, copy the input WCS info using astCopy */
	      if (skyframe == AST__NULL) {
		msgOutif(MSG__VERB, FUNC_NAME, 
			 "Output FrameSet exists but does not have a SKYFRAME; copying WCS from input", 
			 status);
		owcs = astCopy(ihdr->wcs);
		hdr->wcs = owcs;
	      } else {
		msgOutif(MSG__VERB, FUNC_NAME, "Output FrameSet has a SKYFRAME", status);
	      }
	    }
	  }
	}
	/* Set the number of frames */
	hdr->nframes = nframes;

	/* Does the flatfield (smfDA) struct exist? */
	da = (*odata)->da;
	if ( da == NULL ) {
	  da = smf_deepcopy_smfDA( idata->da, status );
	  (*odata)->da = da;
	}

	npts = ((*odata)->dims)[0] * ((*odata)->dims)[1] *((*odata)->dims)[2];

	/* Check that the data array is present and of the right type */
	for (i=0; i<2; i++) {
	  if ( (idata->pntr)[i] != NULL ) {
	    /* We must convert from integer to double for data and
	       variance (if it exists) */
	    if ( idata->dtype == SMF__INTEGER ) {
	      nbytes = sizeof(double);
	      dtype = SMF__DOUBLE;
	      opntr[i] = smf_malloc( npts, nbytes, 0, status);
	      outdata = opntr[i];
	      tstream = (idata->pntr)[i];
	      /* Input data are ints: must re-cast as double */
	      for (j=0; j<npts; j++) {
		outdata[j] = (double)tstream[j];
	      }
	      opntr[i] = outdata;
	    } else {
	      if ( *status == SAI__OK ) {
		*status = SAI__ERROR;
		errRep(FUNC_NAME, "Error: input data are not of type INTEGER (possible programming error)", status);
	      }
	    }
	  }
	}
	/* Quality */
	if ( (idata->pntr)[2] != NULL ) {
	  opntr[2] = smf_malloc( npts, 1, 0, status);
	  memcpy( opntr[2], (idata->pntr)[2], npts );
	} else {
	  opntr[2] = NULL;
	}
	/* Store pointers to D, V and Q */
	for ( i=0; i<3; i++) {
	  ((*odata)->pntr)[i] = opntr[i];
	}

	/* Apply flatfield calibration */
	smf_flatten( *odata, status);

      }
    }
  } else {
    msgOut(FUNC_NAME, 
	   "Hmm status is something other than it should be....", status);
  }

  /*  *status = checkflatstatus;*/
}

