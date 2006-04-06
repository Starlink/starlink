/*
*+
*  Name:
*     smf_check_smfHead

*  Purpose:
*     Check (and set) all elements of a smfHead structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_check_smfHead( const smfData *idata, smfData *odata, int * status );

*  Arguments:
*     idata = const smfData* (Given)
*        Pointer to input smfData
*     odata = smfData * (Given)
*        Pointer to output smfData
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:

*     This function checks all elements of a smfHead structure and
*     copies values from the input structure if necessary

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-04-04 (AGG):
*        Initial version.
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
#include "ast.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* SC2DA routines */
#include "sc2da/sc2store.h"
#include "sc2da/sc2store_struct.h"

#define FUNC_NAME "smf_check_smfHead"

void smf_check_smfHead( const smfData *idata, smfData *odata, int * status ) {

  smfHead *ihdr;
  smfHead *ohdr;
  sc2head *allsc2heads = NULL;
  sc2head *sc2head;
  AstFrameSet *owcs;
  AstFrameSet *skyframe;
  dim_t nframes;

  if (*status != SAI__OK) return;

  /* Retrieve headers */
  ihdr = idata->hdr;
  ohdr = odata->hdr;

  /* Do we have WCS? */
  /* First check if INPUT WCS is null => we have time series data and
     we can forget about the WCS info for now */
  if (ihdr->wcs == NULL) {
    msgOutif(MSG__VERB, "", 
	     "Input data are time series data: don't copy WCS as it is created later", 
	     status);
    /* Set output WCS to null */
    ohdr->wcs = NULL;
  } else {
    owcs = ohdr->wcs;
    if ( owcs == NULL ) {
      msgOutif(MSG__VERB, FUNC_NAME, 
	       "Output data has no WCS, copying from input", status);
      /* Copy over WCS from input */
      owcs = astCopy(ihdr->wcs);
      ohdr->wcs = owcs;
    } else {
      /* If WCS is present then check for a sky frame */
      /* astFindFrame returns AST__NULL if a skyframe is not present */
      skyframe = astFindFrame( owcs, astSkyFrame(""), "");
      /* If no sky frame, copy the input WCS info using astCopy */
      if (skyframe == AST__NULL) {
	msgOutif(MSG__VERB, FUNC_NAME, 
		 "Output FrameSet exists but does not have a SKYFRAME; copying WCS from input", 
		 status);
	owcs = astCopy(ihdr->wcs);
	ohdr->wcs = owcs;
      } else {
	msgOutif(MSG__VERB, FUNC_NAME, "Output FrameSet has a SKYFRAME", status);
      }
    }
  }

  /* Check the number of frames */
  if ( !(ohdr->nframes) ) {
    if ( odata->ndims > 2) {
      ohdr->nframes = (odata->dims)[2];
    } else {
      ohdr->nframes = 1;
    }
  } else {
    if ( odata->ndims > 2) {
      /* Can only do this check if we have 3-d data */
      if ( ohdr->nframes != (odata->dims)[2]) {
	if ( *status == SAI__OK) {
	  *status = SAI__ERROR;
	  msgSeti("NF1", (odata->dims)[2]);
	  msgSeti("NF2", ohdr->nframes);
	  errRep(FUNC_NAME, "Number of frames, ^NF2, does not equal size of third dimension of data array, ^NF1. Possible programming error.", status);
	}
      }
    } else {
      /* For 2-D data nframes should always be 1 */
      if ( ohdr->nframes != 1 ) {
	msgSeti("NF",ohdr->nframes);
	msgOutif(MSG__VERB, FUNC_NAME, "2-D data claims to have ^NF frames: overriding and setting to 1 now", status);
	ohdr->nframes = 1;
      }
    }
  }

  /* Do we have a FITS header? */
  if ( ohdr->fitshdr == NULL) {
    msgOutif( MSG__VERB, FUNC_NAME, "Output has no FITS header, copying from input", status );
    if ( ihdr->fitshdr == NULL ) {
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Input FITS header is NULL, possible programming error", status);
      }
    } else {
      ohdr->fitshdr = astCopy(ihdr->fitshdr);
    }
  }

  /* Do we have the sc2head struct? */
  if ( ohdr->allsc2heads == NULL) {
    if ( ihdr->allsc2heads == NULL ) {
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Input allsc2heads is NULL, possible programming error", status);
      }
    } else {
      nframes = ohdr->nframes;
      allsc2heads = smf_malloc( 1, nframes*sizeof(struct sc2head), 0, status);
      if ( allsc2heads == NULL) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME,"Unable to allocate memory for allsc2heads", status);
      }
      memcpy( allsc2heads, ihdr->allsc2heads, nframes*sizeof(struct sc2head) );
    }
  }

}
