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
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     2006-04-04 (AGG):
*        Initial version.
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2006-07-31 (TIMJ):
*        Check fplanex/y and instrument
*     2006-10-2 (DSB):
*        Check detpos
*     2006-11-6 (DSB):
*        Check detname.
*     2007-02-23 (AGG):
*        Check instap
*     2009-06-23 (TIMJ):
*        Check ocsconfig
*     2014-03-11 (DSB):
*        Fix bug that caused segfault when copying ocsconfig and/or detname
*        from input to output. 
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009,2014 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 University of British Columbia. Particle
*     Physics And Astronomy Research Council. All Rights Reserved.

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
#include "ast.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_check_smfHead"

void smf_check_smfHead( const smfData *idata, smfData *odata, int * status ) {

  smfHead *ihdr;
  smfHead *ohdr;
  JCMTState *allState = NULL;
  AstFrameSet *owcs;
  AstFrameSet *skyframe;
  dim_t nframes;

  if (*status != SAI__OK) return;

  /* Retrieve headers */
  ihdr = idata->hdr;
  ohdr = odata->hdr;

  /* instrument code */
  if (ohdr->instrument == INST__NONE) ohdr->instrument = ihdr->instrument;

  /* Do we have WCS? */
  /* First check if INPUT WCS is null => we have time series data and
     we can forget about the WCS info for now */
  if (ihdr->wcs == NULL) {
    msgOutif(MSG__DEBUG, "",
             "Input data are time series data: don't copy WCS as it is created later",
             status);
    /* Set output WCS to null */
    ohdr->wcs = NULL;
  } else {
    owcs = ohdr->wcs;
    if ( owcs == NULL ) {
      msgOutif(MSG__DEBUG," ",
               "Output data has no WCS, copying from input", status);
      /* Copy over WCS from input */
      owcs = astCopy(ihdr->wcs);
      ohdr->wcs = owcs;
    } else {
      /* If WCS is present then check for a sky frame */
      /* astFindFrame returns AST__NULL if a skyframe is not present */
      skyframe = astFindFrame( owcs, astSkyFrame(" "), " " );
      /* If no sky frame, copy the input WCS info using astCopy */
      if (skyframe == AST__NULL) {
        msgOutif(MSG__DEBUG," ",
                 "Output FrameSet exists but does not have a SKYFRAME; copying WCS from input",
                 status);
        owcs = astCopy(ihdr->wcs);
        ohdr->wcs = owcs;
      } else {
        msgOutif(MSG__DEBUG," ", "Output FrameSet has a SKYFRAME", status);
      }
    }
  }

  /* Copy time series WCS if present and not in the output */
  if ( ihdr->tswcs != NULL ) {
    if (ohdr->tswcs == NULL) {
      msgOutif(MSG__DEBUG," ",
               "Output data has no time series WCS, copying from input", status);
      /* Copy over WCS from input */
      ohdr->tswcs = astCopy(ihdr->tswcs);
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
          msgSetk("NF1", (odata->dims)[2]);
          msgSetk("NF2", ohdr->nframes);
          errRep(FUNC_NAME, "Number of frames, ^NF2, does not equal size of third dimension of data array, ^NF1. Possible programming error.", status);
        }
      }
    } else {
      /* For 2-D data nframes should always be 1 */
      if ( ohdr->nframes != 1 ) {
        msgSetk("NF",ohdr->nframes);
        msgOutif(MSG__DEBUG," ", "2-D data claims to have ^NF frames: overriding and setting to 1 now", status);
        ohdr->nframes = 1;
      }
    }
  }

  /* Do we have a FITS header? */
  if ( ohdr->fitshdr == NULL) {
    msgOutif(MSG__DEBUG," ", "Output has no FITS header, copying from input", status );
    if ( ihdr->fitshdr == NULL ) {
      if ( *status == SAI__OK) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Input FITS header is NULL, possible programming error", status);
      }
    } else {
      ohdr->fitshdr = astCopy(ihdr->fitshdr);
    }
  }

  /* Do we have the STATE struct? */
  if ( ohdr->allState == NULL) {
    if ( ihdr->allState == NULL ) {
      if ( *status == SAI__OK) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Input allState is NULL, possible programming error",
               status);
      }
    } else {
      nframes = ohdr->nframes;
      if (*status == SAI__OK) {
        allState = astMalloc( nframes*sizeof(*allState) );
        if ( allState == NULL) {
          if (*status == SAI__OK) *status = SAI__ERROR;
          errRep(FUNC_NAME,"Unable to allocate memory for allState", status);
        } else {
          memcpy( allState, ihdr->allState, nframes*sizeof(*allState) );
        }
      }
    }
  }

  /* focal plane coordinate */
  if (ohdr->ndet == 0 && ihdr->ndet > 0) {
    if (ohdr->fplanex == NULL && ohdr->fplaney == NULL) {
      ohdr->fplanex = astMalloc( (ihdr->ndet)*sizeof(*(ohdr->fplanex)) );
      if (ohdr->fplanex) {
        memcpy( ohdr->fplanex, ihdr->fplanex,
                ihdr->ndet * sizeof(*(ohdr->fplanex)));
      }
      ohdr->fplaney = astMalloc( (ihdr->ndet)*sizeof(*(ohdr->fplaney)) );
      if (ohdr->fplaney) {
        memcpy( ohdr->fplaney, ihdr->fplaney,
                ihdr->ndet * sizeof(*(ohdr->fplaney)));
      }
      ohdr->ndet = ihdr->ndet;
    }

    /* Detector positions */
    if (ohdr->detpos == NULL ){
      ohdr->detpos = astMalloc( 2*ihdr->ndet*ihdr->nframes*
                                sizeof(*(ohdr->detpos)) );
      if (ohdr->detpos) {
        memcpy( ohdr->detpos, ihdr->detpos,
                2*ihdr->ndet*ihdr->nframes * sizeof(*(ohdr->detpos)));
      }
    }

    /* Detector names */
    if (ohdr->detname == NULL && ihdr->detname != NULL ){
      ohdr->detname = astStore( NULL, ihdr->detname,
                                ihdr->ndet*( strlen( ihdr->detname ) + 1 ) );
    }

    /* OCS Config */
    if (ohdr->ocsconfig == NULL && ihdr->ocsconfig != NULL ){
      ohdr->ocsconfig = astStore( NULL, ihdr->ocsconfig,
                                  strlen( ihdr->ocsconfig ) + 1 );
    }


    ohdr->dpazel = ihdr->dpazel;

  }

  /* Instrument aperture */
  if ( ohdr->instap ) {
    ihdr->instap[0] = ohdr->instap[0];
    ihdr->instap[1] = ohdr->instap[1];
  }

  /* final report */
  if (*status != SAI__OK) {
    errRep( " ", FUNC_NAME ": error checking a smfHead", status );
  }

}
