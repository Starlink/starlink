/*
*+
*  Name:
*     smf_deepcopy_smfHead

*  Purpose:
*     Copy all elements of a smfHead structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     new = smf_deepcopy_smfHead( const smfHead *old, int * status );

*  Arguments:
*     old = const smfHead* (Given)
*        Pointer to smfHead to be copied
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_deepcopy_smfHead = smfHead*
*        Pointer to newly created smfHead. NULL on error.

*  Description:
*     This function copies all information from an existing smfHead
*     structure and all the internal structures to a new smfHead
*     structure.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Can be freed with a smf_free if header resources are freed first.

*  Authors:
*     Tim Jenness (TIMJ)
*     Andy Gibb (UBC)
*     David Berry (DSB)
*     {enter_new_authors_here}

*  History:
*     2006-03-23 (AGG):
*        Initial version.
*     2006-03-24 (AGG):
*        Trap NULL allsc2heads
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2006-07-28 (TIMJ):
*        Add support for tswcs.
*     2006-06-31 (TIMJ):
*        Add instrument and fplane*
*        make sure memcpy is protected. Some tidy up.
*     2006-08-02 (TIMJ):
*        The new smfHead is not a clone.
*     2006-10-02 (DSB):
*        Add detpos.
*     2006-11-06 (DSB):
*        Add detname.
*     2007-02-06 (AGG):
*        Add tsys
*     2007-02-23 (AGG):
*        Add instap
*     2008-04-30 (TIMJ):
*        Add units, label and title
*     2007-07-1 (DSB):
*        Add telpos.
*     2008-07-24 (TIMJ):
*        Add obsmode, obstype.
*     2008-07-28 (TIMJ):
*        Add steptime.
*     2009-05-21 (TIMJ):
*        smf_construct_smfHead API tweak
*     2009-06-23 (TIMJ):
*        ocsconfig added to smfHead
*     2010-03-15 (TIMJ):
*        Include seqtype and obsidss.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2006-2007 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "jcmt/state.h"
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_deepcopy_smfHead"

smfHead *
smf_deepcopy_smfHead( const smfHead *old, int * status ) {

  smfHead *new = NULL;             /* New Header */
  dim_t nframes = 0;               /* Number of frames/time slices in data */
  dim_t curframe = 0;              /* Index of current time slice */
  JCMTState *allState = NULL;      /* Struct with all JCMTState entries */
  AstFitsChan *fitshdr = NULL;     /* FITS header */
  AstFrameSet *tswcs = NULL;       /* Frameset for full time series (if tseries) */
  AstFrameSet *wcs = NULL;         /* Frameset for a particular time slice (frame)*/
  inst_t instrument = INST__NONE;  /* Instrument label */
  unsigned int ndet = 0;           /* Number of detectors */
  double *fplanex = NULL;          /* X focal plane positions */
  double *fplaney = NULL;          /* Y focal plane positions */
  double *detpos = NULL;           /* Array of detector positions */
  char *detname = NULL;            /* Receptor names */
  double *tsys = NULL;             /* System temperatures */
  double instap[2];                /* Instrument aperture (focal plane offsets) */
  char *ocsconfig = NULL;          /* OCS configuration XML */

  if (*status != SAI__OK) return NULL;

  if (!old) return NULL;

  /* Copy elements */
  nframes = old->nframes;
  curframe = old->curframe;

  if (old->fitshdr) fitshdr = astCopy(old->fitshdr);

  if (old->wcs) wcs = astCopy(old->wcs);
  if (old->tswcs) tswcs = astCopy(old->tswcs);

  if (old->instrument) instrument = old->instrument;

  if ( old->instap ) {
    instap[0] = old->instap[0];
    instap[1] = old->instap[1];
  }

  /* Only allocate space for allState if we have a non-NULL input
     allState */
  if ( old->allState != NULL) {
    allState = astCalloc( nframes, sizeof(*allState), 0 );
    if ( allState == NULL) {
      /* Status should be bad from astCalloc */
      if (*status == SAI__OK) *status = SAI__ERROR;
      errRep(FUNC_NAME,"Unable to allocate memory for allState", status);
    } else {
      memcpy( allState, old->allState, nframes*sizeof(*allState) );
    }
  }

  /* need to allocate focal plane offsets */
  if (old->ndet > 0 && old->fplanex && old->fplaney) {
    ndet = old->ndet;
    fplanex = astCalloc( ndet, sizeof(*fplanex), 0 );
    if (fplanex) memcpy( fplanex, old->fplanex, ndet*sizeof(*fplanex) );

    fplaney = astCalloc( ndet, sizeof(*fplaney), 0 );
    if (fplaney) memcpy( fplaney, old->fplaney, ndet*sizeof(*fplaney) );
  }

  /* need to allocate detector positions */
  if (old->ndet > 0 && old->nframes > 0 && old->detpos) {
    ndet = old->ndet;
    nframes = old->nframes;
    detpos = astCalloc( 2*ndet*nframes, sizeof(*detpos), 0 );
    if (detpos) memcpy( detpos, old->detpos, 2*ndet*nframes*sizeof(*detpos) );
  }

  /* need to allocate detector name array */
  if (old->ndet > 0 && old->detname) {
    ndet = old->ndet;
    detname = astCalloc( ndet, strlen( old->detname ) + 1, 0 );
    if( detname ) memcpy( detname, old->detname,
                          ndet*( strlen( old->detname ) + 1 ) );
  }

  /* Allocate Tsys array */
  if (old->ndet > 0 && old->tsys) {
    ndet = old->ndet;
    nframes = old->nframes;
    tsys = astCalloc( ndet*nframes, sizeof(*tsys), 0 );
    if ( tsys ) memcpy( tsys, old->tsys, ndet*nframes*sizeof(*tsys) );
  }

  /* Allocate ocsconfig */
  if (old->ocsconfig != NULL) {
    ocsconfig = astCalloc( 1, strlen( old->ocsconfig ) + 1, 0 );
    if (ocsconfig) strcpy( ocsconfig, old->ocsconfig );
  }

  /* Insert elements into new smfHead */
  new = smf_construct_smfHead( new, instrument, wcs, tswcs, fitshdr,
                               allState, curframe, instap, nframes,
                               old->steptime, old->scanvel, old->obsmode,
                               old->swmode, old->obstype,
                               old->seqtype, old->inbeam, ndet,
                               fplanex, fplaney, detpos,detname, old->dpazel,
                               tsys, old->title, old->dlabel, old->units,
                               old->telpos, ocsconfig, old->obsidss, status );

  /* set isCloned to 0 since we have allocated this memory */
  if (new) new->isCloned = 0;

  /* let people know where things are going wrong */
  if (*status != SAI__OK) {
    errRep( " ", FUNC_NAME ": error deep copying a smfHead", status );
  }

  /* this could be NULL */
  return new;
}
