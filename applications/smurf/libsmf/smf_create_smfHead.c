/*
*+
*  Name:
*     smf_create_smfHead

*  Purpose:
*     Allocate a smfHead structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_create_smfHead( int * status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfHead = smfHead*
*        Pointer to newly created smfHead. NULL on error.

*  Description:
*     This function allocates memory for a smfHead structure and
*     all the internal structures. The structure is initialised.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Can be freed with a smf_free if header resources are freed first.

*  Authors:
*     Tim Jenness (TIMJ)
*     David Berry (DSB)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
*     2006-01-27 (TIMJ):
*        sc2head now pointer. Add allsc2heads as pointer.
*        initialise isCloned
*     2006-03-23 (AGG):
*        curslice changed to curframe, nframes added
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2006-07-26 (TIMJ):
*        Add tswcs.
*     2006-07-31 (TIMJ):
*        Add instrument code. Add fplanex, fplaney.
*     2006-09-07 (EC):
*        Add instap and telpos.
*     2006-10-2 (DSB):
*        Add detpos.
*     2006-11-6 (DSB):
*        Add detname.
*     2007-02-06 (AGG):
*        Add tsys
*     2008-04-30 (TIMJ):
*        Add units, title and dlabel.
*     2008-06-30 (DSB):
*        Add "cache".
*     2008-07-24 (TIMJ):
*        initialise obsmode and obstype
*     2008-07-28 (TIMJ):
*        initialise steptime
*     2008-12-03 (DSB):
*        Add rename cache as cache1, and add cache2 and cache3.
*     2009-05-21 (TIMJ):
*        Add switching mode.
*     2009-06-23 (TIMJ):
*        Add ocsconfig.
*     2010-03-15 (TIMJ):
*        Initialise sequence type
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research
*     Council.
*     Copyright (C) 2006-2007 University of British Columbia.
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
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_create_smfHead"

smfHead *
smf_create_smfHead( int * status ) {

  smfHead * hdr = NULL;   /* Header components */

  if (*status != SAI__OK) return NULL;

  hdr = astMalloc( 1*sizeof(smfHead) );

  if (*status != SAI__OK) {
    errRep(FUNC_NAME,"Unable to allocate memory for smfHead structure",
           status );
    return NULL;
  }

  /* Initialise smfHead */
  hdr->instrument = INST__NONE;
  hdr->wcs = NULL;
  hdr->tswcs = NULL;
  hdr->fitshdr = NULL;
  hdr->cache1 = NULL;
  hdr->cache2 = NULL;
  hdr->cache3 = NULL;
  hdr->curframe = 0;
  hdr->nframes = 0;
  hdr->state = NULL;
  hdr->allState = NULL;
  hdr->ndet = 0;
  hdr->fplanex = NULL;
  hdr->fplaney = NULL;
  hdr->detpos = NULL;
  hdr->detname = NULL;
  hdr->dpazel = 0;
  hdr->isCloned = 0;
  hdr->mtype = SMF__NUL;
  hdr->telpos[0] = 0;
  hdr->telpos[1] = 0;
  hdr->telpos[2] = 0;
  hdr->instap[0] = 0;
  hdr->instap[1] = 0;
  hdr->tsys = NULL;
  hdr->obsmode = SMF__OBS_NULL;
  hdr->obstype = SMF__TYP_NULL;
  hdr->seqtype = SMF__TYP_NULL;
  hdr->swmode = SMF__SWM_NULL;
  hdr->inbeam = SMF__INBEAM_NOTHING;
  hdr->steptime = VAL__BADD;
  hdr->ocsconfig = NULL;
  (hdr->units)[0] = '\0';
  (hdr->dlabel)[0] = '\0';
  (hdr->title)[0] = '\0';
  (hdr->obsidss)[0] = '\0';
  return hdr;
}
