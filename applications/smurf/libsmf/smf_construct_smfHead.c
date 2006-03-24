/*
*+
*  Name:
*     smf_construct_smfHead

*  Purpose:
*     Populate a smfHead structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_construct_smfHead( smfHead * tofill,
*              AstFrameSet * wcs, AstFitsChan * fitshdr,
*	       struct sc2head * allsc2heads,
*              dim_t curframe, int * status );

*  Arguments:
*     tofill = smfHead* (Given)
*        If non-NULL, this is the smfHead that is populated by the remaining
*        arguments. If NULL, the smfHead is malloced.
*     wcs = AstFrameSet * (Given)
*        Frameset for the world coordinates. The pointer is copied,
*        not the contents.
*     fitshdr = AstFitsChan * (Given)
*        FITS header. The pointer is copied, not the contents.
*     allsc2heads = sc2head* (Given)
*        Pointer to array of time series information for all time slices.
*        Should be at least "curslice" in size.
*     curframe = dim_t (Given)
*        Current time index corresponding to the associated WCS. sc2head
*        will be set to this location.
*     nframes = dim_t (Given)
*        Number of frames (timeslices) in data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_construct_smfHead = smfHead*
*        Pointer to newly created smfHead (NULL on error) or,
*        if "tofill" is non-NULL, the pointer to the supplied struct.

*  Description:
*     This function fills a smfHead structure. Optionally, the smfHead
*     is allocated by this routines.

*  Notes:
*     - AST objects are neither cloned not copied by this routine.
*       Use astCopy or astClone when calling if reference counts
*       should be incremented.
*     - sc2head is set to point into allsc2heads[curslice]
*     - allsc2heads is not copied. In general the time series information
*       can be copied between headers without being modified. If this
*       memory should be freed by smf_close_file, set the isCloned flag
*       to false.
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Can be freed with a smf_free if header resources are freed first.

*  Authors:
*     Tim Jenness (TIMJ)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
*     2006-01-27 (TIMJ):
*        Replace sc2head with allsc2heads. sc2head now indexed
*        into allsc2heads.
*     2006-03-23 (AGG):
*        curslice changed to curframe, nframes added
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_construct_smfHead"

smfHead *
smf_construct_smfHead( smfHead * tofill,
		       AstFrameSet * wcs, AstFitsChan * fitshdr,
		       struct sc2head * allsc2heads,
		       dim_t curframe, dim_t nframes, int * status ) {

  smfHead * hdr = NULL;   /* Header components */

  hdr = tofill;
  if (*status != SAI__OK) return hdr;

  if (tofill == NULL) {
    hdr = smf_create_smfHead( status );
  }

  if (*status == SAI__OK) {
    hdr->wcs = wcs;
    hdr->fitshdr = fitshdr;
    hdr->curframe = curframe;
    hdr->nframes = nframes;
    hdr->allsc2heads = allsc2heads;
    hdr->sc2head = &(allsc2heads[curframe]);
    hdr->isCloned = 1;
  }

  return hdr;
}
