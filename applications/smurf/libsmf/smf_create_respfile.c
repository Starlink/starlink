/*
*+
*  Name:
*     smf_create_respfile

*  Purpose:
*     Create a file on disk and map it for use as a responsivity map

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_create_respfile( const Grp * rgrp, size_t index,
*               const smfData* refdata, smfData **respmap,
*               int *status );

*  Arguments:
*     rgrp = const Grp * (Given)
*        Group containing the relevant file name.
*     index = size_t (Given)
*        Index into rgrp.
*     refdata = const smfData* (Given)
*        Reference smfData. Dimensionality, sub array information and
*        FITS header are obtained from this.
*     respmap = smfData** (Returned)
*        Output smfData.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Create a responsivity file on disk with the correct metadata. The file
*     is mapped for WRITE and is ready to receive the actual responsivity data.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-03-27 (TIMJ):
*        Initial version.
*     2009-05-21 (TIMJ):
*        smf_construct_smfHead API tweak

*  Notes:
*     - Does not propogate provenance or history from refdata.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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

#include "smf_typ.h"
#include "smf.h"
#include "sc2da/sc2ast.h"

#include "sae_par.h"
#include "ndf.h"
#include "ast.h"
#include "star/one.h"

void smf_create_respfile( const Grp * rgrp, size_t index,
                          const smfData* refdata, smfData **respmap,
                          int *status ) {

  int lbnd[] = { 1, 1 };
  int ubnd[2];

  *respmap = NULL;
  if (*status != SAI__OK) return;

  /* create the file for WRITE access */
  ubnd[SMF__ROW_INDEX] = (refdata->dims)[SMF__ROW_INDEX] - lbnd[SMF__ROW_INDEX] + 1;
  ubnd[SMF__COL_INDEX] = (refdata->dims)[SMF__COL_INDEX] - lbnd[SMF__COL_INDEX] + 1;
  smf_open_newfile( rgrp, index, SMF__DOUBLE, 2, lbnd, ubnd,
                    SMF__MAP_VAR, respmap, status );

  /* add some niceties - propagate some information from the first measurement */
  if (*status == SAI__OK) {
    char subarray[9];          /* subarray name */
    int subnum;                /* subarray number */
    char buffer[30];
    AstFrameSet *wcs = NULL;

    /* write the FITS header */
    kpgPtfts( (*respmap)->file->ndfid, refdata->hdr->fitshdr, status );

    /* Subarray information */
    smf_find_subarray( refdata->hdr, subarray, sizeof(subarray), &subnum, status );
    one_strlcpy( buffer, subarray, sizeof(buffer), status );
    one_strlcat( buffer, " bolometer responsivity", sizeof(buffer), status );

    (*respmap)->hdr = smf_construct_smfHead( NULL, refdata->hdr->instrument,
                                             NULL, NULL, NULL, NULL, 0, refdata->hdr->instap, 1,
                                             refdata->hdr->steptime, refdata->hdr->obsmode,
                                             refdata->hdr->swmode, refdata->hdr->obstype, 0, NULL, NULL,
                                             NULL, NULL, 0, NULL, buffer, "Responsivity",
                                             "Amps/Watt", refdata->hdr->telpos, status );
    smf_write_clabels( *respmap, status );

    /* create frame for focal plane coordinates. Should really extract it from the
     refdata WCS rather than attempting to reconstruct. */
    sc2ast_createwcs( subnum, NULL, NULL, NULL, &wcs, status );
    ndfPtwcs( wcs, (*respmap)->file->ndfid, status );
  }

}
