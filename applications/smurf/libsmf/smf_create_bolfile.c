/*
*+
*  Name:
*     smf_create_bolfile

*  Purpose:
*     Create bolometer shaped 2D smfData, either malloced or on disk

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_create_bolfile( ThrWorkForce *wf, const Grp * bgrp, dim_t index,
*               const smfData* refdata, const char * datalabel,
*               const char * units, int flags, smfData **bolmap,
*               int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     bgrp = const Grp * (Given)
*        Group containing the relevant file name. If NULL no file
*        is created and the smfData is malloced.
*     index = dim_t (Given)
*        Index into bgrp.
*     refdata = const smfData* (Given)
*        Reference smfData. Dimensionality, sub array information and
*        FITS header are obtained from this.
*     datalabel = const char * (Given)
*        Label for the data array. Can be NULL. Title will be derived
*        from this.
*     flags = int (Given)
*        Flags to indicates if QUALITY and/or VARIANCE should be created.
*        Flags are SMF__MAP_QUAL and SMF__MAP_VAR. See smf_open_newfile
*        for details.
*     bolmap = smfData** (Returned)
*        Output smfData.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Create a smfData with the correct metadata for a 2d bolometer map.
*     The smfData will either be associated with a file and mapped for WRITE
*     access ready to receive data, or else, if no Grp is supplied it will be
*     malloced. Useful for responsivity images and noise data.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2009-03-27 (TIMJ):
*        Initial version.
*     2009-05-21 (TIMJ):
*        smf_construct_smfHead API tweak
*     2009-10-05 (TIMJ):
*        Rename to use for noise files as well as responsivity images.
*     2009-10-08 (TIMJ):
*        Use malloc if the input group is null
*     2009-11-06 (TIMJ):
*        Preferentially select BOLO frame for output WCS
*     2009-11-30 (TIMJ):
*        Add ability to enable quality.
*     2010-03-15 (TIMJ):
*        New API for smf_construct_smfHead
*     2010-07-02 (TIMJ):
*        Work with any data order for refdata.
*     2010-09-17 (COBA):
*        Updated smf_construct_smfData which now contains smfFts
*     2011-05-16 (TIMJ):
*        Allow variance to be disabled as well as quality. Use a single
*        flags argument instead of hasqual.
*     2014-07-30 (TIMJ):
*        Extract BOLO frame from the suppled reference time-series WCS
*        rather than recalculating everything.

*  Notes:
*     - Does not propogate provenance or history from refdata.

*  Copyright:
*     Copyright (C) 2009-2011 Science and Technology Facilities Council.
*     Copyright (C) 2014 Cornell University.
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

#include "smf_typ.h"
#include "smf.h"
#include "sc2da/sc2ast.h"

#include "sae_par.h"
#include "ndf.h"
#include "star/kaplibs.h"
#include "star/thr.h"
#include "star/atl.h"
#include "ast.h"
#include "star/one.h"

void smf_create_bolfile( ThrWorkForce *wf, const Grp * bgrp, dim_t index,
                         const smfData* refdata, const char *datalabel,
                         const char *units, int flags, smfData **bolmap,
                         int *status ) {

  int col_index = SC2STORE__COL_INDEX;
  dim_t lbnd[2];
  dim_t ubnd[2];
  int row_index = SC2STORE__ROW_INDEX;

  *bolmap = NULL;
  if (*status != SAI__OK) return;

  /* Calculate bounds - take into accont time ordering of reference
     smfData. */
  if ( ! refdata->isTordered ) {
    /* one further along */
    col_index++;
    row_index++;
  }

  lbnd[SC2STORE__ROW_INDEX] = (refdata->lbnd)[row_index];
  lbnd[SC2STORE__COL_INDEX] = (refdata->lbnd)[col_index];
  ubnd[SC2STORE__ROW_INDEX] = lbnd[SC2STORE__ROW_INDEX] +
          (refdata->dims)[row_index] - 1;
  ubnd[SC2STORE__COL_INDEX] = lbnd[SC2STORE__COL_INDEX] +
          (refdata->dims)[col_index] - 1;

  /* either create the file or use malloc */
  if (bgrp) {
    /* create the file for WRITE access */
    smf_open_newfile( wf, bgrp, (int) index, SMF__DOUBLE, 2, lbnd, ubnd,
                      flags, bolmap, status );
    if (*bolmap) (*bolmap)->qfamily = SMF__QFAM_TSERIES;
  } else {
    void *pntr[] = {NULL, NULL};
    smf_qual_t *qual = NULL;
    dim_t mydims[2];
    dim_t mylbnd[2];
    dim_t nbols;

    mylbnd[0] = lbnd[0];
    mylbnd[1] = lbnd[1];
    mydims[0] = ubnd[0] - lbnd[0] + 1;
    mydims[1] = ubnd[1] - lbnd[1] + 1;
    nbols = mydims[0] * mydims[1];

    pntr[0] = astMalloc( nbols*sizeof(double) );
    if (flags & SMF__MAP_VAR) pntr[1] = astMalloc( nbols*sizeof(double) );
    if (flags & SMF__MAP_QUAL) qual = astMalloc( nbols*sizeof(*qual) );

    *bolmap = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                     pntr, qual, SMF__QFAM_TSERIES, NULL, 0, 0,
                                     mydims, mylbnd, 2, 0, 0, NULL,
                                     NULL, status );
  }

  /* add some niceties - propagate some information from the first measurement */
  if (*status == SAI__OK) {
    char subarray[9];          /* subarray name */
    int frnum = AST__NOFRAME;  /* Index of BOLO frame */
    sc2ast_subarray_t subnum;  /* subarray number */
    char buffer[30];
    AstFrameSet *wcs = NULL;

    /* Subarray information */
    smf_find_subarray( refdata->hdr, subarray, sizeof(subarray), &subnum, status );
    one_strlcpy( buffer, subarray, sizeof(buffer), status );
    if (datalabel) {
      one_strlcat( buffer, " Bolometer ", sizeof(buffer), status );
      one_strlcat( buffer, datalabel, sizeof(buffer), status );
    }

    /* Create output WCS by selecting the FPLANE domain from the
       time-series WCS. If there is no FPLANE we do not write a WCS */
    if (refdata->hdr->tswcs) {
      wcs = atlFrameSetSplit( refdata->hdr->tswcs, "FPLANE", NULL, NULL, status );

      /* and switch to BOLO frame which is best for bolometer analysis */
      if (wcs) {
        kpg1Asffr( wcs, "BOLO", &frnum, status );
        if (frnum != AST__NOFRAME) astSetI( wcs, "CURRENT", frnum );
      }
    }
    (*bolmap)->hdr = smf_construct_smfHead( NULL, refdata->hdr->instrument,
                                            wcs, NULL,
                                            astCopy( refdata->hdr->fitshdr ),
                                            NULL, 0, refdata->hdr->instap, 1,
                                            refdata->hdr->steptime,
                                            refdata->hdr->scanvel,
                                            refdata->hdr->obsmode,
                                            refdata->hdr->swmode,
                                            refdata->hdr->obstype,
                                            refdata->hdr->seqtype,
                                            refdata->hdr->inbeam, 0, NULL, NULL,
                                            NULL, NULL, 0, NULL, buffer,
                                            datalabel, units,
                                            refdata->hdr->telpos, NULL,
                                            refdata->hdr->obsidss, status );

    /* write WCS and FITS information to file and sync other information */
    if (bgrp) {
      kpgPtfts( (*bolmap)->file->ndfid, refdata->hdr->fitshdr, status );
      if (wcs) ndfPtwcs( wcs, (*bolmap)->file->ndfid, status );
      smf_write_clabels( *bolmap, status );
    }
  }

}
