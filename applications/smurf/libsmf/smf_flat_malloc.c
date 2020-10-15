/*
*+
*  Name:
*     smf_flat_malloc

*  Purpose:
*     Create smfData structures suitable for flatfield processing

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*    void smf_flat_malloc( dim_t nheat, const smfData * refdata,
*             smfData ** powvald, smfData ** bolvald, int *status );

*  Arguments:
*     nheat = dim_t (Given)
*        Size of powvald and size of 3rd dimension for bolvald.
*     refdata = const smfData * (Given)
*        Template smfData used to obtain bolometer dimensions and
*        associated metadata.
*     powvald = smfData ** (Returned)
*        smfData suitable for holding heater power settings.
*        Can be NULL.
*     bolvald = smfData ** (Returned)
*        smfData suitable for holding flatfield bolometer data.
*        The 3rd dimension will match "powvald". Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Create container smfData structures for the flatfield bolometer
*     data and the flatfield power data.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - See also smf_flat_standardpow and smf_flat_polyfit.
*     - powvald or bolvald can be NULL. At least one should be non-NULL.

*  History:
*     2010-03-03 (TIMJ):
*        Initial version
*     2010-09-17 (COBA):
*        Updated smf_construct_smfData which now contains smfFts
*     2011-04-25 (TIMJ):
*        Handle time ordering properly.
*     2011-04-26 (TIMJ):
*        Fix assignment of rows and colums to bolval
*     2014-09-19 (DSB):
*        Ensure *bolvald is returned with a (3-D) WCS FrameSet.

*  Copyright:
*     Copyright (C) 2010-2014 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "ast.h"
#include "star/atl.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"

void smf_flat_malloc( dim_t nheat, const smfData * refdata,
                      smfData **powvald, smfData **bolvald, int *status ) {

  dim_t rowidx = SC2STORE__ROW_INDEX;
  dim_t colidx = SC2STORE__COL_INDEX;
  double * bolval = NULL; /* Data array inside bolrefd */
  double * bolvalvar = NULL;     /* Variance inside bolrefd */
  dim_t dims[] = { 1, 1, 1 };    /* Default dimensions */
  smfHead * hdr = NULL;   /* New header */
  dim_t lbnd[] = { 1, 1, 1 };    /* Default pixel lower bounds */
  dim_t nelem = 0;        /* Number of elements in first two dimensions of refdims */
  smfHead * oldhdr = NULL;/* header from refdata */
  void *pntr[] = { NULL, NULL }; /* pointers for smfData */
  double * powval = NULL; /* Data array inside powrefd */
  const char *dom;        /* Domain of axis 1 */
  AstFrameSet *new_fs;    /* New FrameSet for returned *bolvald */
  AstMapping *map;        /* Mapping from pixel index 3 to heater index */
  AstFrame *frm;          /* Frame describing heater index */
  dim_t ubnd[ 1 ];        /* Upper bound on heater index */

  if (bolvald) *bolvald = NULL;
  if (powvald) *powvald = NULL;

  if ( *status != SAI__OK ) return;

  if ( !bolvald && !powvald) {
    *status = SAI__ERROR;
    errRep( "", "Must provide at least one non-NULL pointer to smf_flat_malloc"
            " (possible programming error)", status );
    return;
  }

  /* Sanity check */
  if ( nheat == 0 ) {
    *status = SAI__ERROR;
    errRep( "", "No flatfield information present for creating new smfData",
            status );
    return;
  }

  if ( !smf_validate_smfData( refdata, 1, 0, status ) ) return;
  oldhdr = refdata->hdr;

  if (powvald) {
    powval = astCalloc( nheat, sizeof(*powval) );
    pntr[0] = powval;
    pntr[1] = NULL;
    dims[0] = nheat;
    *powvald = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                      pntr, NULL, SMF__QFAM_NULL, NULL, 0, 1,
                                      dims, NULL, 1, 0, 0, NULL,
                                      NULL, status );
  }

  if (bolvald) {
    /* Handle data ordering */
    if ( ! refdata->isTordered ) {
      rowidx++;
      colidx++;
    }

    nelem = refdata->dims[rowidx] * refdata->dims[colidx];
    bolval = astCalloc( nheat * nelem, sizeof(*bolval) );
    bolvalvar = astCalloc( nheat * nelem, sizeof(*bolvalvar) );
    pntr[0] = bolval;
    pntr[1] = bolvalvar;
    dims[SC2STORE__ROW_INDEX] = refdata->dims[rowidx];
    dims[SC2STORE__COL_INDEX] = refdata->dims[colidx];
    dims[2] = nheat;
    lbnd[SC2STORE__ROW_INDEX] = refdata->lbnd[rowidx];
    lbnd[SC2STORE__COL_INDEX] = refdata->lbnd[colidx];
    lbnd[2] = 1;

    /* Create a header to attach to the bolometer data. We only want the basic 2-d
       information to propagate. */
    hdr = smf_construct_smfHead( NULL, oldhdr->instrument, NULL, NULL,
                                 astCopy( oldhdr->fitshdr ), NULL, 0,
                                 oldhdr->instap, nheat, oldhdr->steptime,
                                 oldhdr->scanvel, oldhdr->obsmode,
                                 oldhdr->swmode, oldhdr->obstype,
                                 oldhdr->seqtype, oldhdr->inbeam, 0, NULL, NULL,
                                 NULL, NULL, 0, NULL,
                                 "Flatfield measurement", "Response",
                                 oldhdr->units, oldhdr->telpos,
                                 NULL, oldhdr->obsidss, status );

    *bolvald = smf_construct_smfData( NULL, NULL, hdr, NULL, NULL, SMF__DOUBLE,
                                      pntr, NULL, SMF__QFAM_TSERIES, NULL, 0, 1,
                                      dims, lbnd, 3, 0, 0, NULL, NULL, status );

    /* Assign a 3D WCS FRameSet in which the third axis represents heater
       value index (note, not actual heater value, since we do not yet
       know what the heater values are). First split the supplied time-series
       WCS FrameSet to extract a FrameSet in which the current Frame
       contains only the axes within the ame Domain as the first axis
       (this is safe because the first axis is always a spatial axis). */
    if( oldhdr->tswcs ) {
      dom = astGetC( oldhdr->tswcs, "Domain(1)" );
      new_fs = atlFrameSetSplit( oldhdr->tswcs, dom, NULL, NULL, status );

      /* Check this FrameSet is 2D, and if so, add in a third axis describing
         heater value index. */
      if( new_fs && astGetI( new_fs, "Naxes" ) == 2 ) {
         map = (AstMapping *) astUnitMap( 1, " " );
         frm = astFrame( 1, "Domain=HEATER_INDEX" );
         ubnd[ 0 ] = nheat;
         atlAddWcsAxis8(  new_fs, map, frm, NULL, ubnd, status );
         map = astAnnul( map );
         frm = astAnnul( frm );

         /* Hand over the FrameSet pointer to the returned smfData. */
         (*bolvald)->hdr->tswcs = new_fs;

      }
    }
  }

  return;
}
