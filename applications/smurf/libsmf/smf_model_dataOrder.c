/*
*+
*  Name:
*     smf_model_dataOrder

*  Purpose:
*     Ensure that all the listed components are in the same data order

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_model_dataOrder( ThrWorkForce *wf, smfDIMMData *dat,
*                          smfArray ** allmodel,
*                          int chunk, smf_modeltype toOrder,
*                          int isTordered, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     allmodel = smfArray ** (Given)
*        Collection of model data as array of smfArrays indexed by chunk.
*        Can be NULL.
*     chunk = int (Given)
*        Index of time chunk in allmodel (and dat) to be reordered
*     toOrder = smf_modeltype (Given)
*        Bit mask indicating which models should have their order
*        synchronized. The allowed types are listed in smf_typ.h and must
*        have an entry in the smfDIMMData struct and be 3-d models matching
*        raw data dimensions.
*     isTordered = int (Given)
*        If 0, ensure data is ordered by bolometer. If 1 ensure data is
*        ordered by time slice (default ICD ordering)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*      Force all the indicated models for the specified chunk to be in the
*      specified data order.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Currently allows reordering of "ext", "res", "noi", "qua", "gai" and "lut".

*  History:
*     2010-05-17 (TIMJ):
*        Initial version
*     2013-05-29 (DSB):
*        Re-order GAI when requested. Previously, RES was reordered when
*        GAI was requested.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

#include "sae_par.h"
#include "mers.h"

#include "smf.h"

#define REORDER( TYP, ARR )                                             \
  {                                                                     \
    dim_t idx;                                                         \
    for (idx = 0; idx < ARR->ndat; idx++) {                             \
      int waschanged = 0;                                               \
      int old_order = 0;                                                \
      if ( ARR->sdata[idx] ) old_order = ARR->sdata[idx]->isTordered;   \
      waschanged = smf_dataOrder( wf, ARR->sdata[idx], isTordered, status ); \
      if ( waschanged && ARR->sdata[idx] ) {                            \
        const char tordered[] = "time ordered";                         \
        const char bordered[] = "bolo ordered";                         \
        msgOutiff( MSG__VERB, " ",                                      \
                   "    Converted %s model from %s to %s\n",           \
                   status, TYP,                                         \
                   (old_order ? tordered : bordered),                   \
                   (isTordered ? tordered : bordered ) );               \
      }                                                                 \
    }                                                                   \
  }

void
smf_model_dataOrder( ThrWorkForce *wf, smfDIMMData *dat, smfArray ** allmodel, int chunk, smf_modeltype toOrder,
                     int isTordered, int * status ) {

  if (*status != SAI__OK) return;
  if (!dat) {
    *status = SAI__ERROR;
    errRep( "", "smf_model_dataOrder called without data model pointer"
            " (possible programming error)", status );
    return;
  }

  if (allmodel) {
    REORDER( "current", allmodel[chunk] );
  }

  if ( toOrder & SMF__EXT && dat->ext ) REORDER( "ext", dat->ext[chunk] );
  if ( toOrder & SMF__GAI && dat->gai ) REORDER( "gai", dat->gai[chunk] );
  if ( toOrder & SMF__RES && dat->res ) REORDER( "res", dat->res[chunk] );
  if ( toOrder & SMF__NOI && dat->noi ) REORDER( "noi", dat->noi[chunk] );
  if ( toOrder & SMF__QUA && dat->qua ) REORDER( "qua", dat->qua[chunk] );
  if ( toOrder & SMF__LUT && dat->lut ) REORDER( "lut", dat->lut[chunk] );

}

