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
*     smf_model_dataOrder( smfDIMMData *dat, smfArray ** allmodel,
*                          int chunk, smf_modeltype toOrder,
*                          int isTordered, int * status );

*  Arguments:
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "sae_par.h"
#include "mers.h"

#include "smf.h"

#define REORDER( TYP, ARR )                                             \
  {                                                                     \
    size_t idx;                                                         \
    for (idx = 0; idx < ARR->ndat; idx++) {                             \
      if ( ARR->sdata[idx] ) {                                          \
        const char tordered[] = "time ordered";                         \
        const char bordered[] = "bolo ordered";                         \
        msgOutiff( MSG__VERB, " ",                                      \
                   "Converting model %s from %s to %s\n",               \
                   status, TYP,                                         \
                   (ARR->sdata[idx]->isTordered ? tordered : bordered), \
                   (isTordered ? tordered : bordered ) );               \
      }                                                                 \
      smf_dataOrder( ARR->sdata[idx], isTordered, status );             \
    }                                                                   \
  }

void
smf_model_dataOrder( smfDIMMData *dat, smfArray ** allmodel, int chunk, smf_modeltype toOrder,
                     int isTordered, int * status ) {

  if (*status != SAI__OK) return;
  if (!dat) {
    *status = SAI__ERROR;
    errRep( "", "smf_model_dataOrder called without data model pointer"
            " (possible programming error)", status );
    return;
  }

  if (allmodel) {
    REORDER( "", allmodel[chunk] );
  }

  if ( toOrder & SMF__EXT ) REORDER( "ext", dat->ext[chunk] );
  if ( toOrder & SMF__GAI ) REORDER( "gai", dat->res[chunk] );
  if ( toOrder & SMF__RES ) REORDER( "res", dat->res[chunk] );
  if ( toOrder & SMF__NOI ) REORDER( "noi", dat->noi[chunk] );
  if ( toOrder & SMF__QUA ) REORDER( "qua", dat->qua[chunk] );
  if ( toOrder & SMF__LUT ) REORDER( "lut", dat->lut[chunk] );

}

