/*
*+
*  Name:
*     smf_flat_mergedata

*  Purpose:
*     Merge data from discrete flatfields into single smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*    void smf_flat_mergedata( const smfArray * heatframes,
*                             const double heatval[], smfData ** bolvald,
*                             int * status );

*  Arguments:
*     heatframes = const smfArray* (Given)
*        Collection of heat frames.
*     heatval = const double[] (Given)
*        For each frame in "heatframes", this is the heater setting that
*        was used.
*     bolvald = smfData ** (Returned)
*        smfData holding the measured bolometer values.
*        The 3rd dimension will match "powvald" the number
*        of entries in "heatframes". The heater values will be
*        stored in the smfDA struct in the "heatval" entry.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Create a single smfData from the separate heater frames. No attempt
*     is made to normalise the readings to a standard heater setting
*     (use smf_flat_standardpow for that).

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - See also smf_flat_standardpow and smf_flat_polyfit.
*     - To simplify the API the heater settings for each flatfield
*       reading are returned in the smfDa component of "bolvald" (using
*       the "heatval" slot).

*  History:
*     2010-03-04 (TIMJ):
*        Initial version
*     2010-03-09 (TIMJ):
*        Also merge JCMTSTATE information.

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
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"

void smf_flat_mergedata( const smfArray * heatframes,
                         const double heatval[], smfData ** bolvald,
                         int * status ) {

  double * bolval = NULL; /* Output bolometer data */
  double * bolvalvar = NULL; /* Variance */
  dim_t i;
  dim_t nheat;           /* Number of input frames */
  dim_t numbol;          /* Number of bolometers */

  if (bolvald) *bolvald = NULL;

  if (*status != SAI__OK) return;

  if (!bolvald) {
    *status = SAI__ERROR;
    errRep( "", "Must provide a pointer for returned smfData"
            " (possible programming error)", status );
    return;
  }

  nheat = heatframes->ndat;
  numbol = (heatframes->sdata)[0]->dims[0] *
    (heatframes->sdata)[0]->dims[1];

  /* Create a smfData for powref and bolref */
  smf_flat_malloc( nheat, (heatframes->sdata)[0], NULL, bolvald, status );

  if (*status == SAI__OK) {
    bolval = (*bolvald)->pntr[0];
    bolvalvar = (*bolvald)->pntr[1];

    /* copy the data from the individual frames to the new smfData */
    for (i = 0; i<nheat; i++) {
      double * datpntr = &(bolval[ i * numbol ]);
      double * varpntr = &(bolvalvar[ i * numbol ] );

      memcpy( datpntr, (heatframes->sdata)[i]->pntr[0],
              numbol * smf_dtype_sz( (heatframes->sdata)[i]->dtype, status ) );

      /* just in case we are missing variance */
      if ( (heatframes->sdata)[i]->pntr[1] ) {
        memcpy( varpntr, (heatframes->sdata)[i]->pntr[1],
                numbol * smf_dtype_sz( (heatframes->sdata)[i]->dtype, status ) );
      } else {
        dim_t j;
        for (j = 0; j < numbol; j++) {
          varpntr[j] = VAL__BADD;
        }
      }
    }
  }

  /* Get the representative jcmtstate information */
  if (*status == SAI__OK) {
    JCMTState * state = astMalloc( nheat*sizeof(*state) );
    for (i = 0; i < nheat; i++) {
      smfData * frame = (heatframes->sdata)[i];
      memcpy( &(state[i]), &(frame->hdr->allState)[0], sizeof(*state) );
    }
    (*bolvald)->hdr->allState = state;
  }

  /* Store the heater values in a smfDA */
  if (*status == SAI__OK) {
    smfDA * da = NULL;
    double * dheatval = astMalloc( nheat*sizeof(*heatval) );
    memcpy( dheatval, heatval, nheat * sizeof(*heatval) );

    da = smf_construct_smfDA( NULL, NULL, NULL, NULL,
                              SMF__FLATMETH_NULL, 0, VAL__BADD,
                              dheatval, nheat, status );

    (*bolvald)->da = da;
  }

  return;
}
