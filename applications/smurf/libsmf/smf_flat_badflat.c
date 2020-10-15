/*
*+
*  Name:
*     smf_flat_badflat

*  Purpose:
*     Fill a flatfield solution with bad values.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void smf_flat_bad( ThrWorkForce *wf, smfData * refdata, smfData ** resp, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     refdata = smfData * (Given)
*        smfData to be populated with the bad flatfield.
*     resp = smfData ** (Returned)
*        If non-NULL, will be filled with a bad responsivity image.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Attaches a bad flatfield to the data file and optionally creates
*     a bad responsivity image. Functionally the same inputs and outputs
*     as smf_flat_caclflat.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-06-05 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf.h"

#include "sae_par.h"

static void
smf__fill_smfData ( smfData * data, double dval, int *status );

void
smf_flat_badflat( ThrWorkForce *wf, smfData * refdata, smfData ** resp, int * status ) {
  smfData * bolrefd = NULL;
  smfData * powrefd = NULL;
  dim_t nheat = 1;

  if (*status != SAI__OK) return;
  if (!refdata) return;

  /* See if we have heater values */
  if (refdata->da && refdata->da->nheat > 0) {
    nheat = refdata->da->nheat;
  }

  /* Need to create bogus powref and bolref arrays */
  smf_flat_malloc( nheat, refdata, &powrefd, &bolrefd, status );

  smf__fill_smfData ( powrefd, VAL__BADD, status );
  smf__fill_smfData ( bolrefd, VAL__BADD, status );

  /* Create a responsivity image and fill with bad values */
  if (resp) {
    smf_create_bolfile( wf, NULL, 0, refdata, "Responsivity",
                        "A/W", SMF__MAP_VAR, resp, status );

    smf__fill_smfData ( *resp, VAL__BADD, status );
  }

  /* Attach the bad flatfield data */
  smf_flat_assign( 0, SMF__FLATMETH_POLY, 3.0, powrefd, bolrefd,
                   refdata, status );

}

/* Local helper routine for filling a smfData with a double.
   Should consider general routine but have to worry about data type
   verification */

static void
smf__fill_smfData ( smfData * data, double dval, int *status ) {
  dim_t ndata;
  if (*status != SAI__OK) return;
  if (!data) return;

  /* And fill the flatfield with bad values */
  if ( data->ndims == 1) {
    ndata = data->dims[0];
  } else {
    smf_get_dims( data, NULL, NULL, NULL, NULL, &ndata,
                  NULL, NULL, status );
  }
  if (*status == SAI__OK) {
    double *dpntr = (data->pntr)[0];
    dim_t i;
    for (i=0; i<ndata; i++) {
      dpntr[i] = dval;
    }
  }


}
