/*
*+
*  Name:
*     smf_flat_smfData

*  Purpose:
*     Retrieve flatfield information from a smfData extension as two smfDatas

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_flat_smfData ( const smfData *data, smf_flatmeth * flatmethod,
*                        double * refres, smfData **powval, smfData **bolval,
*                        int *status );

*  Arguments:
*     data = const smfData * (Given)
*        smfData from which to extract the flatfield information.
*     flatmethod = smf_flatmeth * (Returned)
*        Flatfield method used for bolval. SMF__FLATMETH_TABLE or
*        SMF__FLATMETH_POLY.
*     refres = double * (Returned)
*        Reference resistance in ohms.
*     powval = smfData ** (Returned)
*        Resistance input powers. Will be returned NULL on error or if
*        no DA extension is present.
*     bolval = smfData ** (Returned)
*        Response of each bolometer to powval. Dimensioned as number of
*        number of bolometers times nheat. Will be returned NULL on error
*        or if no DA extension is present.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Read the DA extension and copy the flatfield information into two
*     smfDatas suitable for use in smf_flat_responsivity.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-01-28 (TIMJ):
*        Original version
*     2010-02-03 (TIMJ):
*        Set lbnd in returned smfData and also indicate the flat method.
*     2010-02-08 (TIMJ):
*        Need to malloc the memory for the local copy since the pointers
*        will be freed when the main file containing the smfDA is freed.
*     2010-03-03 (TIMJ):
*        Use smf_flat_malloc
*     2010-03-05 (TIMJ):
*        Use a smf_flatmeth type for flatfield method.
*     2012-02-22 (TIMJ):
*        Add refres argument.
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

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "star/one.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"

void smf_flat_smfData ( const smfData *data, smf_flatmeth * flatmethod,
                        double * refres, smfData ** powval, smfData **bolval,
                        int *status ) {
  smfDA * da = NULL;

  *powval = NULL;
  *bolval = NULL;
  *flatmethod = SMF__FLATMETH_NULL;

  if (*status != SAI__OK) return;

  if (!data) return;
  if (!(data->da)) return;

  da = data->da;

  /* we need to malloc space to make sure that we do not free the pointers
     twice. There really needs to be a way to tell smf_close_file to free
     everything except the pointers to the data. */
  smf_flat_malloc( da->nflat, data, powval, bolval, status );

  /* flatpar is powval */
  if (*status == SAI__OK) {
    dim_t nelem = (*bolval)->dims[0] * (*bolval)->dims[1] * (*bolval)->dims[2];
    memcpy( (*powval)->pntr[0], da->flatpar, da->nflat * sizeof(*(da->flatpar)) );
    memcpy( (*bolval)->pntr[0], da->flatcal, nelem * sizeof(*(da->flatcal)) );
  }

  *flatmethod = da->flatmeth;
  *refres = da->refres;

  if (*status != SAI__OK) {
    if (*bolval) smf_close_file( NULL, bolval, status );
    if (*powval) smf_close_file( NULL, powval, status );
  }

  return;

}
