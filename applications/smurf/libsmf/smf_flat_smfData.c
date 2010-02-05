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
*     smf_flat_smfData ( const smfData *data, smfData **powval,
*                        smfData **bolval, int *status );

*  Arguments:
*     data = const smfData * (Given)
*        smfData from which to extract the flatfield information.
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

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"

void smf_flat_smfData ( const smfData *data, smfData ** powval, smfData **bolval,
                        int *status ) {
  smfDA * da = NULL;
  void * pntr[3];
  dim_t dims[3];
  int lbnd[3];

  *powval = NULL;
  *bolval = NULL;

  if (*status != SAI__OK) return;

  if (!data) return;
  if (!(data->da)) return;

  da = data->da;

  /* flatpar is powval */
  pntr[0] = da->flatpar;
  pntr[1] = NULL;
  pntr[2] = NULL;
  dims[0] = da->nflat;
  *powval = smf_construct_smfData( NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                   pntr, 1, dims, NULL, 1, 0, 0, NULL,
                                   NULL, status );

  pntr[0] = da->flatcal;
  pntr[1] = NULL;
  pntr[2] = NULL;
  dims[0] = (data->dims)[0];
  dims[1] = (data->dims)[1];
  dims[2] = da->nflat;
  lbnd[0] = (data->lbnd)[0];
  lbnd[1] = (data->lbnd)[1];
  lbnd[2] = 1;
  *bolval = smf_construct_smfData( NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                   pntr, 1, dims, lbnd, 3, 0, 0, NULL,
                                   NULL, status );

  if (*status != SAI__OK) {
    if (*bolval) smf_close_file( bolval, status );
    if (*powval) smf_close_file( powval, status );
  }

  return;

}
