/*
*+
*  Name:
*     smf_flat_override

*  Purpose:
*     Choose a relevant flatfield from a list and assign it to the smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     stored = smf_flat_override ( const smfArray *flats, smfData * indata,
*                                  int *status );

*  Arguments:
*     flats = const smfArray* (Given)
*        Set of flatfield observations. Can be NULL.
*     indata = smfData * (Given)
*        The smfData to receive the updated flatfield information. "heatval" will
*        only be updated if present in the smfDA struct of the chosen flatfield,
*        otherwise it will be left unchanged.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Assigns new flatfield information to a smfData copied from most
*     relevant flatfield in the supplied smfArray. Can do nothing if no
*     flatfield is relevant.

*  Returned Value:
*     Returns true if a new flatfield was assigned.

*  See Also:
*     Uses smf_choose_flat and smf_flat_assign.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-12-06 (TIMJ):
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

#define FUNC_NAME "smf_flat_override"

int smf_flat_override ( const smfArray *flats, smfData * indata,
                        int *status ) {

  dim_t flatidx = SMF__BADIDX;
  int retval = 0;

  if (*status != SAI__OK) return retval;

  smf_choose_flat( flats, indata, &flatidx, status );

  if ( flatidx != SMF__BADIDX ) {
    const smfData *flatdata = (flats->sdata)[flatidx];

    smf_smfFile_msg( indata->file, "INF", 1, "<unknown>" );
    smf_smfFile_msg( flatdata->file, "FLAT", 1, "<unknown>");
    msgOutif( MSG__VERB, "",
              "Override flatfield of ^INF using ^FLAT", status );
    smf_flat_assign( 1, SMF__FLATMETH_NULL, VAL__BADD, NULL, flatdata,
                     indata, status );
    retval = 1;
  } else {
    smf_smfFile_msg( indata->file, "INF", 1, "<unknown>" );
    msgOutif( MSG__VERB, "",
              "Did not find an override flatfield for ^INF", status );
  }

  return retval;

}
