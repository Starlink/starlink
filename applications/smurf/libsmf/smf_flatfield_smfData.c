/*
*+
*  Name:
*     smf_flatfield_smfData

*  Purpose:
*     Flatfield a smfData in place

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     didflat = smf_flatfield_smfData( smfData *data, const smfArray * flats,
*                                      AstKeyMap * heateffmap, int force, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to a smfData struct to be flatfielded.
*     flats = const smfArray * (Given)
*        Array of flatfield data. If a relevant flatfield is found it
*        will be applied to "data" before flatfielding is calculated.
*     heateffmap = AstKeyMap * (Given)
*        Details of heater efficiency data to be applied during flatfielding.
*     force = int (Given)
*        Force flatfielding without checking whether the data are flatfielded.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     If required, flatfield the supplied smfData. Nothing is done
*     if the data have already been flatfielded. Can override
*     the flatfield information in the file if a relevant flatfield
*     is found in "flats".

*  Returned Value:
*     int
*        True if a flatfield was applied, false otherwise.

*  Notes:
*     - Use smf_flatfield to create an independent output smfData
*     - Use smf_flatten directly to apply a flatfield without updating
*       history (not recommended) or applying an override.
*     - The force argument is required because smf_check_flat is not
*       100% reliable (it does not examine history) and in some cases
*       we know we have done something that would make smf_check_flat
*       get the wrong answer. The best fix is to fix smf_check_flat.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2011-08-11 (TIMJ):
*        Initial version from smf_flatfield
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science and Technology Facilities Council.
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

#include <string.h>
#include <stdio.h>

#include "sae_par.h"
#include "mers.h"

#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_flatfield_smfData"

int smf_flatfield_smfData ( smfData *data, const smfArray * flats,
                            AstKeyMap * heateffmap, int force, int *status ) {

  if (*status != SAI__OK) return 0;

  /* See if data are flatfielded (unless forced) */
  if (!force) {
    smf_check_flat( data, status );

    /* Data are flatfielded if status set to SMF__FLATN */
    if ( *status == SMF__FLATN ) {
      errAnnul(status);
      return 0;
    }
  }

  /* See if we have an override flatfield. */
  smf_flat_override( flats, data, status );

  /* OK now apply flatfield calibration */
  smf_flatten( data, heateffmap, status);

  /* Write history entry to file */
  smf_history_add( data, "smf_flatfield", status);

  return 1;
}
