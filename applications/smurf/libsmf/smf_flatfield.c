/*
*+
*  Name:
*     smf_flatfield

*  Purpose:
*     Mid-level FLATFIELD implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_flatfield( ThrWorForce *wf, smfData *idata, const smfArray * flats, AstKeyMap * heateffmap,
*                    smfData **odata, const int flags, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     idata = smfData* (Given)
*        Pointer to a smfData struct
*     flats = const smfArray * (Given)
*        Array of flatfield data. If a relevant flatfield is found it
*        will be applied to idata before flatfielding is calculated.
*     heateffmap = AstKeyMap * (Given)
*        Details of heater efficiency data to be applied during flatfielding.
*     odata = smfData** (Given and Returned)
*        Pointer to a smfData struct. If *odata is NULL a new smfData*
*        will be created and stored in this location.
*     flags = const int (Given)
*        Flags to be passed to either smf_check_smfData or smf_deepcopy_smfData.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is a handler routine for determining if the lower-level
*     FLATFIELD task needs to be run. A history entry is written into
*     the output file if the flatfield task is run.

*  Notes:
*     - If an NDF file is involved then at the very least, the pointer
*     to the DATA array (pntr[0]) and its type (dtype) must be
*     defined.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-12-01 (AGG):
*        Initial test version.
*     2005-12-12 (AGG):
*        Add checks on data type and dimensions for the case that the
*        data need flatfielding and odata exists
*     2005-12-14 (AGG):
*        Now calls smf_clone_data for when the data are already flatfielded
*     2005-12-16 (AGG):
*        All combinations of flatfield & *odata statuses covered
*     2005-12-20 (AGG):
*        Further checks are now carried out to see if the WCS
*        component contains a sky frame
*     2005-12-21 (AGG):
*        Now deals with time series data correctly by not attempting
*        to copy the non-existent WCS info (which is added by
*        smf_tslice_ast)
*     2006-01-25 (AGG):
*        Add check on whether input header is null for unflatfielded data
*     2006-01-25 (TIMJ):
*        Replace malloc with smf_malloc
*     2006-01-27 (TIMJ):
*        - No longer have xloc member in smfFile
*        - use smf_create_smfHead rather than smf_malloc
*     2006-03-23 (AGG):
*        Store number of frames (timeslices) in smfData
*     2006-04-06 (AGG):
*        Major changes: all checking of data structures is done within
*        the new smf_check_smf*** routines.
*     2006-04-21 (AGG):
*        - update subroutine calls due to changed API
*        - add history updates
*     2010-03-15 (TIMJ):
*        Update prologue with new API. Slight clean ups.
*     2010-03-16 (TIMJ):
*        Accept override flatfields and assign them before processing.
*     2010-12-06 (TIMJ):
*        Use smf_flat_override
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 University of British Columbia &
*     Particle Physics and Astronomy Research Council.  All
*     Rights Reserved.

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

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "ast.h"
#include "ndf.h"
#include "star/thr.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_flatfield"

void smf_flatfield ( ThrWorkForce *wf, smfData *idata, const smfArray * flats, AstKeyMap * heateffmap,
                     smfData **odata, const int flags, int *status ) {

  if ( *status != SAI__OK ) return;

  /* See if data are flatfielded */
  smf_check_flat( idata, status );

  /* Data are flatfielded if status set to SMF__FLATN */
  if ( *status == SMF__FLATN ) {
    errAnnul(status);
    /* check *odata */
    if ( *odata == NULL) {
      msgOutif(MSG__DEBUG1," ",
               "OK, data are flatfielded and output struct is NULL: cloning input",
               status);
      /* If NULL then we need to clone idata to odata i.e. copy the
         pointer ONLY */
      smf_clone_data( idata, odata, status );
    } else {
      msgOutif(MSG__DEBUG1," ",
               "OK, data are flatfielded and odata exists", status);
      /* Check and set */
      smf_check_smfData( idata, *odata, flags, status );
    }
  } else if ( *status == SAI__OK ) {

    /* OK data are not flatfielded: create smfData based on input and
       apply flatfield */
    /* Check if *odata exists */
    if ( *odata == NULL) {
      msgOutif(MSG__DEBUG1," ","Data not flatfielded, no output data file.", status);
      /* If NULL then we need create odata not associated with a file
         (i.e. leave smfFile NULL) */
      /* Allocate space for *odata and all necessary cpts */
      /* Set the rawconvert flag to return doubles in the DATA array */
      *odata = smf_deepcopy_smfData( wf, idata, 1, flags, 0, 0, status );
    } else {
      /* OK, *odata exists */
      msgOutif(MSG__DEBUG1," ","Data not flatfielded, output data file exists.", status);
      /* Check and set */
      smf_check_smfData( idata, *odata, flags, status );
    }

    /* Disable the check because we know that we have just checked */
    smf_flatfield_smfData( *odata, flats, heateffmap, 1, status );

  }
}

