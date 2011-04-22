/*
*+
*  Name:
*     smf_check_flat

*  Purpose:
*     Low-level routine to check if data are flatfielded

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_check_flat( const smfData *data, int *status );

*  Arguments:
*     data = const smfData* (Given)
*        Pointer to a smfData struct
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:

*     This subroutine determines whether the data have been
*     flatfielded by testing for the existence of the da component in
*     the smfData struct, and if the data type is double. Returns a
*     status of SMF__FLATN if either is true.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-12-05 (AGG):
*        Initial version.
*     2005-12-12 (AGG):
*        Update API prototype to reflect const input struct
*     2006-01-25 (AGG):
*        Minor change to logic on checking existence of smfDA
*     2006-03-29 (AGG):
*        Add check on data type
*     2011-04-21 (TIMJ):
*        Trap null smfData
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2005-2006 University of British Columbia. All
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "mers.h"
#include "sae_par.h"
#include "msg_par.h"

#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_check_flat"

void smf_check_flat ( const smfData *data, int *status ) {

  smfDA *da;
  smf_dtype dtype;

  if ( *status != SAI__OK ) return;
  if ( !data ) {
    *status = SAI__ERROR;
    errRep("", "Error checking if data are flatfielded. smfData is null."
           " (possible programming error)", status );
    return;
  }

  /* Retrieve the smfDa struct */
  da = data->da;
  dtype = data->dtype;

  /* Data need flatfielding if da is defined */
  if ( da == NULL || dtype == SMF__DOUBLE) {
    /* No raw data struct => data flatfielded */
    *status = SMF__FLATN;
    errRep(FUNC_NAME, "Data are already flatfielded", status);
  }

}
