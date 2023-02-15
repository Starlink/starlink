/*
*+
*  Name:
*     smf_lock_related

*  Purpose:
*     Lock or unlock all AST Objects within a smfArray structure.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_lock_array( smfArray *data, int lock, int *status )

*  Arguments:
*     data = smfArray * (Given)
*        Pointer to the smfArray to be unlocked.
*     lock = int (Given)
*        If non-zero, then astLock is called. Otherwise, astUnlock is
*        called.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Description:
*     This function call astLock ot astUnlock on all AST objects within
*     the smfData's in the smfArray.

*  Notes:
*     - This function attempts to execute even if an error has occurred.

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     24-NOV-2008 (EC):
*        Initial version, multiple calls to smf_lock_data.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 University of British Columbia.
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

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "ems.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

void smf_lock_related( smfArray *data, int lock, int *status ){

  /* Local Variables */
  dim_t i;           /* Loop counter */

  if( *status != SAI__OK ) return;

  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", "smf_lock_related: NULL smfArray provided", status );
    return;
  }

  for( i=0; i<data->ndat; i++ ) {
    smf_lock_data( data->sdata[i], lock, status );
  }

}

