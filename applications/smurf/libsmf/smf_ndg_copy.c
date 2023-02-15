/*
*+
*  Name:
*     smf_ndg_copy

*  Purpose:
*     Copy an NDG group, retaining any slaved metadata group.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     Grp *smf_ndg_copy( const Grp *grp1, dim_t indxlo, dim_t indxhi, int reject,
*                        int *status )

*  Arguments:
*     grp = const Grp * (Given)
*        Pointer to the input group to be copied.
*     indxlo = dim_t (Given)
*        The lowest index to reject or to copy.
*     indxhi = dim_t (Given)
*        The highest index to reject or to copy.
*     reject = int (Given)
*        If reject is non-zero, then names in the given range are
*        rejected.  Otherwise, names in the given range are copied.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Vaue:
*     A pointer to the new group.

*  Description:
*     This function is like ndgCopy but it retains any global metadata
*     associated with the input group (such as added by smf_add_grp_metadata).

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-MAY-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
#include "star/grp.h"
#include "star/ndg.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

Grp *smf_ndg_copy( const Grp *grp1, dim_t indxlo, dim_t indxhi, int reject,
                   int *status ){

/* Local Variables: */
   Grp *result;
   Grp *slave;

/* Initialise returned value */
   result = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Create the new group. */
   result = ndgCopy( grp1, indxlo, indxhi, reject, status );

/* Get any slave group that is owned by the supplied group. */
   slave = grpSlave( grp1, status );

/* If the supplied group has a slave, create a copy of the slave and
   associate it with the returned new group. */
   if( slave ) grpSown( grpCopy( slave, 0, 0, 0, status ), result, status );

/* Return the new group. */
   return result;
}

