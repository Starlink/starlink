/*
*+
*  Name:
*     smf_grp_new

*  Purpose:
*     Create a new GRP group with inherited metadata.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     Grp *smf_grp_new( const Grp *grp, const char *type, int *status )

*  Arguments:
*     grp = Grp * (Given)
*        Pointer to the existing GRP from which names will be copied.
*     type = const char * (Given)
*        A descriptive string to be associated with the group.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Vaue:
*     A pointer to the new group.

*  Description:
*     This function should be used in place of grpNew whenever the new
*     group will recieve copies of names from an existing group. It
*     checks the existing group for metadata (such as added using
*     smf_add_grp_metadata) and copies any such metadata to the new Grp.

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
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

Grp *smf_grp_new( const Grp *grp, const char *type, int *status ){

/* Local Variables: */
   Grp *result;
   Grp *slave;

/* Initialise returned value */
   result = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Create the new group. */
   result = grpNew( type, status );

/* Get any slave group that is owned by the supplied group. */
   slave = grpSlave( grp, status );

/* If the supplied group has a slave, create a copy of the slave and
   associate it with the returned new group. */
   if( slave ) grpSown( grpCopy( slave, 0, 0, 0, status ), result, status );

/* Return the new group. */
   return result;
}

