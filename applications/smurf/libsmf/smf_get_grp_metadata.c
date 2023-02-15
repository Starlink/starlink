/*
*+
*  Name:
*     smf_get_grp_metadata

*  Purpose:
*     Get an item of metadata from a GRP group

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_get_grp_metadata( const Grp *grp, const char *name,
*                                char *value, int *status )

*  Arguments:
*     grp = const Grp * (Given)
*        Pointer to the GRP from which metadata is to be retrieved.
*     name = const char * (Given)
*        A string giving the name of the metadata item to retrieve.
*     value = char * (Returned)
*        Pointer to a buffer in which to return the value associated with
*        the requested name. The buffer should be at least (GRP__SZNAM+1)
*        characters long.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function retrieves a named item of metadata from a specified
*     GRP group. Such metadata can be stored in the group using
*     smf_add_grp_metadata. If no value has been stored for the named item,
*     the contents of the supplied buffer are returned unchanged, but no
*     error is reported.

*  Notes:
*     - Metadata items are stored in a separate group that is associated
*     with the supplied group using the GRP "owner-slave" relationship.
*     The metadata group is made a slave of the supplied group. This should
*     not interfere with the supplemental groups used by NDG since NDG
*     assumes that the main group (i.e. the group holding the NDF names -
*     the group supplied to this function) is a slave of the supplemental
*     groups, leaving us free to add a slave group to the main group.

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

void smf_get_grp_metadata( const Grp *grp, const char *name, char *value,
                           int *status ){

/* Local Variables: */
   Grp *slave;                   /* Slave group */
   char buff[ GRP__SZNAM  + 1 ]; /* Buffer for group element text */
   dim_t size;                  /* Size of slave group */
   dim_t i;                     /* Index of current group element */
   dim_t nlen;                  /* Length of supplied name */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Get the slave group that is owned by the supplied group. */
   slave = grpSlave( grp, status );

/* Return the buffer unchanged if no slave group is available. */
   if( slave ) {

/* Record the length of the name of the metadata item. */
      nlen = strlen( name );

/* Sadly we need to check each element in the group to find any that
   match the name. */
      size = grpGrpsz( slave, status );
      for( i = 1; i <= size; i++ ) {

/* Get the text of the i'th element from the group. */
         grpInfoc( slave, i, "NAME", buff, sizeof( buff ), status );

/* If it is of the form "<name>=..." we have found the required item. */
         if( !strncmp( buff, name, nlen ) && buff[ nlen ] == '=' ) {

/* Copy the value into the returned buffer. */
            strcpy( value, buff + nlen + 1 );

/* Leave the loop. */
            break;
         }
      }
   }
}

