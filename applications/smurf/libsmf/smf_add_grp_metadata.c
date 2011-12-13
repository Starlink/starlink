/*
*+
*  Name:
*     smf_add_grp_metadata

*  Purpose:
*     Add items of metadata to a GRP group

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_add_grp_metadata( Grp *grp, const char *name,
*                                const char *value, int *status )

*  Arguments:
*     grp = Grp * (Given)
*        Pointer to the GRP to which metadata is to be added.
*     name = const char * (Given)
*        A string giving the name of the metadata item to add to the group.
*     value = const char * (Given)
*        A string giving the value of the metadata item to add to the group.
*        If this is NULL, then the supplied "name" is assumed to be the name
*        of an environment parameter, and the value of the metadata item
*        is determined by getting a literal value for the parameter using
*        function parGet0C.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function allows global items of metadata to be associated with
*     a specified GRP group. Each item is stored as  a "<name>=<value>"
*     string in a separate GRP group, which is associated with the supplied
*     group. Metadata items can be retrieved using "smf_get_grp_metadata".
*
*     The value for the metadata item can be supplied as a function
*     argument ("value") or obtained through a speicfied environment
*     parameter. If a null (!) value is supplied for the parameter, then
*     the error is annulled and no metadata is added to the group.

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
#include "ast.h"
#include "star/grp.h"
#include "mers.h"
#include "sae_par.h"
#include "par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_add_grp_metadata( Grp *grp, const char *name, const char *value,
                           int *status ){

/* Local Variables: */
   Grp *slave;                   /* Slave group */
   char *item;                   /* Buffer for formatted item */
   char vbuff[ GRP__SZNAM ];     /* Buffer for user-supplied value */
   int nc;                       /* Length of item */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Delete any existing value for the metadata item. */
   smf_remove_grp_metadata( grp, name, status );

/* If no value was supplied, get a value from the environment using the
   supplied name as a parameter name. */
   if( !value ) {
      parGet0c( name, vbuff, sizeof( vbuff ) - 1, status );

/* If a null parameter value was supplied, annul the error and retain the
   NULL value pointer. Otherwise, if no error occurred, use the
   user-supplied value in future. */
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else if( *status == SAI__OK ){
         value = vbuff;
      }
   }

/* Do nothing more if we have no value string. */
   if( value ) {

/* Get the slave group that is owned by the supplied group. */
      slave = grpSlave( grp, status );

/* If the supplied group does not have a slave, create a new group now
   and make it the slave of the supplied group. */
      if( !slave ) {
         slave = grpNew( " ", status );
         grpSown( slave, grp, status );
      }

/* Format the metadata item. */
      item = NULL;
      nc = 0;
      item = astAppendString( item, &nc, name );
      item = astAppendString( item, &nc, "=" );
      item = astAppendString( item, &nc, value );

/* Report an error if the item is too long */
      if( nc > GRP__SZNAM && *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "I", item );
         errRep( " ", "smf_add_grp_metadata: GRP metadata item is too "
                 "long: '^I'", status );
      }

/* Store the item in the slave group. */
      grpPut1( slave, item, 0, status );

/* Free resources. */
      item = astFree( item );

   }
}

