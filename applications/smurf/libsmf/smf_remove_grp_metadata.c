/*
*+
*  Name:
*     smf_remove_grp_metadata

*  Purpose:
*     Remove an item of metadata from a GRP group

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_remove_grp_metadata( Grp *grp, const char *name,
*                                   int *status )

*  Arguments:
*     grp = Grp * (Given)
*        Pointer to the GRP to which metadata is to be added.
*     name = const char * (Given)
*        A string giving the name of the metadata item to remove.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function removes an item of metadata previously associated
*     with a specified GRP group using smf_add_grp_metadata. No error is
*     reported if the item does not exist.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-MAY-2011 (DSB):
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

void smf_remove_grp_metadata( Grp *grp, const char *name, int *status ){

/* Local Variables: */
   Grp *newslave;                /* New slave group */
   Grp *slave;                   /* Slave group */
   char buff[ GRP__SZNAM  + 1 ]; /* Buffer for group element text */
   int changed;                  /* Have slave group been changed? */
   dim_t i;                     /* Index of current group element */
   dim_t nlen;                  /* Length of supplied name */
   dim_t size;                  /* Size of slave group */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Get the slave group that is owned by the supplied group. */
   slave = grpSlave( grp, status );
   if( slave ) {

/* Create a new slave group that excludes any existing value for the specified
   name. */
      changed = 0;
      newslave = grpNew( " ", status );
      nlen = strlen( name );
      size = grpGrpsz( slave, status );
      for( i = 1; i <= size; i++ ) {
         grpInfoc( slave, i, "NAME", buff, sizeof( buff ), status );
         if( strncmp( buff, name, nlen ) || buff[ nlen ] != '=' ) {
            grpPut1( newslave, buff, 0, status );
         } else {
            changed = 1;
         }
      }

      if( changed ) {
         grpSown( slave, NULL, status );
         grpDelet( &slave, status );
         slave = newslave;
         grpSown( slave, grp, status );
      } else {
         grpDelet( &newslave, status );
      }
   }
}

