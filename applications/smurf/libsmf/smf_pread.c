/*
*+
*  Name:
*     smf_pread

*  Purpose:
*     Read pointing corrections and store with a Grp group.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_pread( Grp *igrp, const char *param, int *status )

*  Arguments:
*     igrp = Grp * (Given)
*        Pointer to the Grp with which to store the pointing corrections,.
*     param = const char * (Given)
*        The name of an environment parameter from which to obtain the name
*        of a text file contining a table of longitude and latitude offsets
*        against time. The file should be in TOPCAT "ascii" format with at
*        least three columns called TAI, DLON and DLAT. The TAI column is
*        the TAI (as an MJD), the DLON column is the longitude offset in
*        arc-seconds, and the DLAT column is the latitude offset arc-seconds.
*        The longitude and latitude axes are AZEL unless the table contains
*        a comment line of the form "# SYSTEM=TRACKING". The TAI values
*        should be monotonic increasing with row number.
*
*        If the parameter name is NULL, any Mapping previously associated
*        with the group are annulled and the function then returns
*        without further action.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function reads a table of longitude and latitude offsets
*     with associated times from a text file specified via the supplied
*     parameter. It then creates an AST Mapping that maps TAI value into
*     DLON value (using linear interpolation between tabulated values)
*     and that maps TAI into DLAT value. It stores formatted pointers to
*     these two Mappings as metadata items "DLONMAP" and "DLATMAP" in the
*     supplied Grp group.

*  Notes:
*     - The Mappings are unlocked on exit from this routine. This means
*     that they can be used within a different thread. However, they must be
*     locked using astLock before using them, and they must also be
*     unlocked using astUnlock once they have been finished with.
*     - The Mappings should be annulled when no longer needed by calling
*     this function with a NULL parameter name.
*     - See also smf_pcorr which uses these Mappings to to correct the
*     jiggle offsets in a smfData.

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "star/atl.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "jcmt/state.h"

void smf_pread( Grp *igrp, const char *param, int *status ){

/* Local Variables: */
   AstMapping *dlatmap;
   AstMapping *dlonmap;
   AstMapping *taimap;
   AstTable *table;
   char file[ GRP__SZNAM + 1 ];
   char pbuf[ GRP__SZNAM + 1 ];
   const char *system;
   void *p;

/* Before we check the error status, see if we are annulling previously
   created Mappings. If so, get each formatted pointer from the group
   metadata, get an Object pointer form it, lock it for use by the
   current thread, and then annul it. Remove the metadata item from the
   group. */
   if( !param ) {

      pbuf[ 0 ] = 0;
      smf_get_grp_metadata( igrp, "DLONMAP", pbuf, status );
      if( pbuf[ 0 ] ) {
         sscanf( pbuf, "%p", &p );
         dlonmap = (AstMapping *) p;
         astLock( dlonmap, 0 );
         dlonmap = astAnnul( dlonmap );
         smf_remove_grp_metadata( igrp, "DLONMAP", status );
      }

      pbuf[ 0 ] = 0;
      smf_get_grp_metadata( igrp, "DLATMAP", pbuf, status );
      if( pbuf[ 0 ] ) {
         sscanf( pbuf, "%p", &p );
         dlatmap = (AstMapping *) p;
         astLock( dlatmap, 0 );
         dlatmap = astAnnul( dlatmap );
         smf_remove_grp_metadata( igrp, "DLATMAP", status );
      }

      return;
   }

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Use the specified parameter to get the name of the text file containing
   the table of pointing corrections. */
   parGet0c( param, file, sizeof( file ) - 1, status );

/* If no file was specified, annul the error. */
   if( *status == PAR__NULL ) {
      errAnnul( status );

/* If a file was obtained sccuesfully, read it. */
   } else if( *status == SAI__OK ) {

/* Start an AST context. */
      astBegin;

/* Attempt to read an AST Table from the text file. */
      table = atlReadTable( file, status );

/* Create a LutMap from each of the three columns. */
      taimap = (AstMapping *) atlTablelutMap( table, "TAI", status );
      dlonmap = (AstMapping *) atlTablelutMap( table, "DLON", status );
      dlatmap = (AstMapping *) atlTablelutMap( table, "DLAT", status );

/* Create Mappings that transforms TAI into a DLON and DLAT. These use
   linear interpolation for non-tabulated TAI values. */
      astInvert( taimap );
      dlonmap = (AstMapping *) astCmpMap( taimap, dlonmap, 1, " " );
      dlatmap = (AstMapping *) astCmpMap( taimap, dlatmap, 1, " " );

/* Format the pointers to these two Mappings and store them in the
   supplied group using names "DLONMAP" and "DLATMAP". */
      sprintf( pbuf, "%p", (void *) dlonmap );
      smf_add_grp_metadata( igrp, "DLONMAP", pbuf, status );

      sprintf( pbuf, "%p", (void *) dlatmap );
      smf_add_grp_metadata( igrp, "DLATMAP", pbuf, status );

/* See what system the DLON/DLAT values refer to (default to AZEL). Store
   it in the group.  */
      if( !astMapGet0C( table, "SYSTEM", &system ) ) system = "AZEL";
      smf_add_grp_metadata( igrp, "PSYSTEM", system, status );

/* Unlock the pointers to the Mappings so that they can be used by a
   different thread. This also exempts the pointers from AST context
   handling (until they are re-locked) so the following call to astEnd
   will not annull them. */
      astUnlock( dlonmap, 1 );
      astUnlock( dlatmap, 1 );

/* End the AST context. This annuls all Objects created during the
   context, except for the unlocked Mappings. */
      astEnd;

/* Debug message. */
      msgSetc( "F", file );
      msgSetc( "S", system );
      msgOutif( MSG__DEBUG, " ", "^S pointing corrections read from file ^F",
                status );

/* Issue a context message if anything went wrong. */
      if( *status != SAI__OK ) {
         msgSetc( "F", file );
         errRep( " ", "Failed to read pointing corrections from text file ^F.",
                 status );
      }
   }
}

