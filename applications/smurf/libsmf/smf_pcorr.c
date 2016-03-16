/*
*+
*  Name:
*     smf_pcorr

*  Purpose:
*     Add pointing corrections to the jiggle positions in a smfHead.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_pcorr( smfHead *head, const Grp *igrp, int *status )

*  Arguments:
*     head = smfHead * (Given)
*        Pointer to the smfHead structure conting the JCMTSTATE
*        information to be modified.
*     igrp = const Grp * (Given)
*        A group which has associated metadata contining Mappings from MJD
*        to DLON and DLAT, and the system (TRACKING or AZEL) of the
*        DLON/DLAT values. This information is stored in the group by
*        smf_pread.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function reads a table of longitude and latitude offsets with
*     associated times from the supplied text file. It then adds these
*     offsets onto the jiggle positions in the supplied smfHead, using
*     linear interpolation in the table to get offsets at non-tabulated
*     times.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-MAY-2011 (DSB):
*        Original version.
*     2012-01-04 (TIMJ):
*        Move the SMU correction code to smf_add_smu_pcorr.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012, 2011 Science & Technology Facilities Council.
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

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "jcmt/state.h"

static void smf1_pcorr( smfHead *head, const Grp *igrp, const char *name1,
                        const char *name2, const char *name3, int *status );

void smf_pcorr( smfHead *head, const Grp *igrp, int *status ){

/* Local Variables: */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* First apply the primary corrections (if any) given by metadata items
   DLONMAP, DLATMAP and PSYSTEM within the supplied group. */
   smf1_pcorr( head, igrp, "DLONMAP", "DLATMAP", "PSYSTEM", status );

/* Now apply the secondary corrections (if any) given by metadata items
   DLONMAP2, DLATMAP2 and PSYSTEM2 within the supplied group. */
   smf1_pcorr( head, igrp, "DLONMAP2", "DLATMAP2", "PSYSTEM2", status );
}

static void smf1_pcorr( smfHead *head, const Grp *igrp, const char *name1,
                        const char *name2, const char *name3, int *status ){

/* Local Variables: */
   AstMapping *dlatmap;
   AstMapping *dlonmap;
   JCMTState *state;
   char buff[ GRP__SZNAM + 1 ];
   dim_t iframe;
   double *dlat;
   double *dlon;
   double *tai;
   int azel;
   void *p;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* See if the supplied group has a metadata item with the name given by
   argument name1. If not, there are no pointing corrections to apply.
   This should be the DLONMAP item. */
   buff[ 0 ] = 0;
   smf_get_grp_metadata( igrp, name1, buff, status );
   if( buff[ 0 ] ) {

/* The value of the DLONMAP metadata item in the group is the formatted
   pointer to an AST Mapping from TAI MJD to arc-distance offsets
   parallel to the longitude axis (in arc-seconds). Unformat the text to
   get the usable pointer. */
      sscanf( buff, "%p", &p );
      dlonmap = (AstMapping *) p;

/* Likewise, get a pointer to the DLATMAP Mapping (given by argument name2),
   from TAI MJD to arc-distance offsets parallel to the latitude axis (in
   arc-seconds). */
      buff[ 0 ] = 0;
      smf_get_grp_metadata( igrp, name2, buff, status );
      if( !buff[ 0 ] ) {
         dlatmap = NULL;
         *status = SAI__ERROR;
         errRep( " ", "smf_pcorr: DLATMAP not found in group metadata.",
                 status );
      } else {
         sscanf( buff, "%p", &p );
         dlatmap = (AstMapping *) p;
      }

/* Also get the system of the axes - AZEL or TRACKING. Default to AZEL. */
      azel = 1;
      buff[ 0 ] = 0;
      smf_get_grp_metadata( igrp, name3, buff, status );
      if( buff[ 0 ] ) {
         buff[ astChrLen( buff ) ] = 0;
         if( astChrMatch( buff, "TRACKING" ) ) {
            azel = 0;
         } else if( !astChrMatch( buff, "AZEL" ) && *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc( "S", buff );
            errRep( " ", "smf_pcorr: Bad system (^S).", status );
         }
      }

/* Allocate arrays to hold the tai, dlon and dlat values at every frame. */
      tai = astMalloc( head->nframes*sizeof( double ) );
      dlon = astMalloc( head->nframes*sizeof( double ) );
      dlat = astMalloc( head->nframes*sizeof( double ) );
      if( *status == SAI__OK ) {

/* Store the TAI at every frame. */
         for( iframe = 0; iframe < head->nframes; iframe++ ){
            state = head->allState + iframe;
            tai[ iframe ] = state->tcs_tai;
         }

/* Lock the Mappings for use by this thread. Wait for them if they are
   currently in use by a different thread. */
         astLock( dlonmap, 1 );
         astLock( dlatmap, 1 );

/* Use the Mappings to transform TAI into DLON and DLAT values at every frame
   (still in arc-seconds). */
         astTran1( dlonmap, head->nframes, tai, 1, dlon );
         astTran1( dlatmap, head->nframes, tai, 1, dlat );

/* We have now finished with the Mappings, so unlock them so that hey can
   be used by other threads. */
         astUnlock( dlonmap, 1 );
         astUnlock( dlatmap, 1 );

/* Loop round every time slice */
         for( iframe = 0; iframe < head->nframes ; iframe++ ){
            state = head->allState + iframe;

/* Apply the correction */
            smf_add_smu_pcorr( state, azel, dlon[ iframe ],
                               dlat[ iframe ], status );

         }
      }

/* Free resources. */
      tai = astFree( tai );
      dlon = astFree( dlon );
      dlat = astFree( dlat );
   }
}

