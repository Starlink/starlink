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
*     void smf_pcorr( smfHead *head, const char *file, int *status )

*  Arguments:
*     head = smfHead * (Given)
*        Pointer to the smfHead structure conting the JCMTSTATE
*        information to be modified.
*     file = const char * (Given)
*        The name of the text file contining a table of longitude and
*        latitude offsets against time. The file should be in TOPCAT
*        "ascii" format with three columns called MJD DLON and DLAT.
*        The MJD column is the TAI MJD, the DLON column is the longitude
*        offset in arc-seconds, and the DLAT column is the latitude offset
*        arc-seconds. The longitude and latitude axes are AZEL unless the
*        table contains a comment line of the form "# SYSTEM=TRACKING".
*        The MJD values should be monotonic increasing with row number.
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
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "jcmt/state.h"

void smf_pcorr( smfHead *head, const char *file, int *status ){

/* Local Variables: */
   AstMapping *dlatmap;
   AstMapping *dlonmap;
   AstMapping *mjdmap;
   AstTable *table;
   JCMTState *state;
   const char *system;
   dim_t iframe;
   double *dlat;
   double *dlon;
   double *mjd;
   double cosrot;
   double dlat_az;
   double dlat_tr;
   double dlon_az;
   double dlon_tr;
   double rot;
   double sinrot;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Begin an AST context */
   astBegin;

/* Attempt to read an AST Table from the text file. */
   table = atlReadTable( file, status );

/* Create a LutMap from each of the three columns. */
   mjdmap = (AstMapping *) atlTablelutMap( table, "MJD", status );
   dlonmap = (AstMapping *) atlTablelutMap( table, "DLON", status );
   dlatmap = (AstMapping *) atlTablelutMap( table, "DLAT", status );

/* Create Mappings that transforms MJD into a DLON and DLAT. These use
   linear interpolation for non-tabulated MJD values. */
   astInvert( mjdmap );
   dlonmap = (AstMapping *) astCmpMap( mjdmap, dlonmap, 1, " " );
   dlatmap = (AstMapping *) astCmpMap( mjdmap, dlatmap, 1, " " );

/* Allocate arrays to hold the mjd, dlon and dlat values at every frame. */
   mjd = astMalloc( head->nframes*sizeof( double ) );
   dlon = astMalloc( head->nframes*sizeof( double ) );
   dlat = astMalloc( head->nframes*sizeof( double ) );
   if( *status == SAI__OK ) {

/* Store the MJD at every frame. */
      for( iframe = 0; iframe < head->nframes; iframe++ ){
         state = head->allState + iframe;
         mjd[ iframe ] = state->tcs_tai;
      }

/* Use the above Mappings to transform MJD into DLON and DLAT values at
   every frame (still in arc-seconds). */
      astTran1( dlonmap, head->nframes, mjd, 1, dlon );
      astTran1( dlatmap, head->nframes, mjd, 1, dlat );

/* See what system the DLON/DLAT values refer to. Default to AZEL. */
      if( !astMapGet0C( table, "SYSTEM", &system ) ) system = "AZEL";

/* Loop round every time slice */
      dlon_az = AST__BAD;
      dlat_az = AST__BAD;
      dlon_tr = AST__BAD;
      dlat_tr = AST__BAD;
      for( iframe = 0; iframe < head->nframes; iframe++ ){
         state = head->allState + iframe;

/* Get the rotation, in rads, from tracking north to elevation, measured
   positive from tracking north to tracking east. */
         rot = state->tcs_tr_ang + state->tcs_az_ang;
         cosrot = cos( rot );
         sinrot = sin( rot );

/* Get the DLON/DLAT values in tracking and in azel. */
         if( !strcmp( system, "AZEL" ) ) {
            dlon_az = dlon[ iframe ];
            dlat_az = dlat[ iframe ];
            dlon_tr = -dlon_az*cosrot + dlat_az*sinrot;
            dlat_tr =  dlon_az*sinrot + dlat_az*cosrot;

         } else if( !strcmp( system, "TRACKING" ) ) {
            dlon_tr = dlon[ iframe ];
            dlat_tr = dlat[ iframe ];
            dlon_az = -dlon_tr*cosrot + dlat_tr*sinrot;
            dlat_az =  dlon_tr*sinrot + dlat_tr*cosrot;

         } else if( *status == SAI__OK ){
            msgSetc( "S", system );
            *status = SAI__ERROR;
            errRep( " ", "Unknown SYSTEM '^S' specified in pointing "
                    "corrections file.", status );
            break;
         }

/* Add the dlon and dlat values onto the SMU jiggle positions (note, these
   are in units of arc-seconds, not radians). */
         if(  state->smu_az_jig_x == VAL__BADD ) {
            state->smu_az_jig_x = dlon_az;
         } else {
            state->smu_az_jig_x += dlon_az;
         }

         if(  state->smu_az_jig_y == VAL__BADD ) {
            state->smu_az_jig_y = dlat_az;
         } else {
            state->smu_az_jig_y += dlat_az;
         }

         if(  state->smu_tr_jig_x == VAL__BADD ) {
            state->smu_tr_jig_x = dlon_tr;
         } else {
            state->smu_tr_jig_x += dlon_tr;
         }

         if(  state->smu_tr_jig_y == VAL__BADD ) {
            state->smu_tr_jig_y = dlat_tr;
         } else {
            state->smu_tr_jig_y += dlat_tr;
         }
      }
   }

/* Free resources. */
   mjd = astFree( mjd );
   dlon = astFree( dlon );
   dlat = astFree( dlat );

/* End the AST context */
   astEnd;

/* Issue a context message if anything went wrong. */
   if( *status != SAI__OK ) {
      msgSetc( "F", file );
      errRep( " ", "Failed to read pointing corrections from text file ^F.",
              status );
   }
}

