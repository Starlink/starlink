/*
*+
*  Name:
*     FINDSLICES

*  Purpose:
*     Find time slices that are centred close to a given sky position.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_findslices( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This application lists the time-slices within a given set of
*     time-series files for which the telescope was close to a specified
*     position on the sky. For each such position, the file, time slice
*     index and distance (in arc-seconds) is displayed on the screen.
*
*     Each time the telescope scans past the supplied position, a continuous
*     group of time slices will fall within the specified radius of the
*     given position. By default, only the closest time slice within each
*     such group is displayed. This can be changed using parameter CLOSEST.

*  ADAM Parameters:
*     CLOSEST = _LOGICAL (Read)
*        If TRUE, then only a single time slice (the closest) is displayed
*        for each pass of the telescope past the specified position. If
*        FALSE, then all time slices within the specified radius of the
*        specified positiopn are displayed. [TRUE]
*     FRAME = NDF (Read)
*        The position specified by parameters XPOS and YPOS is assumed to
*        be in the current WCS Frame of the NDF supplied for parameter
*        FRAME. If a null (!) value is supplied for FRAME, the position
*        is assumed to be in the tracking system of the first NDF supplied
*        for parameter IN.  [!]
*     IN = NDF (Read)
*        Input time-series file(s).
*     RADIUS = _DOUBLE (Read)
*        The radius of the search circle, in ars-seconds. [300]
*     XPOS = LITERAL (Read)
*        The first axis value at the search position. This should be a
*        formatted value for the first axis of the Frame specified by
*        parameter FRAME.
*     YPOS = LITERAL (Read)
*        The second axis value at the search position. This should be a
*        formatted value for the second axis of the Frame specified by
*        parameter FRAME.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-OCT-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "par.h"
#include "par_err.h"
#include "ast.h"
#include "ndf.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/grp.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"
#include "jcmt/state.h"

void smurf_findslices( int *status ) {

/* Local Variables: */
   AstFrame *frm = NULL;
   AstFrameSet *fs;
   AstFrameSet *iwcs;
   AstMapping *smap;
   AstMapping *tmap;
   Grp *igrp = NULL;
   JCMTState *state;
   char xpos[ 40 ];
   char ypos[ 40 ];
   const char *bestf = "";
   const char *dom;
   const char *sys;
   dim_t bestj = -1;
   dim_t jthis;
   double best;
   double d;
   double dlast;
   double dthis;
   double pos2[ 2 ];
   double pos[ NDF__MXDIM ];
   double radius;
   int close;
   int display;
   int done;
   int indf;
   int j;
   int nax;
   size_t i;
   size_t ssize;
   smfData *data = NULL;

/* Check inhereited status */
   if( *status != SAI__OK ) return;

/* Start new AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* Get a group of input files */
   kpg1Rgndf( "IN", 0, 1, "  Give more NDFs...", &igrp, &ssize, status );

/* Get the required minimum distance form the telescope boresight to the
   sky position, in arc-sec, and convert to radians. */
   parGet0d( "RADIUS", &radius, status );
   radius *= AST__DD2R/3600.0;

/* See if only the closest slices are to be listed. */
   parGet0l( "CLOSEST", &close, status );

/* Initialise closest approach distance. */
   best = VAL__MAXD;

/* Loop round the supplied input time-series files. */
   for( i = 0; i < ssize && *status == SAI__OK; i++ ) {

/* Open the file. */
      smf_open_file( NULL, igrp, (int) i + 1, "Read", SMF__NOCREATE_DATA, &data,
                     status );

/* If this is the first one, get the position to search for, and convert
   it to the tracking frame. */
      if( ! frm ) {

/* Get the spatial WCS FrameSet for the first time slice. */
         smf_tslice_ast( data, 0, 1, NO_FTS, status);
         if( data->hdr && data->hdr->wcs ) {

/* We need a Frame describing absolute tracking system coords. Take a
   copy of the skyframe (to inherit obslat, obslon, epoch, etc), and then
   set its system to the tracking system. */
            frm = astCopy( astGetFrame( data->hdr->wcs, AST__CURRENT ) );
            astSetC( frm, "System", sc2ast_convert_system(
                     (data->hdr->allState)[0].tcs_tr_sys, status ) );

/* Get the NDF defining the Frame in which the position is specified. If
   no NDF is supplied, annul the error and use the tracking Frame. Otherwise,
   get the WCS FrameSet from the NDF. */
            ndfAssoc( "FRAME", "Read", &indf, status );
            if( *status == PAR__NULL ) {
               errAnnul( status );
               iwcs = astClone( frm );
            } else {
               kpg1Gtwcs( indf, &iwcs, status );
            }

/* Get the two axis values. Convert the strings to numerical values using
   the current Frame in the supplied NDF. */
            parGet0c( "XPOS", xpos, sizeof(xpos)-1, status );
            if( astUnformat( iwcs, 1, xpos, pos ) == 0 && *status == SAI__OK ) {
               errRepf( "", "Bad value supplied for XPOS: '%s'", status, xpos );
            }

            parGet0c( "YPOS", ypos, sizeof(ypos)-1, status );
            if( astUnformat( iwcs, 2, ypos, pos+1 ) == 0 && *status == SAI__OK ) {
               errRepf( "", "Bad value supplied for YPOS: '%s'", status, ypos );
            }

/* Fill any remaining axes with bad values. */
            nax = astGetI( iwcs, "Naxes" );
            for( j = 2; j < nax; j++ ) pos[ j ] = VAL__BADD;

/* Get the Mapping from the Frame in which the position is supplied, to
   the tracking Frame. Report an error if conversion is not possible. */
            fs = astConvert( iwcs, frm, " " );
            if( !fs && *status == SAI__OK ) {
               dom = astGetC( iwcs, "Domain" );
               sys = astGetC( frm, "System" );
               *status = SAI__ERROR;
               errRepf( "", "Cannot convert from the supplied '%s' Frame "
                        "to the tracking frame (%s).", status, dom, sys );
            }

/* For accuracy, get a simplified Mapping. */
            tmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
            smap = astSimplify( tmap );

/* Convert the supplied position to the tracking Frame. */
            astTranN( smap, 1, nax, 1, pos, 1, 2, 1, pos );

            smap = astAnnul( smap );
            tmap = astAnnul( tmap );
         }
      }

/* Loop round all the time slices. */
      if( data->hdr && data->hdr->allState && frm ) {
         dlast = VAL__BADD;
         done = 0;
         state = data->hdr->allState;
         for( j = 0; j < data->hdr->nframes; j++,state++ ) {

/* Get the distance from the supplied position to the telescope
   boresight, at the current time slice, in radians. */
            pos2[ 0 ] = state->tcs_tr_ac1;
            pos2[ 1 ] = state->tcs_tr_ac2;
            d = astDistance( frm, pos, pos2 );

/* If it is below the specified limit, consider display it. */
            if( d != VAL__BADD ) {
               if( d < radius ) {

/* If we are only displaying details for each closest approach, and we
   have not yet displayed the closest approach for the current pass,
   and this time slice is further from the position than the previous
   time slice, we display details for the previous time slice. */
                  if( close ) {
                     display = 0;
                     if( dlast != VAL__BADD && dlast < d && !done ) {
                        dthis = dlast;
                        jthis = j - 1;
                        display = 1;
                        done = 1;
                     }

/* If we are displaying all time slices, display details of the current
   time slice. */
                  } else {
                     dthis = d;
                     jthis = j;
                     display = 1;
                  }

/* Display the details if required. */
                  if( display ) {
                     smf_smfFile_msg( data->file, "F", 1, "" );
                     msgSeti( "J", (int) jthis );
                     msgSetd( "D", dthis*AST__DR2D*3600.0 );
                     msgOutf( "", "^J ^D ^F", status );
                  }

/* Once we are outside the requested radius, we prepare for the next
   group. */
               } else {
                  done = 0;
               }

/* Record the overall closest approach. */
               if( d < best ) {
                  best = d;
                  bestf = data->file->name;
                  bestj = j;
               }
            }
            dlast = d;
         }
      }

/* Close the file. */
      smf_close_file( NULL, &data, status );
   }

   msgBlank( status );
   msgOutf( "", "Closest distance was %g arc-sec at time slice %zu of "
            "%s.", status, best*AST__DR2D*3600, bestj, bestf );
   msgBlank( status );


/* Free resources. */
   if( igrp ) grpDelet( &igrp, status);

/* End the NDF and AST contexts. */
   ndfEnd( status );
   astEnd;

/* Issue a status indication.*/
   if( *status == SAI__OK ) {
     msgOutif( MSG__VERB, " ", "FINDSLICES succeeded.", status);
   } else {
     msgOutif( MSG__VERB, " ", "FINDSLICES failed.", status);
   }
}
