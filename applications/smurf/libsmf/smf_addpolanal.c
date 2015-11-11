/*
*+
*  Name:
*     smf_addpolanal

*  Purpose:
*     Add a POLANAL Frame to the output FrameSet if required.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_addpolanal( AstFrameSet *fset, smfHead *hdr,
*                     AstKeyMap *config, int *status )

*  Arguments:
*     fset = AstFrameSet * (Given)
*        The WCS FrameSet to be added to the makemap output map. The
*        original base and current Frames are unchanged on exit.
*     hdr = SmfHead * (Given)
*        The header for an example input time-series file.
*     config = AstKeyMap * (Given)
*        A KeyMap containing the user-supplied configuration parameter
*        values.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     If the supplied smfHead indicates that the input time-series
*     data holds POL-2 Q or U values, then a new Frame is added to
*     the supplied FrameSet, which POLPACK will use to define the
*     polarimetric reference direction. The new Frame has domain POLANAL,
*     and its first axis (which defines the reference direction) is
*     parallel to celestial north in the coordinate system specified by
*     FITS Header POLNORTH.
*
*     The input data is assumed to be POL-2 data if the FITS header
*     contains a keyword "POLNORTH" (as written by smurf task CALCQU). An
*     error is reported if POLNORTH is "FPLANE" (i.e. if the Q/U values use
*     focal plane Y as the reference direction rather than celestial north).

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     5-JUN-2015 (DSB):
*        Initial version.
*     2-NOV-2015 (DSB):
*        POLNORTH now gives the celestial system in which north defines
*        the reference direction (before, it was always the tracking system).
*     11-NOV-2015 (DSB):
*        Add argument "config", and allow maps to be made with focal
*        plane Y axis as the reference direction (but only if "pol2fp"
*        is non-zero).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
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
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_addpolanal( AstFrameSet *fset, smfHead *hdr, AstKeyMap *config,
                     int *status ){

/* Local Variables */
   AstCmpMap *tmap;
   AstFrame *cfrm;
   AstFrame *pfrm;
   AstFrame *tfrm;
   AstFrameSet *tfs;
   AstPermMap *pm;
   char *polnorth = NULL;
   const char *cursys;
   const char *trsys;
   int aloff;
   int icurr;
   int inperm[2];
   int outperm[2];
   int pol2fp;

/* Check inherited status, and also check the supplied angle is not bad. */
   if( *status != SAI__OK ) return;

/* Begin an AST object context. */
   astBegin;

/* Get the value of the POLNORTH FITS keyword from the supplied header.
   The rest only happens if the keyword is found. */
   if( astGetFitsS( hdr->fitshdr, "POLNORTH", &polnorth ) ) {

/* Normally, we do not allow maps to be made from Q/U time streams that
   use focal plane Y as the reference direction (because of the problems
   of sky rotation). Therefore we report an error. However, we do need to
   make such maps as part of the process of determining the parameters of
   the Instrumental Polarisation (IP) model. So only report the error if
   "pol2fp" config parameter is non-zero. */
      if( !strcmp( polnorth, "FPLANE" ) ) {
         astMapGet0I( config, "POL2FP", &pol2fp );

         if( pol2fp ) {
            msgBlank( status );
            msgOut( "", "WARNING: The input NDFs hold POL-2 Q/U data specified "
                    "with respect to focal plane Y.",status );
            msgOut( "", "Maps should normally be made from POL-2 data specified "
                    "with respect to celestial north.",status );
            msgOut( "", "The output map will not contain a POLANAL Frame and "
                    "so will be unusable by POLPACK applications.",status );
            msgBlank( status );

         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "", "The input NDFs hold POL-2 Q/U data specified with "
                    "respect to focal plane Y.",status );
            errRep( "", "Maps can only be made from POL-2 data specified with "
                    "respect to celestial north.",status );
         }

/* If the ref. direction is celestial north, create a suitable Frame and
   Mapping and add them into the supplied FrameSet. */
      } else {

/* Check the current Frame is a SkyFrame. */
         cfrm = astGetFrame( fset, AST__CURRENT );
         if( astIsASkyFrame( cfrm ) ) {

/* Create a POLANAL Frame. */
            pfrm = astFrame( 2, "Domain=POLANAL" );
            astSet( pfrm, "Title=Polarimetry reference frame" );
            astSet( pfrm, "Label(1)=Polarimetry reference direction" );
            astSet( pfrm, "Label(2)=" );

/* Create a PermMap that ensures that axis 1 of the POLANAL Frame is parallel
   to the latitude axis (i.e. north) of the curent Frame (the current Frame axes
   may have been swapped). */
            outperm[ 0 ] = astGetI( cfrm, "LatAxis" );
            outperm[ 1 ] = astGetI( cfrm, "LonAxis" );
            inperm[ outperm[ 0 ] - 1 ] = 1;
            inperm[ outperm[ 1 ] - 1 ] = 2;
            pm = astPermMap( 2, inperm, 2, outperm, NULL, " " );

/* Record the index of the original current Frame. */
            icurr = astGetI( fset, "Current" );

/* Determine the system to use. */
            if( !strcmp( polnorth, "TRACKING" ) ) {
               trsys = sc2ast_convert_system( hdr->state->tcs_tr_sys, status );
            } else {
               trsys = polnorth;
            }

/* If the current Frame in the supplied FrameSet has this system. Then we
   use the above PermMap to connect the POLANAL Frame directly to the current
   Frame. */
            cursys = astGetC( cfrm, "System" );
            if( trsys && cursys && !strcmp( cursys, trsys ) ) {
               astAddFrame( fset, AST__CURRENT, pm, pfrm );

/* Otherwise we need to get a Mapping from the current Frame to the
   required frame. */
            } else {

/* Take a copy of the current Frame (in order to pick up epoch, observatory
   position, etc), and set its System to the required system. */
               tfrm = astCopy( cfrm );
               astSetC( tfrm, "System", trsys );

/* Get the Mapping from the original current Frame to this modified copy.
   Ensure alignment happens in absolute coords (alignment in offset
   coords is always a unit mapping and so no rotation occurs). */
               aloff = astGetI( cfrm, "AlignOffset" );
               if( aloff ) {
                  astSetI( cfrm, "AlignOffset", 0 );
                  astSetI( tfrm, "AlignOffset", 0 );
               }
               tfs = astConvert( cfrm, tfrm, "SKY" );
               if( aloff ) {
                  astSetI( cfrm, "AlignOffset", 1 );
                  astSetI( tfrm, "AlignOffset", 1 );
               }
               if( tfs ) {

/* Use it, in series with with the above PermMap, to connect the POLANAL frame
   to the current Frame. */
                  tmap = astCmpMap( astGetMapping( tfs, AST__BASE,
                                                   AST__CURRENT ),
                                    pm, 1, " " );
                  astAddFrame( fset, AST__CURRENT, astSimplify( tmap ),
                               pfrm );

/* Report an error if the mapping from current to required system could
   not be found. */
               } else if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( "", "smf_addpolanal: Could not convert Frame "
                           "from %s to %s (prgramming error).", status,
                           cursys, trsys );
               }
            }

/* Re-instate the original current Frame. */
            astSetI( fset, "Current", icurr );

/* Report an error if the current Frame is not a SkyFrame. */
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "", "smf_addpolanal: The current Frame in the "
                    "supplied FrameSet is not a SkyFrame (prgramming "
                    "error).", status );
         }
      }
   }

/* End the AST object context. */
   astEnd;
}

