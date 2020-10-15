/*
*+
*  Name:
*     smf_polext

*  Purpose:
*     Create a POLPACK extension in an output NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_polext( int ondf, int store_angle, double angle, const char *domain,
*                 int axis, int *status )

*  Arguments:
*     ondf = int (Given)
*        Identifier for the NDF to modify.
*     store_angle = int (Given)
*        Indicates if the supplied "angle" value should be stored in a
*        POLPACK extension in the NDF. If zero, the new POLPACK extension
*        is left empty, but the POLANAL Frame is still added to the WCS
*        FrameSet.
*     angle = double (Given)
*        Ignored if "store_angle" is zero. The position angle of the analysed
*        intensity (what POLPACK calls "ANLANG"). This is the angle from north
*        in the spatial coordinate system of the output, to the analyser axis.
*        Positive rotation is in the same sense as rotation from the first
*        spatial pixel axis to the second spatial pixel axis (as required by
*        POLPACK).
*     domain = const char * (Given)
*        The domain name for the WCS Frame in which the "angle" value is
*        specified. If a NULL pointer is supplied, the current Frame is
*        assumed.
*     axis = int (Given)
*        The zero-based index of the axis wihin the Frame specified by
*        "domain" that corresponds to the reference direction. Only used
*        if "domain" is supplied with a value other than SKY. Positive
*        rotation in the frame is assumed to be from axis 0 to axis 1.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     This function creates a POLPACK extension in the supplied NDF.
*     Values are put in the extension that tell POLPACK that the NDF holds
*     linearly analysed intensity, at the specified angle. These are the
*     values needed by the POLPACK:POLCAL task.
*
*     If the Domain is SKY, the reference direction is assumed to be
*     north. Otherwise, it is assumed to be the axis specified by "axis".

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     12-OCT-2007 (DSB):
*        Initial version.
*     9-AUG-2012 (DSB):
*        Added argument store_angle.
*     15-DEC-2012 (DSB):
*        Added argument "domain".
*     7-JAN-2013 (DSB):
*        Ensure that the original current WCS Frame is not changed.
*     8-JAN-2013 (DSB):
*        Add argument "axis".
*     7-JUL-2015 (DSB):
*        Indicate that sky separations below 0.05 arc-seconds (SC2AST__SKYTOL)
*        are insignificant.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
#include "ndf.h"
#include "star/hds.h"
#include "mers.h"
#include "sae_par.h"
#include "dat_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_polext( int ondf, int store_angle, double angle, const char *domain,
                 int axis, int *status ){

/* Local Variables */
   AstFrame *curfrm = NULL;
   AstFrame *polfrm = NULL;
   AstFrame *template = NULL;
   AstFrameSet *fs = NULL;
   AstFrameSet *wcs = NULL;
   AstMapping *map = NULL;
   HDSLoc *loc = NULL;
   double matrix[ NDF__MXDIM*NDF__MXDIM ];
   dim_t dummy;
   int iaxis;
   int icur;
   int ifrm;
   int naxes;
   int perm[ NDF__MXDIM ];

/* Check inherited status, and also check the supplied angle is not bad. */
   if( *status != SAI__OK || angle == AST__BAD ) return;

/* Start an AST context. */
   astBegin;

/* Get a pointer to the WCS FrameSet in the output NDF. */
   ndfGtwcs( ondf, &wcs, status );

/* Note the index of the original current Frame in "wcs" so that we can
   re-instate it later. */
   icur = astGetI( wcs, "Current" );

/* POLPACK uses the first axis of the WCS Frame with Domain POLANAL as the
   reference direction, so we need to add such a Frame to the WCS FrameSet.
   The "angle" value supplied by the caller is with respect to north in the
   WCS current Frame, so first get a pointer to the current WCS Frame. */
   if( !domain || !strcmp( domain, "SKY" ) ) {
      curfrm = astGetFrame( wcs, AST__CURRENT );

/* The current Frame is 3D, and should contain a SkyFrame. North should
   usually be axis 2 in the 3D Frame, but we make no assumptions here about
   how this axis is connected to the underlying SkyFrame axes. Instead,
   we create a default SkyFrame, permute its axes to make north (latitude)
   the first axis, and then search the 3D current WCS Frame to find a
   Frame that looks like the template. */
      template = (AstFrame *) astSkyFrame( "MaxAxes=3,SkyTol=%g", SC2AST__SKYTOL );
      perm[ 0 ] = 2;
      perm[ 1 ] = 1;
      astPermAxes( template, perm );
      fs = astFindFrame( curfrm, template, " " );

/* Check a match was found. */
      if( fs ) {

/* The base Frame in the "fs" FrameSet will be identical to "curfrm", and
   the current Frame will be a SkyFrame with axes permuted like "template",
   but will inherit other attributes from "curfrm". This is the Frame we
   will use as the POLANAL Frame. Extract the Mapping and current Frame
   from "fs". */
         map = astGetMapping( fs, AST__BASE, AST__CURRENT );
         polfrm = astGetFrame( fs, AST__CURRENT );
         ifrm = AST__CURRENT;

/* If the current WCS Frame did not contain a SkyFrame, warn the user. */
      } else {
         ndfMsg( "NDF", ondf );
         msgOut( "", "The WCS current Frame in output NDF ^NDF does not "
                 "contain a SkyFrame - no POLPACK extension can be created.",
                 status );
      }

/* If a non-SKY Domain was specified, find it, then reset the modified
   current Frame index. */
   } else {
      template = (AstFrame *) astFrame( 2, "MaxAxes=3,Domain=%s", domain );
      fs = astFindFrame( wcs, template, domain );
      astSetI( wcs, "Current", icur );

/* Check a match was found. */
      if( fs ) {

/* The base Frame in the "fs" FrameSet will be identical to the base
   Frame of "wcs", and the current Frame will be a Frame with the
   required Domain. POLPACK uses the first POLANAL axis as the reference
   direction - if we are using some other axis we need to rotate the
   Frame to put it first. Note, we rotate the frame rather than permute
   axes because we want to avoid swapping the sense of positive rotation,
   which should be from axis zero to axis one (as required by polpack).
   For cubes, we assume the first two axes are the spatial axes. */
         if( axis ==  1 ) {

            naxes = astGetI( fs, "Naxes" );
            memset( matrix, 0, sizeof( *matrix )*naxes*naxes );
            matrix[ 1 ] = 1.0;
            matrix[ naxes ] = -1.0;
            for( iaxis = 2; iaxis < naxes; iaxis++ ) matrix[ iaxis*naxes + iaxis ] = 1.0;
            map = (AstMapping *) astMatrixMap( naxes, naxes, 0, matrix, " " );
            astRemapFrame( fs, AST__CURRENT, map );
            map = astAnnul( map );

         } else if( axis != 0 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "smf_polext: Cannot use axis %d as the "
                    "polarimetry reference axis.", status, axis );
         }

/* This is now the Frame we will use as the POLANAL Frame. Extract the Mapping
   and current Frame from "fs". */
         map = astGetMapping( fs, AST__BASE, AST__CURRENT );
         polfrm = astGetFrame( fs, AST__CURRENT );
         ifrm = AST__BASE;

/* If the current WCS Frame did not contain a SkyFrame, warn the user. */
      } else {
         ndfMsg( "NDF", ondf );
         msgOutf( "", "The WCS current Frame in output NDF ^NDF does not "
                  "contain a Frame with Domain \"%s\" - no POLPACK "
                  "extension can be created.", status, domain);
      }
   }

/* Check a match was found. */
   if( fs ) {

/* Change the Domain to POLANAL. */
      astSet( polfrm, "Domain=POLANAL" );

/* Change the Frame title, and axis labels. */
      astSet( polfrm, "Title=Polarimetry reference frame" );
      astSet( polfrm, "Label(1)=Polarimetry reference direction" );
      astSet( polfrm, "Label(2)=" );

/* Add the POLANAL Frame into the WCS FrameSet using the above Mapping to
   connect it to the required Frame. */
      astAddFrame( wcs, ifrm, map, polfrm );

/* Re-instate the original current Frame (astAddFrame changes the current
   Frame). */
      astSetI( wcs, "Current", icur );

/* Save the modified FrameSet back in the output NDF. */
      ndfPtwcs( wcs, ondf, status );

/* Create the empty POLPACK extension. */
      ndfXnew( ondf, "POLPACK", "POLPACK", 0, &dummy, &loc, status );

/* If required, ctore the angle from the reference direction (north) to the
   effective analyser axis, measured positive in the same sense as rotation
   from the first to the second pixel axis. */
      if( store_angle ) ndfXpt0r( angle*AST__DR2D, ondf, "POLPACK", "ANLANG",
                                  status );

/* Annul the extension locator.  */
      (void) datAnnul( &loc, status );

/* If the current WCS Frame did not contain a SkyFrame, warn the user. */
   } else {
      ndfMsg( "NDF", ondf );
      if( domain ) {
         msgOutf( "", "The WCS current Frame in output NDF ^NDF does not "
                 "contain a Frame with Domain \"%s\" - no POLPACK extension "
                 "can be created.", status, domain );
      } else {
         msgOut( "", "The WCS current Frame in output NDF ^NDF does not "
                 "contain a SkyFrame - no POLPACK extension can be created.",
                 status );
      }
   }

/* End the AST context. */
   astEnd;
}
