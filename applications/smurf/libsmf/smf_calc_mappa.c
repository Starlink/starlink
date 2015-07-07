/*
*+
*  Name:
*     smf_calc_mappa

*  Purpose:
*     Calculate the map position angle in a specified celestial
*     co-ordinate system.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     result = smf_calc_mappa( smfHead *hdr, const char *system,
*                              Frame *sf, int *status );

*  Arguments:
*     hdr = smfHead * (Given)
*        Pointer to header containing FITS keywords.
*     system = char* (Given)
*        Specifies the celestial coordinate system in which the map
*        position angle is required. It should be a valid value for the
*        System attribute of an AST SkyFrame, or "TRACKING".
*     sf = AstFrame * (Given)
*        A Frame that is used to determine the observers position, and the
*        the time at which the position angle is determined.
*     status = int * (Given and Returned)
*        The inherited status value.

*  Returned Value:
*     The angle from north in the requested system, through east, to the
*     second pixel axis, in radians.

*  Description:
*     This function converts the MAP_PA value stored in the supplied
*     header from the tacking system to a requested system.

*  Authors:
*     DSB: David S. Berry (JAC, UCLan)

*  History:
*     20-MAY-2008 (DSB):
*        Original version.
*     21-MAY-2008 (DSB):
*        Check for undefined MAP_PA values, and use zero instead.
*     10-JUN-2008 (DSB):
*        Prevent warnings from being issued about undefined MAP_PA values.
*     29-JUL-2008 (TIMJ):
*        Use astIsUndefF macro.
*     2-DEC-2008 (DSB):
*        Avoid use of astIsUndefF macro.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
#include "sae_par.h"

/* SMURF includes */
#include "sc2da/sc2ast.h"
#include "libsmf/smf.h"

double smf_calc_mappa( smfHead *hdr, const char *system, AstFrame *sf,
                       int *status ){

/* Local Variables */
   AstFrameSet *fs = NULL;    /* FrameSet joining tracking and requested systems */
   AstFrame *sf2 = NULL;      /* Frame in requested system */
   AstMapping *fmap = NULL;   /* Mapping from tracking to requested */
   const char *oldsys = NULL; /* Original System value for supplied Frame */
   const char *trsys = NULL;  /* AST tracking system */
   const char *usesys = NULL; /* AST system for output cube */
   double map_pa;             /* MAP_PA in tracking system (degs) */
   double p1[ 2 ];            /* Base pointing position */
   double p2[ 2 ];            /* A point on the map vertical axis */
   double p3[ 2 ];            /* A point north of the base pointing position */
   double result;             /* The returned angle */
   double xin[ 2 ];           /* Longitude values in tracking system */
   double xout[ 2 ];          /* Longitude values in requested system */
   double yin[ 2 ];           /* Latitude values in tracking system */
   double yout[ 2 ];          /* Latitude values in requested system */

/* Check inherited status */
   if (*status != SAI__OK) return 0.0;

/* Begin an AST context. */
   astBegin;

/* Determine the tracking system, and choose the celestial coordinate system
   for the output cube. */
   trsys = sc2ast_convert_system( hdr->state->tcs_tr_sys, status );
   if( !strcmp( system, "TRACKING" ) ) {
      usesys = trsys;
   } else {
      usesys = system;
   }

/* Save the System value of the supplied Frame, and set it to the
   tracking system. */
   oldsys = astGetC( sf, "System" );
   astSetC( sf, "System", trsys );

/* Get the base pointing position in the tracking system. */
   p1[ 0 ] = hdr->state->tcs_tr_bc1;
   p1[ 1 ] = hdr->state->tcs_az_bc2;

/* Move along the map "vertical" axis (as specified by the MAP_PA FITS
   header) for 1 arc-minute from the base pointing position. Set up a
   suitable default first in case the MAP_PA value is undefined. */
   map_pa = 0.0;
   smf_getfitsd( hdr, "MAP_PA", &map_pa, status );
   (void) astOffset2( sf, p1, map_pa*AST__DD2R, AST__DD2R/60.0, p2 );

/* Take a copy of the Frame and set its System value to the requested
   system. */
   sf2 = astCopy( sf );
   astSetC( sf2, "System", usesys );

/* Find a Mapping from the tracking system to the requested system. */
   fs = astConvert( sf, sf2, "" );
   fmap = astSimplify( astGetMapping( fs, AST__BASE, AST__CURRENT ) );

/* Use this Mapping to transform the above two positions from tracking to
   the requested system. */
   xin[ 0 ] = p1[ 0 ];
   yin[ 0 ] = p1[ 1 ];
   xin[ 1 ] = p2[ 0 ];
   yin[ 1 ] = p2[ 1 ];
   astTran2( fmap, 2, xin, yin, 1, xout, yout );
   p1[ 0 ] = xout[ 0 ];
   p1[ 1 ] = yout[ 0 ];
   p2[ 0 ] = xout[ 1 ];
   p2[ 1 ] = yout[ 1 ];

/* Create a 3rd position which 1 arc-minute to the north of the base
   pointing position in the requested system. */
   p3[ 0 ] = p1[ 0 ];
   p3[ 1 ] = p1[ 1 ] + AST__DD2R/60.0;

/* Find the angle subtended at p1 by p2 and p3. */
   result = astAngle( sf2, p3, p1, p2 );

/* Re-instate the original System value in the supplied Frame. */
   astSetC( sf, "System", oldsys );

/* Free resources. */
   astEnd;

/* Return the required angle. */
   return result;
}
