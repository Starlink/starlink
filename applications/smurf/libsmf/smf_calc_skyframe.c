/*
*+
*  Name:
*     smf_calc_skyframe

*  Purpose:
*     Calculate the output skyframe

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_calc_skyframe( const AstFrame *skyin, const char * system,
*                        const smfHead* hdr, 
*                        int alignsys, AstSkyFrame ** skyframe,
*                        double skyref[2],
*                        int * moving, int * status )

*  Arguments:
*     skyin = const AstFrame * (Given)
*        Input sky frame to use as reference.
*     system = char* (Given)
*        Specifies the celestial coordinate system which will be used to
*        describe the spatial axes of the output cube. It should be a 
*        valid value for the System attribute of an AST SkyFrame, or
*        "TRACKING". 
*     hdr = const smfHead* (Given)
*        Header to be used to calculate base position and tracking system.
*     alignsys = int (Given)
*        If non-zero, then the input data will be aligned in the coordinate 
*        system specified by "system" rather than in the default system
*        (ICRS).
*     skyframe = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST 
*        SkyFrame describing the spatial axes of the output WCS FrameSet.
*        If "moving" is non-zero, the spatial axes represent (lon,lat) 
*        offsets (in the requested "system") from the base telescope position 
*        associated with the first time slice.
*     skyref = double[2] (Returned)
*        Reference position to be used for this sky frame.
*     moving = int* (Returned)
*        Address of an int in which to return a flag indicating if the 
*        telescope is tracking a moving object. If so, the returned
*        SkyFrame will describe offsets (in the system specified by "system")
*        from the base pointing position for the first time slice.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:


*  Authors:
*     DSB: David Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-11-14 (DSB):
*        Original smf_cubegrid implementation.
*     2008-06-04 (TIMJ):
*        Initial version factored out of smf_cubegrid
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
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

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "star/slalib.h"

/* SMURF includes */
#include "sc2da/sc2ast.h"
#include "smurf_par.h"
#include "libsmf/smf.h"

void smf_calc_skyframe( const AstFrame *skyin, const char * system,
                        const smfHead* hdr, 
                        int alignsys, AstSkyFrame ** skyframe,
                        double skyref[2],
                        int * moving, int * status ) {

   double az[ 2 ];       /* Azimuth values */
   AstMapping *azel2usesys = NULL; /* Mapping form AZEL to requested system */
   double dec[ 2 ];      /* Dec values */
   double el[ 2 ];       /* Elevation values */
   double ra[ 2 ];       /* RA values */
   double sep;           /* Separation between first and last base positions */
   AstFrame *sf1 = NULL;      /* Spatial Frame representing AZEL system */
   AstFrame *sf2 = NULL;      /* Spatial Frame representing requested system */
   const char *usesys = NULL; /* AST system for output cube */

  if (*status != SAI__OK) return;

/* Determine the tracking system, and choose the celestial coordinate system 
   for the output cube. */
  if( !strcmp( system, "TRACKING" ) ) {
    usesys = sc2ast_convert_system( hdr->state->tcs_tr_sys, status );
  } else {
    usesys = system;
  }

/* Create a SkyFrame by copying the input SkyFrame (in order to inherit
   all the other attributes like Epoch, Equinox, ObsLat, ObsLon, Dut1, etc)
   and then set its System to the required system. */
  *skyframe = astCopy( skyin );
  astSetC( *skyframe, "System", usesys );

/* If required, ensure that alignment on the sky is performed in the output
   coordinate system rather than the default (ICRS). */
  if( alignsys ) astSetC( *skyframe, "AlignSystem", usesys );

/* We will later record the telescope base pointing position as the SkyRef 
   attribute in the output SkyFrame. To do this, we need to convert the 
   stored telescope base pointing position from AZEL to the requested
   output system. Create a Mapping to do this using astConvert, and then
   use the Mapping to transform the stored position. */
  sf1 = astCopy( *skyframe );
  astSetC( sf1, "System", "AZEL" );
  azel2usesys = astConvert( sf1, *skyframe, "" );
  astTran2( azel2usesys, 1, &(hdr->state->tcs_az_bc1),
            &(hdr->state->tcs_az_bc2), 1, skyref, skyref + 1 );
  azel2usesys = astAnnul( azel2usesys );
/* Normalise these values. */
  astNorm( *skyframe, skyref );

/* Determine if the telescope is tracking a moving target such as a planet 
   or asteroid. This is indicated by significant change in the telescope 
   base pointing position within the output coordinate system. Here, 
   "significant" means more than 1 arc-second. Apparently users will only 
   want to track moving objects if the output cube is in AZEL or GAPPT, so 
   we ignoring a moving base pointing position unless the output system 
   is AZEL or GAPPT. */
  if( !strcmp( usesys, "AZEL" ) || !strcmp( usesys, "GAPPT" ) ) {

/* Get a copy of the output SkyFrame. */
    sf2 = astCopy( *skyframe );

/* Set the Epoch for `sf1' andf `sf2' to the epoch of the first time slice, 
   then use  the Mapping from `sf1' (AzEl) to `sf2' (output system) to 
   convert the telescope base pointing position for the first time slices 
   from (az,el) to the output system. */
    astSet( sf1, "Epoch=MJD %.*g", DBL_DIG, 
            (hdr->allState)[ 0 ].tcs_tai + 32.184/86400.0 );
    astSet( sf2, "Epoch=MJD %.*g", DBL_DIG, 
            (hdr->allState)[ 0 ].tcs_tai + 32.184/86400.0 );
    az[ 0 ] = (hdr->allState)[ 0 ].tcs_az_bc1;
    el[ 0 ] = (hdr->allState)[ 0 ].tcs_az_bc2;
    astTran2( astConvert( sf1, sf2, "" ), 1, az, el, 1, ra, dec );

/* Set the Epoch for `sf1' andf `sf2' to the epoch of the last time slice, 
   then use  the Mapping from `sf1' (AzEl) to `sf2' (output system) to 
   convert the telescope base pointing position for the last time slices 
   from (az,el) to the output system. */
    astSet( sf1, "Epoch=MJD %.*g", DBL_DIG, 
            (hdr->allState)[ hdr->nframes - 1 ].tcs_tai + 32.184/86400.0 );
    astSet( sf2, "Epoch=MJD %.*g", DBL_DIG, 
            (hdr->allState)[ hdr->nframes - 1 ].tcs_tai + 32.184/86400.0 );
    az[ 1 ] = (hdr->allState)[ hdr->nframes - 1 ].tcs_az_bc1;
    el[ 1 ] = (hdr->allState)[ hdr->nframes - 1 ].tcs_az_bc2;
    astTran2( astConvert( sf1, sf2, "" ), 1, az + 1, el + 1, 1, 
              ra + 1, dec + 1 );
    sf1 = astAnnul( sf1 );
    sf2 = astAnnul( sf2 );

/* Get the arc distance between the two positions and see if it is
   greater than 1 arc-sec. */
    sep = slaDsep( ra[ 0 ], dec[ 0 ], ra[ 1 ], dec[ 1 ] );
    *moving = ( sep > AST__DD2R/3600.0 );
  } else {
    *moving = 0;
  }

}
