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
*     void smf_calc_skyframe( const AstFrame *skyin, const char * system,
*                             const smfHead* hdr, int alignsys,
*                             AstSkyFrame ** skyframe, double skyref[2],
*                             int * moving, int * status )

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
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-11-14 (DSB):
*        Original smf_cubegrid implementation.
*     2008-06-04 (TIMJ):
*        Initial version factored out of smf_cubegrid
*     2009-08-25 (DSB):
*        Added args "igrp" and "size" to allow returned "*moving" value to
*        be based on the total distance moved by the tracking centre over
*        all supplied observations rather than just one observation.
*     2009-09-09 (EC):
*        Add extra status checks
*     2009-09-11 (DSB):
*        Cater for SCUBA-2 file names in addition to ACSIS.
*     2009-09-14 (DSB):
*        Removed "igrp" and "size". Returned "moving" value is now determined
*        just from the supplied output coordinate system.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "star/grp.h"
#include "star/ndg.h"

/* SMURF includes */
#include "sc2da/sc2ast.h"
#include "smurf_par.h"
#include "libsmf/smf.h"

void smf_calc_skyframe( const AstFrame *skyin, const char * system,
                        const smfHead* hdr, int alignsys,
                        AstSkyFrame ** skyframe, double skyref[2],
                        int * moving, int * status ) {

/* Local Variables: */
   AstFrame    *sf1 = NULL;    /* Spatial Frame representing AZEL system */
   AstFrameSet *fs = NULL;     /* FrameSet from AZEL to requested system */
   AstMapping  *azel2usesys = NULL; /* Mapping from AZEL to requested system */
   AstMapping  *tmap = NULL;   /* Mapping from AZEL to requested system */
   const char  *usesys = NULL; /* AST system for output cube */

/* Check inherited status */
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

  fs = astConvert( sf1, *skyframe, "" );
  tmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
  azel2usesys = astSimplify( tmap );

  astTran2( azel2usesys, 1, &(hdr->state->tcs_az_bc1),
            &(hdr->state->tcs_az_bc2), 1, skyref, skyref + 1 );

  azel2usesys = astAnnul( azel2usesys );
  tmap = astAnnul( tmap );
  fs = astAnnul( fs );

/* Normalise these values. */
  astNorm( *skyframe, skyref );

/* Determine if the telescope is tracking a moving target such as a planet
   or asteroid. We assume this is the case if the output cube is in AZEL
   or GAPPT. */
  *moving = ( !strcmp( usesys, "AZEL" ) || !strcmp( usesys, "GAPPT" ) );
}
