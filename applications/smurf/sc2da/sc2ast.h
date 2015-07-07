/*
*+
*  Name:
*     sc2ast.h

*  Purpose:
*     Prototypes for the sc2ast library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "sc2ast.h"

*  Description:
*     Prototypes used by the sc2ast functions.

*  Authors:
*     B.D.Kelly (bdk@roe.ac.uk)
*     Tim Jenness (timj@jach.hawaii.edu)
*     D.S. Berry (dsb@ast.man.ac.uk)
*     E.Chapin (echapin@phas.ubc.ca)
*     {enter_new_authors_here}

*  History:
*     2006-08-15 (EC):
*        Placed comments at top of header, added SC2AST_BOLCOL/BOLROW
*     2006-08-17 (EC):
*        Removed SC2AST_BOLCOL/BOLROW,sc2ast_get_gridcoords->smf_get_gridcoords
*     2006-09-07 (EC)
*        Modified interface for sc2ast_createwcs
*     2007-04-30 (BDK) this file now contains more than just the
*                prototypes, so include sc2ast_pro.h. This allows the
*                prototypes to be created in the data acquisition
*                system, but this is hidden from smurf.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

#ifndef HEADGEN___src_sc2ast_sc2ast_h
#define HEADGEN___src_sc2ast_sc2ast_h

#define SC2AST_SPD 86400.0  /* Seconds per day */

/* Minimum significant separation between sky positions, in arc-seconds.
   This is used to determine when the mapping between two SkyFrames can
   be considered to be a unit transformation for the purposes of
   simplification. If the Mapping between two SkyFrames produces shifts
   smaller than this amount, then they are considered to be identical. */
#define SC2AST__SKYTOL 0.05

/* Following include is for JCMTState definition */
#include "jcmt/state.h"

/* Include AST definitions */
#include "ast.h"

/* sc2ast enums */
#include "sc2ast_typ.h"

/* Define a structure used to hold information cached by sc2ast_createwcs. */
typedef struct sc2astCache {

/* A cache containing, for each sub-array, a FrameSet and a Mapping. The
   FrameSet will contai a single Frame representing GRID coords in the
   sub-array. The result of applying the Mapping to this Frame will be
   Cartesian (i.e. in the tangent plane) Nasmyth coords in rads. The
   AST pointers in this cache are exempted from AST context handling, and
   so need to be released explicitly using astAnnul. This is done by
   calling this function with the sub-frame number set to SC2AST__NULLSUB. */
   AstMapping *map[ SC2AST__NSUB ];
   AstFrameSet *frameset[ SC2AST__NSUB ];

/* Cache the SkyFrame used to represent final spherical (Az,El) coords */
   AstSkyFrame *skyframe;

/* Cache used to hold Mappings needed in the tangent plane to celestial
   longitude,latitude Mapping. */
   AstMapping *azel[ 2 ];

/* Cache used to hold the instap values which were hatrd-wired into the
   cached Mapping for each subarray. */
   double instap_x[ SC2AST__NSUB ];
   double instap_y[ SC2AST__NSUB ];

} sc2astCache;

/* The function prototypes */
#include "sc2ast_pro.h"

#endif
