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
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#ifndef HEADGEN___src_sc2ast_sc2ast_h
#define HEADGEN___src_sc2ast_sc2ast_h 

#define SC2AST_SPD 86400.0  /* Seconds per day */
 
/* Following include is for JCMTState definition */
#include "jcmt/state.h"

/*+ sc2ast_createwcs - create WCS description */

void sc2ast_createwcs
(
int subnum,             /* subarray number, 0-7 (given). If -1 is
                           supplied the cached AST objects will be freed. */
const JCMTState *state, /* Current telescope state (time, pointing etc.) */
const double fplane_off[2], /* Offset of subarray in the focal plane */ 
const double telpos[3], /* Geodetic Lon/Lat/Alt of telescope (deg/deg/ign.) */
AstFrameSet **fset,     /* constructed frameset (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2ast_getdomain - select a domain within a frameset */

void sc2ast_getdomain
(
const char *name,               /* AST domain name (given) */
AstFrameSet *fset,        /* modified frameset (given and returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2ast_makefitschan - create a set of FITS headers in a FitsChan */

void sc2ast_makefitschan
(
double crpix1,            /* index of reference point (given) */
double crpix2,            /* index of reference point (given) */
double cd1_1,             /* data increment (given) */
double cd2_2,             /* data increment (given) */
double crval1,            /* reference coordinate (given) */
double crval2,            /* reference coordinate (given) */
const char *ctype1,             /* coordinate mapping type (given) */
const char *ctype2,             /* coordinate mapping type (given) */
AstFitsChan *fitschan,   /* FitsChan to be filled (given and returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2ast_moveframe - move the base frame within a frameset */

void sc2ast_moveframe
(
double x,                 /* X coordinate offset in pixels (given) */
double y,                 /* Y coordinate offset in pixels (given) */
AstFrameSet *fset,        /* modified frameset (given and returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2ast_name2num - convert subarray name to id number */

void sc2ast_name2num
(
const char *name,             /* subarray name s8a-d, s4a-d (given) */
int *subnum,            /* subarray number, 0-7 (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2ast_polframeset - create a frameset for polarimetry */

void sc2ast_polframeset
(
AstFrameSet *frameset,  /* 2-D frameset (given) */
AstFrameSet **fset,     /* constructed 3-D frameset (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2ast_maketanmap - create a Mapping representing a tangent plane 
                        projection */

AstMapping *sc2ast_maketanmap
(
double lon,              /* Celestial longitude at ref point (rads) */
double lat,              /* Celestial latitude at ref point (rads) */
AstMapping *cache[ 2 ],  /* Cached Mappings (supply as NULL on 1st call) */
double rot,              /* Rotation needed to align input Y axis with North */
int *status              /* global status (given and returned) */
);

#endif
