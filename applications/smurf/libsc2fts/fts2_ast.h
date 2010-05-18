/*
*+
*  Name:
*     fts2_ast.h

*  Purpose:
*     

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     

*  Arguments:
*     

*  Description:
*

*  Authors:
*     Coskun OBA (UoL)

*  History :
*     Created: May 17, 2010

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of Lethbridge. All Rights Reserved.

*  License:
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

#ifndef FTS2_AST_H_
#define FTS2_AST_H_

#define FTS2AST_SPD 86400.0  /* Seconds per day */

#include "ast.h"          // Include AST definitions
#include "jcmt/state.h"   // Include JCMTState definition
#include "sc2da/sc2ast.h"

/*
 * Create WCS description using a static cache
 *
 */
void fts2ast_createwcs
(
  int subnum,             /* subarray number, 0-7 (given). If -1 is
                             supplied the cached AST objects will be freed. */
  const JCMTState *state, /* Current telescope state (time, pointing etc.) */
  const double instap[2], /* Offset of subarray in the focal plane */
  const double telpos[3], /* Geodetic W Lon/Lat/Alt of telescope (deg/deg/ign.)*/
  AstFrameSet **fset,     /* constructed frameset (returned) */
  int *status             /* global status (given and returned) */
);

/*
 * Create WCS description using a supplied cache.
 *
 */
sc2astCache *fts2ast_createwcs2
(
  int subnum,             /* subarray number, 0-7 (given). If -1 is
                             supplied the cached AST objects will be freed. */
  const JCMTState *state, /* Current telescope state (time, pointing etc.) */
  double dut1,            /* UT1-UTC (seconds) */
  const double instap[2], /* Offset of subarray in the focal plane */
  const double telpos[3], /* Geodetic W Lon/Lat/Alt of telescope (deg/deg/ign.)*/
  AstFrameSet **fset,     /* constructed frameset (returned) */
  sc2astCache *cache,    /* A pointer to a structure holding cached info */
  int *status             /* global status (given and returned) */
);

#endif /* FTS2_AST_H_ */
