/*
*+
*  Name:
*     fts2.h

*  Purpose:
*     Definitions of general purpose FTS2 methods and types.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Definitions of general purpose FTS2 methods and types.

*  Authors:
*     Coskun (Josh) OBA (UoL)

*  History :
*     Created: July 9, 2010

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

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

#include "libsmf/smf.h"

/* Boolean Type Support */
typedef enum boolean
{
  FALSE = 0,
  TRUE = 1
} bool;

/* FTS-2 Scan Modes */
typedef enum FTS2ScanMode
{
  UNKNOWN = -1, // Undefined or Unknown
  FSCAN = 0,    // Fast Scan
  STEPINT = 1   // Step-and-Integrate
} FTS2Mode;

/* SCUBA-2 sub-arrays. */
typedef enum Scuba2SubArrayID
{
  s8a = 0, // 850 A
  s8b = 1, // 850 B
  s8c = 2, // 850 C
  s8d = 3, // 850 D
  s4a = 4, // 450 A
  s4b = 5, // 450 B
  s4c = 6, // 450 C
  s4d = 7  // 450 D
} SubArrayID;

/* Determines whether the FTS-2 is in the beam. */
bool fts2_isInBeam(smfData* data, int* status);

/* Gets the FTS-2 scanning mode. */
FTS2Mode fts2_getScanMode(smfData* data, int* status);

/* Gets the mirror position when in STEPINT mode, [mm]. */
double fts2_getMirrorPosition(smfData* data, int* status);

/* Gets the mirror scan rate in FSCAN mode [mm/s]. */
double fts2_getScanVelocity(smfData* data, int* status);

/*
 * Gets the FTS-2 positions.
 * Caller is responsible for freeing the returned positions.
 * Example: smf_free(positions, status);
 */
double* fts2_getPositions(smfData* data, int* status);

/* Gets the wave number factor. */
double fts2_getWaveNumberFactor(smfData* data, int* status);
