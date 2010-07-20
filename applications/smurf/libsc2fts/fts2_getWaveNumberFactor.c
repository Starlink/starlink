/*
*+
*  Name:
*     fts2_getWaveNumberFactor.c

*  Purpose:
*     Gets the wave number factor.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     FUNCTION

*  Invocation:
*     fts2_getWaveNumberFactor(smfData* data, int* status)

*  Description:
*     Gets the wave number factor.

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

#include <string.h>
#include <stdio.h>
#include <ctype.h>

// SMURF includes
#include "fts2.h"
#include "libsmf/smf.h"

/* Gets the wave number factor. */
double fts2_getWaveNumberFactor(smfData* data, int* status)
{
  double wnFactor = 0.0;
  HDSLoc* hdsLoc = smf_get_xloc(data, "FTS2DR", "EXT", "READ", 0, 0, status);
  HDSLoc* hdsLocFactor = NULL;
  datFind(hdsLoc, "FTS_WN_FACTOR", &hdsLocFactor, status);
  datGet0D(hdsLocFactor, &wnFactor, status);
  // FREE RESOURCES
  datAnnul(&hdsLocFactor, status);
  datAnnul(&hdsLoc, status);
  return wnFactor;
}
