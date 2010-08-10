/*
*+
*  Name:
*     fts2.h

*  Purpose:
*     Definitions of FTS2 utility methods.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Definitions of FTS2 utility methods.

*  Authors:
*     Coskun (Josh) OBA (UoL)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History :
*     2010-07-09 (OBA):
*        Original.
*     2010-08-09 (TIMJ):
*        Remove fts2_isInBeam.

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

// SMURF includes
#include "fts2_type.h"
#include "libsmf/smf_typ.h"

double  fts2_getMirrorPosition(smfData* data, int* status);
double  fts2_getScanVelocity(smfData* data, int* status);
double* fts2_getPositions(smfData* data, int* status);
double  fts2_getWaveNumberFactor(smfData* data, int* status);
FTSMode fts2_getScanMode(smfData* data, int* status);

int     fts2_getSplineIndex(double* x, int m, double xNew);
void    fts2_naturalCubicSplineInterpolator(double* x, double* y, int m, double* xNew, double* yNew, int n);

