/*
*+
*  Name:
*     fts2_interpolation.h

*  Purpose:
*     Definitions of interpolation methods utilized by the FTS2 data reduction.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Definitions of interpolation methods utilized by the FTS2 data reduction.

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

#ifndef FTS2_INTERPOLATION_H_
#define FTS2_INTERPOLATION_H_

/*
 * Given m known (x, y) points, computes the n (xNew, yNew) points
 * at the specified xNew locations using natural spline interpolation algorithm.
 */
void fts2_naturalCubicSplineInterpolator(double* x, double* y, int m, double* xNew, double* yNew, int n);

/*
 * Given m x-data points, returns the index of the interval
 * containing the specified xNew location using Bisection algorithm.
 * Returns -1 if xTarget is NOT in the interval.
 */
int fts2_getSplineIndex(double* x, int m, double xNew);
#endif
