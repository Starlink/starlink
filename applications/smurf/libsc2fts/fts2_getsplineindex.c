/*
*+
*  Name:
*     fts2_getsplineindex.c

*  Purpose:
*     Finds the spline index of the specified xNew value in the given array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Function

*  Invocation:
*     fts2_getsplineindex(x, m, xNew);

*  Arguments:
*     x = double* (Given)
*        x-array.
*     m = int (Given)
*        Number of x points.
*     xNew = double (Given)
*        The x-value of which interval index is seeked.

*  Description:
*     Given m number of x-data points returns the spline index of the specified
*     xNew value using bisection method.

*  Authors:
*     COBA: Coskun OBA (UoL)

*  History :
*     2010-07-09
*        Original version
*     2010-09-30
*        - Adapted coding style to SMURF

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* STANDARD INCLUDES */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* STARLINK INCLUDES */
#include "ast.h"

/* SMURF INCLUDES */
#include "fts2.h"

int fts2_getsplineindex(
    double* x,
    int m,
    double xNew)
{
  int index = 0;  /* Current index */
  int ni    = 0;  /* Initial index */
  int nf    = 0;  /* Final index */

  if( xNew < x[0] || xNew > x[m -1]) {
    return -1;
  }

  ni = 0;
  nf = m - 1;
  while(nf - ni > 1) {
    index = (ni + nf) >> 1;
    if(xNew > x[index]) {
      ni = index;
    } else {
      nf = index;
    }
  }
  return ni;
}
