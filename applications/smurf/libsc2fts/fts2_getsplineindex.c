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

*  Description:
*     Given m number of x-data points returns the spline index of the specified 
*     xNew value using bisection method.

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

/* STANDARD INCLUDES */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* STARLINK INCLUDES */
#include "ast.h"

/* SMURF INCLUDES */
#include "fts2.h"

int fts2_getsplineindex(
    double* x, int m, 
    double xNew)
{
  int index = 0;
  int end   = 0;
  int start = 0;
  
  if( xNew < x[0] || xNew > x[m -1])
  {
    return -1;
  }

  start = 0;
  end = m - 1;
  while((end - start) > 1)
  {
    index = (start + end) >> 1;
    if(xNew < x[index])
    {
      end = index;
    }
    else
    {
      start = index;
    }
  }
  return start;
}
