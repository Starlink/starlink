/*
*+
*  Name:
*     fts2_polyfit.c

*  Purpose:
*     Computes the polynomial fit for the given value.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Computes the polynomial fit for the given value.

*  Authors:
*     Coskun (Josh) OBA (UoL)

*  History :
*     2010-08-26 (OBA):
*        Original.

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

double fts2_polyfit( 
    int n,          /* Polynomial degree */
    double* coeffs, /* Coefficients, pre-computed with fts2_polyfitcoeffs() */
    double x)       /* The value of which the fit is seeked */
{
  int i = 0;
  double y = 0;
  double xn = 1.0;

  n++;
  for(i = 0; i < n; i++)
  {
    y += coeffs[i] * xn;
    xn *= x;
  }
	return y;
}
