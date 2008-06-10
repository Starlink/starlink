/*
*+
*  Name:
*     sc2fts_common.c

*  Purpose:
*     common functions used by sc2fts_entry

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     see sc2fts_common.h


*  Description:
*
*

*  Authors:
*     B.Zhang (UoL)

*  History :
*     2008-03-18 (BZ):
*        Create a implementation for FTS-2

*  Copyright:
*     Copyright (C) University of Lethbridge. All Rights Reserved.

*  Licence:
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

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/**
 * Simplified CubicSpline Interpolation. Here, the original values of x are from
 * 0 to n-1, and the new values of x are from 0 * scalefactor to (n-1) * scalefactor.
 * @param y_old the values of y-axis corresponding to x_old.
 * @return the values of the interpolate y values corresponding to x_new.
 */

void csi_simplified(float *y_old, int _n, double *x_new, int _m, float *y_new)
{
   int i;
   double *_coeff = (double*)malloc(_n*sizeof(double));
   double *_p = (double*)malloc(_n*sizeof(double));
   double *_coeff_x = (double*)malloc(_n*sizeof(double));

   _coeff_x[0] = 0.0;
   for(i = 1; i < _n-1; i++)
   {
      _p[i] = 1.0/(0.5 * _coeff_x[i-1] + 2.0);
      _coeff_x[i] = -0.5*_p[i];
   }

   /* Solve for coefficients by tridiagonal algorithm. */

   double *z = (double*)malloc(_n*sizeof(double));

   double t;
   z[0] = 0.0;
   for(i = 1; i < _n-1; i++)
   {
       t = y_old[i+1] + y_old[i-1] - 2.0*y_old[i];
       z[i] = (3.0 * t  - 0.5 * z[i-1]) * _p[i];
   }
   free(_p);

   _coeff[_n - 1] = 0.0;

   for (i = _n-2; i >= 0; i--)
   {
      _coeff[i] = _coeff_x[i] * _coeff[i+1] + z[i];
   }

   free(z);
   free(_coeff_x);

   for(i=0; i<_m; i++) 
   {
      int k = (int)(x_new[i]) + 1;
      if(k>_n) 
      { 
        y_new[i] = 0.0; 
      }
      else
      {
        double y0 = y_old[k-1], y1 = y_old[k];
        double coeff0 = _coeff[k-1], coeff1 = _coeff[k];
        double a = (k - x_new[i]);
        double b = (x_new[i] - k + 1);
        y_new[i] =  a * y0 + b * y1
           + ((a*a*a - a) * coeff0 + (b*b*b - b) * coeff1) / 6.0;
      }
   }

   free(_coeff);
}
