/*
*+
*  Name:
*     fts2_naturalcubicsplineinterpolator.c

*  Purpose:
*     Implementation of natural cubic spline interpolation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Function

*  Invocation:
*     fts2_naturalcubicsplineinterpolator(x, y, m, xNew, yNew, n);

*  Arguments:
*     x = double* (Given)
*        x-array.
*     y = double* (Given)
*        y-array.
*     m = int (Given)
*        Number of (x, y) pairs.
*     xNew = double* (Given)
*        New x-array values where the interpolated values are seeked.
*     y = double* (Returned)
*        Interpolated values of y-array. Must be pre allocated.
*     n = int (Given)
*        Number of (xNew, yNew) pairs.

*  Description:
*     Given m known (x, y) points, computes the n (xNew, yNew) points
*     at the specified xNew locations using natural spline interpolation
*     algorithm.

*  Authors:
*     COBA: Coskun OBA (UoL)

*  History :
*     2010-07-09
*        Original version
*     2010-09-30
*        - Replaced a missed malloc with astMalloc
*        - Removed explicit casting from astMalloc calls
*        - Adapted coding style to SMURF
*        - Fixed the bug due to indexing

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

void fts2_naturalcubicsplineinterpolator(
    double* x,
    double* y,
    int m,
    double* xNew,
    double* yNew,
    int n)
{
  int i       = 0;    /* Counter */
  int index   = 0;    /* Index */
  int j       = 0;    /* Counter */
  int N       = 0;    /* m - 2 */
  int NSpline = 0;    /* Number of splines/intervals */
  double t    = 0.0;  /* Temporary variable */
  double X    = 0.0;  /* X */
  double X2   = 0.0;  /* X * X */
  double X3   = 0.0;  /* X * X * X */
  double* A   = NULL; /* Coefficient of X3 */
  double* B   = NULL; /* Coefficient of X2 */
  double* C   = NULL; /* Coefficient of X */
  double* D   = NULL; /* Constant term */
  double* DX  = NULL; /* x-intervals */
  double* DY  = NULL; /* y-intervals */
  double* M0  = NULL; /* Main diagonal */
  double* MA  = NULL; /* Sub-main diagonal */
  double* MB  = NULL; /* Upper main diagonal */
  double* MD  = NULL; /* Right-Hand side */
  double* Z   = NULL; /* 2nd Derivatives */

  NSpline = m - 1;
  DX = astMalloc(NSpline * sizeof(*DX));
  DY = astMalloc(NSpline * sizeof(*DY));
  for(i = 1; i < m; i++) {
    j = i - 1;
    DX[j] = x[i] - x[j];
    DY[j] = y[i] - y[j];
  }

  /* FORM & SOLVE THE TRI-DIAGONAL MATRIX */
  N = m - 2;
  M0 = astMalloc(N * sizeof(*M0));
  MA = astMalloc(N * sizeof(*MA));
  MB = astMalloc(N * sizeof(*MB));
  MD = astMalloc(N * sizeof(*MD));
  for(i = 0; i < N; i++) {
    M0[i] = 2.0 * (DX[i] + DX[i + 1]);
    MA[i] = MB[i] = DX[i + 1];
    MD[i] = 6.0 * (DY[i + 1] / DX[i + 1] - DY[i] / DX[i]);
  }
  MA[0] = MB[N - 1] = 0;
  MB[0] /= M0[0];
  MD[0] /= M0[0];
  for(i = 1; i < N; i++) {
    t = 1.0 / (M0[i] - MB[i - 1] * MA[i]);
    MB[i] *= t;
    MD[i] = (MD[i] - MD[i - 1] * MA[i]) * t;
  }

  /* COMPUTE 2ND DERVIVATIVES FOR EACH X[I] */
  Z = astMalloc(m * sizeof(*Z));
  Z[N] = MD[N - 1];
  for(i = N - 2; i >= 0; i--) {
    Z[i + 1] = MD[i] - MB[i] * Z[i + 2];
  }
  Z[0] = Z[m - 1] = 0; /* Natural Cubic Spline Condition */

  astFree(MA);
  astFree(M0);
  astFree(MB);
  astFree(MD);

  /* COMPUTE CUBIC SPLINE COEFFICIENTS FOR EACH SPLINE */
  A = astMalloc(NSpline * sizeof(*A));
  B = astMalloc(NSpline * sizeof(*B));
  C = astMalloc(NSpline * sizeof(*C));
  D = astMalloc(NSpline * sizeof(*D));
  for(i = 0; i < NSpline; i++) {
    A[i] = (Z[i + 1] - Z[i]) / (6.0 * DX[i]);
    B[i] = 0.5 * Z[i];
    C[i] = DY[i] / DX[i] - DX[i] * (2.0 * Z[i] + Z[i + 1]) / 6.0;
    D[i] = y[i];
  }
  astFree(Z);
  astFree(DX);
  astFree(DY);

  /* COMPUTE YNEW[I] FOR EACH XNEW[I] */
  for(i = 0; i < n; i++) {
    index = fts2_getsplineindex(x, m, xNew[i]);
    if(index < 0) {
      yNew[i] = 0;
    } else {
      X = xNew[i] - x[index];
      X2 = X * X;
      X3 = X * X2;
      yNew[i] = A[index] * X3 + B[index] * X2 + C[index] * X + D[index];
    }
  }
  astFree(A);
  astFree(B);
  astFree(C);
  astFree(D);
}
