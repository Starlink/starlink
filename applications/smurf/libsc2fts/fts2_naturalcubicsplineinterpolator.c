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

*  Description:
*     Given m known (x, y) points, computes the n (xNew, yNew) points
*     at the specified xNew locations using natural spline interpolation 
*     algorithm.

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

void fts2_naturalcubicsplineinterpolator(
    double* x, 
    double* y, 
    int m, 
    double* xNew, 
    double* yNew, 
    int n)
{
  int NSpline = m - 1; // Number of splines

  double* DX = (double*) astMalloc(NSpline * sizeof(double));
  double* DY = (double*) astMalloc(NSpline * sizeof(double));

  int j;
  for(int i = 1; i < m; i++)
  {
	j = i - 1;
    DX[j] = x[i] - x[j];
    DY[j] = y[i] - y[j];
  }

  // FORM & SOLVE THE TRI-DIAGONAL MATRIX
  int N = m - 2;
  double* M0 = (double*) astMalloc(N * sizeof(double)); // Main diagonal
  double* MA = (double*) astMalloc(N * sizeof(double)); // Sub-main diagonal
  double* MB = (double*) astMalloc(N * sizeof(double)); // Upper main diagonal
  double* MD = (double*) astMalloc(N * sizeof(double)); // Right-Hand side
  for(int i = 0; i < N; i++)
  {
    M0[i] = 2.0 * (DX[i] + DX[i + 1]);
    MA[i] = MB[i] = DX[i + 1];
    MD[i] = 6.0 * (DY[i + 1] / DX[i + 1] - DY[i] / DX[i]);
  }
  MA[0] = MB[N - 1] = 0;

  MB[0] /= M0[0];
  MD[0] /= M0[0];

  for(int i = 1; i < N; i++)
  {
    double t = 1.0 / (M0[i] - MB[i - 1] * MA[i]);
    MB[i] *= t;
    MD[i] = (MD[i] - MD[i - 1] * MA[i]) * t;
  }

  // COMPUTE 2ND DERVIVATIVES FOR EACH X[I]
  double* Z = (double*) malloc(m * sizeof(double));
  Z[N] = MD[N - 1];
  for(int i = N - 2; i >= 0; i--)
  {
    Z[i + 1] = MD[i] - MB[i] * Z[i + 2];
  }
  Z[0] = Z[m - 1] = 0; // Natural Cubic Spline Condition

  astFree(MA); 
  astFree(M0); 
  astFree(MB); 
  astFree(MD);

  // COMPUTE CUBIC SPLINE COEFFICIENTS FOR EACH SPLINE
  double* A = (double*) astMalloc(NSpline * sizeof(double));
  double* B = (double*) astMalloc(NSpline * sizeof(double));
  double* C = (double*) astMalloc(NSpline * sizeof(double));
  double* D = (double*) astMalloc(NSpline * sizeof(double));
  for(int i = 0; i < NSpline; i++)
  {
    A[i] = (Z[i + 1] - Z[i]) / (6.0 * DX[i]);
    B[i] = 0.5 * Z[i];
    C[i] = DY[i] / DX[i] - DX[i] * (2.0 * Z[i] + Z[i + 1]) / 6.0;
    D[i] = y[i];
  }
  astFree(Z); 
  astFree(DX); 
  astFree(DY);

  // COMPUTE YNEW[I] FOR EACH XNEW[I]
  double X, X2, X3;
  for(int i = 0; i < n; i++)
  {
    int index = fts2_getsplineindex(x, m, xNew[i]);
    if(index < 0)
    {
      yNew[i] = 0;
    }
    else
    {
      X = xNew[i] - x[index];
      X2 = X * X;
      X3 = X * X2;
      yNew[i] = A[i] * X3 + B[i] * X2 + C[i] * X + D[i];
    }
  }
  astFree(A); 
  astFree(B); 
  astFree(C); 
  astFree(D);
}
