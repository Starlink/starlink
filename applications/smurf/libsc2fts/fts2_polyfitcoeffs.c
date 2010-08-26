/*
*+
*  Name:
*     fts2_polyfitcoeffs.c

*  Purpose:
*     Computes the polynomial fit coefficients using weighted lest squares.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Computes the polynomial fit coefficients using weighted lest squares.

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

/* GSL INCLUDES */
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_linalg.h>

// STARLINK INCLUDES
#include "ast.h"

// SMURF INCLUDES
#include "fts2.h"

double* fts2_polyfitcoeffs(
    int n,          /* Polynomial degree */
    double* x,      /* Discrete x-values */
    double* y,      /* Discrete y-values */
    double* w,      /* The weights */
    int size,       /* Size of x, y and w */
    double* sigma)  /* Standard deviation (out) */
{
	int i;
	int j;
	int cSize = 0;	
	double sum = 0.0;
	double error = 0.0;
	double wSum = 0.0;
	double* z = NULL;
	double* c = NULL;
	double* coeffs = NULL;	
	double* yFit = NULL;
	gsl_matrix* mat = NULL;
	gsl_vector* vec = NULL;
	gsl_vector* tau = NULL;
	gsl_vector* veccoeffs = NULL;

	/* GET MATRIX A */
	z = (double*) astMalloc(size * sizeof(double));
	for(i = 0; i < size; i++) 
	{ 
	  z[i] = w[i]; 
	}

	cSize = 2 * n + 1;
	c = (double*) astMalloc(cSize * sizeof(double));
	c[0] = 0.0;
	for(i = 0; i < size; i++) 
	{ 
	  c[0] += z[i]; 
	}
	
	for(i = 1; i < cSize; i++)
	{
		for(j = 0; j < size; j++) 
		{ 
		  z[j] *= x[j]; 
		}

		c[i] = 0.0;
		for(j = 0; j < size; j++) 
		{ 
		  c[i] += z[j]; 
		}
	}

	n++;
	mat = gsl_matrix_alloc(n, n);
	for(i = 0; i < n; i++)
	{
		for(j = 0; j < n; j++)
		{
			gsl_matrix_set(mat, i, j, c[i + j]);
		}
	}

	/* GET VECTOR B */
	vec = gsl_vector_alloc(n);
	for(i = 0; i < size; i++) 
	{ 
	  z[i] = w[i] * y[i]; 
	}

	sum = 0.0;
	for(i = 0; i < size; i++) 
	{ 
	  sum += z[i]; 
	}
	gsl_vector_set(vec, 0, sum);
	for(i = 1; i < n; i++)
	{
		for(j = 0; j < size; j++) 
		{ 
		  z[j] *= x[j]; 
		}

		sum = 0.0;
		for(j = 0; j < size; j++) 
		{ 
		  sum += z[j]; 
		}
		gsl_vector_set(vec, i, sum);
	}

	/* SOLVE Ax = b -- (Using QR Decomposition) */
	tau = gsl_vector_alloc(n);
	gsl_linalg_QR_decomp(mat, tau);
	veccoeffs = gsl_vector_alloc(n);
	gsl_linalg_QR_solve(mat, tau, vec, veccoeffs);

	coeffs = (double*) astMalloc(n * sizeof(double));
	for(i = 0; i < n; i++)
	{
		coeffs[i] = gsl_vector_get(veccoeffs, i);
	}

	/* COMPUTE STANDARD DEVIATION */
	error = 0.0;
	wSum = 0.0;
	yFit = fts2_polyfitarray(n - 1, coeffs, x, size);
	for(i = 0; i < size; i++)
	{
		error = (y[i] - yFit[i]);
		error = error * error;
		error *= w[i];
		wSum += w[i];
	}
	*sigma = sqrt(error / wSum);

	gsl_matrix_free(mat);
	gsl_vector_free(tau);
	gsl_vector_free(vec);
	gsl_vector_free(veccoeffs);

	astFree(c);
	astFree(z);

	return coeffs;
}
