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

#ifndef PI
#define PI 3.14159265358979323
#endif

#ifndef DEGLITCH_THRESHOLD
#define DEGLITCH_THRESHOLD 1.0e-30
#endif

#ifndef DBL_MIN
#define DBL_MIN -1.0e308
#endif

#ifndef DBL_MAX
#define DBL_MAX -DBL_MIN
#endif

int fts2_getsplineindex(
    double* x, 
    int m, 
    double xNew);
    
void fts2_naturalcubicsplineinterpolator(
    double* x, 
    double* y, 
    int m, 
    double* xNew, 
    double* yNew, 
    int n);
    
double* fts2_polyfitcoeffs(
    int n,          /* Polynomial degree */
    double* x,      /* Discrete x-values */
    double* y,      /* Discrete y-values */
    double* w,      /* The weights */
    int size,       /* Size of x, y and w */
    double* sigma); /* Standard deviation (out) */

double fts2_polyfit( 
    int n,          /* Polynomial degree */
    double* coeffs, /* Coefficients, pre-computed with fts2_polyfitcoeffs() */
    double x);      /* The value of which the fit is seeked */

double* fts2_polyfitarray(
    int n,          /* Polynomial degree */
    double* coeffs, /* Coefficients, pre-computed with fts2_polyfitcoeffs() */
    double* x,      /* The array of which the fit is seeked */
    int size);      /* Size of array */

void fts2_deglitch(
    double* interferogram,              /* The interferogram */
    int size,                           /* Sample size of the interferogram */
    int coreClusterSize,                /* Core cluster size */
    int tailClusterSize,                /* Tail cluster size */
    double tailCutoffStdDevPercent,     /* Tail cutoff standard deviation (as percentage) */
    double tailCutoffStdDevMultiplier,  /* Tail cutoff standard deviation multiplier */
    int zpdIndex,                       /* Index of ZPD */
    int dsHalfLength,                   /* Size of the double sided interferogram */
    smf_deglitchmode mode,					    /* Deglitch mode */
    double threshold);            	    /* Deglitch threshold */

int fts2_phasecorrcetion(
    double* interferogram,        /* The interferogram */
    int size,                     /* Sample size of the interferogram */
    int zpdIndex,                 /* Index of ZPD */
    int* dsHalfLength,            /* Double-Sided interferogram half-length */
    int* ssHalfLength,            /* Single-Sided interferogram half-length */
    int polynomialDegree,     	  /* Degree of the fitting polynomial */
    int phaseFunctionHalfLength,  /* Phase correction function half-length  */
    double wnLBoundPercent,   	  /* The lower bound of the wavenumber range (as percentage) */
    double wnUBoundPercent,   	  /* The upper bound of the wavenumber range (as percentage) */
    double weightLimit,       	  /* Weight limit */
    double* sigma); 			        /* Holds the value of the Standard Deviation after performing the Phase Correction */
