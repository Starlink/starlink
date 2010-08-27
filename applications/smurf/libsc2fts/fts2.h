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

/* STARLINK INCLUDES */
#include "jcmt/state.h" 
#include "sc2da/sc2ast.h"

/* SMURF INCLUDES */
#include "fts2_type.h"
#include "libsmf/smf_typ.h"

#ifndef PI
#define PI 3.14159265358979323
#endif

#ifndef PIBY2
#define PIBY2 (0.5 * PI)
#endif 

#ifndef DBL_MIN
#define DBL_MIN -1.0e308
#endif

#ifndef DBL_MAX
#define DBL_MAX -DBL_MIN
#endif

#ifndef MM2RAD /* scale at array in radians */
#define MM2RAD (0.92*2.4945e-5) 
#endif

#ifndef PIX2MM /* pixel interval in mm */
#define PIX2MM 1.135                   
#endif

#ifndef DEGLITCH_THRESHOLD
#define DEGLITCH_THRESHOLD 1.0e-30
#endif

#ifndef SMF__FLAT_THRESHOLD
#define SMF__FLAT_THRESHOLD 1.0e-10
#endif

#ifndef FTS2AST_SPD
#define FTS2AST_SPD 86400.0  /* Seconds per day */
#endif

void fts2_arraycopy( 
    double* source,
    int sourceSize,
    double* destination,
    int destinationSize,
    int sourceStart,
    int destinationStart,
    int count);

void fts2_arrayquicksort(
    double* array, 
    int size, 
    int start, 
    int end, 
    int ascending);

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
    

void fts2ast_createwcs
(
    sc2ast_subarray_t subnum, /* subarray number, 0-7 (given). If SC2AST__NULLSUB is
                              *  supplied the cached AST objects will be freed. */
    const JCMTState *state,   /* Current telescope state (time, pointing etc.) */
    const double instap[2],   /* Offset of subarray in the focal plane */
    const double telpos[3],   /* Geodetic W Lon/Lat/Alt of telescope (deg/deg/ign.)*/
    AstFrameSet **fset,       /* constructed frameset (returned) */
    int *status               /* global status (given and returned) */
);

sc2astCache* fts2ast_createwcs2
(
    sc2ast_subarray_t subnum, /* subarray number, 0-7 (given). 
                              *   If SC2AST__NULLSUB is supplied the cached 
                              *   AST objects will be freed. */
    const JCMTState *state,   /* Current telescope state (time, pointing etc.) */
    double dut1,              /* UT1-UTC (seconds) */
    const double instap[2],   /* Offset of subarray in the focal plane */
    const double telpos[3],   /* Geodetic W Lon/Lat/Alt of telescope (deg/deg/ign.)*/
    AstFrameSet **fset,       /* constructed frameset (returned) */
    sc2astCache *cache,       /* A pointer to a structure holding cached info */
    int *status               /* global status (given and returned) */
);

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
