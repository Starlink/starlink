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
*     COBA: Coskun OBA (UoL)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History :
*     2010-07-09 (OBA):
*        Original.
*     2010-08-09 (TIMJ):
*        Remove fts2_isInBeam.
*     2010-08-09 (TIMJ):
*        Remove fts2_isInBeam.
*     2010-09-20 (COBA):
*        - Remove fts2_polyfitarray
*        - Remove fts2_polyfit
*        - Remove fts2_polyfitcoeffs
*        - Remove PI
*        - Remove PIBY2
*        - Remove FTS2AST_SPD
*     2011-07-18 (COBA):
*        - Add fts2_getmirrorpositions

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

#ifndef MM2RAD /* scale at array in radians */
#define MM2RAD (0.92 * 2.4945e-5)
#endif

#ifndef PIX2MM /* pixel interval in mm */
#define PIX2MM 1.135
#endif

#ifndef SMF__DEGLITCH_THRESHOLD
#define SMF__DEGLITCH_THRESHOLD 1.0e-9
#endif

#ifndef SMF__FLAT_THRESHOLD
#define SMF__FLAT_THRESHOLD 1.0e-10
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

void fts2ast_createwcs(
    sc2ast_subarray_t subnum,
    const JCMTState *state,
    const double instap[2],
    const double telpos[3],
    AstFrameSet **fset,
    int *status);

sc2astCache* fts2ast_createwcs2(
    sc2ast_subarray_t subnum,
    const JCMTState *state,
    double dut1,
    const double instap[2],
    const double telpos[3],
    AstFrameSet **fset,
    sc2astCache *cache,
    int *status);

void fts2_deglitch(
    double* interferogram,
    int size,
    int coreClusterSize,
    int tailClusterSize,
    double tailCutoffStdDevPercent,
    double tailCutoffStdDevMultiplier,
    int zpdIndex,
    int dsHalfLength,
    smf_deglitchmode mode,
    double threshold);

void fts2_phasecorrection(
    double* interferogram,
    int size,
    int zpdIndex,
    int dsHalfLength,
    int ssHalfLength,
    int polynomialDegree,
    int phaseFunctionHalfLength,
    double wnLBoundPercent,
    double wnUBoundPercent,
    double weightLimit,
    double* sigma,
    double* coefficients,
    double* phase,
    double* phaseFunction,
    double* newInterferogram,
    int* status);

void fts2_getmirrorpositions(
    smfData* data,
    double* positions,
    int* size,
    int* status);
