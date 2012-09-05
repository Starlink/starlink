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
*        - Remove fts2_deglitch
*        - Remove fts2_phasecorrection
*     2011-10-18 (COBA):
*        - Defined SMF__FTS2_(LOW/MED/HIGH)RES_SSI
*        - Add fts2_apodization

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

// STARLINK INCLUDES
#include "jcmt/state.h"
#include "sc2da/sc2ast.h"

// SMURF INCLUDES
#include "fts2_type.h"
#include "libsmf/smf_typ.h"

// SCALE AT ARRAY IN RADIANS
#ifndef MM2RAD
#define MM2RAD (0.92 * 2.4945e-5)
#endif

// PIXEL INTERVAL IN MM
#ifndef PIX2MM
#define PIX2MM 1.135
#endif

// LOW RESOLUTION SPECTRAL SAMPLING INTERVAL
#ifndef SMF__FTS2_LOWRES_SSI
#define SMF__FTS2_LOWRES_SSI 0.25
#endif

// MEDIUM RESOLUTION SPECTRAL SAMPLING INTERVAL
#ifndef SMF__FTS2_MEDRES_SSI
#define SMF__FTS2_MEDRES_SSI 0.1
#endif

// HIGH RESOLUTION SPECTRAL SAMPLING INTERVAL
#ifndef SMF__FTS2_HIGHRES_SSI
#define SMF__FTS2_HIGHRES_SSI 0.02
#endif

// DEGLITCH THRESHOLD
#ifndef SMF__DEGLITCH_THRESHOLD
#define SMF__DEGLITCH_THRESHOLD 1.0e-9
#endif

void fts2_validatemirrorpositions(
    double* positions,
    int count,
    int* ni,
    int* nf,
    int* status);

void fts2_arraycopy(
    double* source,
    int sourceSize,
    double* destination,
    int destinationSize,
    int sourceStart,
    int destinationStart,
    int count,
    int* status);

void fts2_arrayquicksort(
    double* array,
    int size,
    int start,
    int end,
    int ascending,
    int* status);

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

void fts2_getmirrorpositions(
    smfData* data,
    double* positions,
    int* size,
    int* status);

void fts2_apodization(
    double* signal,
    int size,
    double a,
    double b,
    double* window,
    int apodization,
    int* status);
