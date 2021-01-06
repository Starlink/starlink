#if !defined( ATL_INCLUDED )  /* Include this file only once */
#define ATL_INCLUDED
/*
*  Name:
*     atl.h

*  Purpose:
*     Define the C interface to the ATL library.

*  Description:
*     This module defines the C interface to the functions of the ATL
*     library. The file atl.c contains C wrappers for the Fortran
*     ATL routines.

*  Authors:
*     DSB: David S. Berry (JAC)
*     TIMJ: Tim Jenness (JAC)

*  History:
*     26-MAY-2006 (DSB):
*        Original version.
*     7-FEB-2007 (DSB):
*        Added atlMgfts.
*     20-MAR-2007 (TIMJ):
*        Added atlPtfts, atlPtfti
*     16-APR-2009 (DSB):
*        Added atlGetParam.
*     6-JUL-2009 (TIMJ):
*        Add atlRmblft
*     2010-01-12 (TIMJ):
*        Add atlMapCopy
*     2010-05-05 (TIMJ):
*        Remove atlMapCopy
*     2010-05-07 (TIMJ):
*        Add atlPtftd

*  Copyright:
*     Copyright (C) 2009-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

*/

#include "f77.h"
#include "star/hds.h"
#include "ast.h"

/* Public Constants */
/* ---------------- */

/* Maximum number of axes to consider. This should be equal to
   NDF__MXDIM. */
enum { ATL__MXDIM  = 7 };


/* Public function prototypes */
/* -------------------------- */
AstLutMap *atlTablelutMap( AstTable *, const char *, int * );
AstMapping *atlFindMap( AstMapping *, const char *i, AstMapping **, AstMapping **, int * );
AstObject *atlReadFile( const char *, const char *, int * );
AstRegion *atlMatchRegion( AstRegion *, AstFrame *, int * );
AstTable *atlReadTable( const char *, int * );
int atlMapGet1C( AstKeyMap *, const char *, int, int, int *, char *, int * );
void atlAddWcsAxis( AstFrameSet *, AstMapping *, AstFrame *, int *, int *, int * );
void atlAddWcsAxis8( AstFrameSet *, AstMapping *, AstFrame *, int64_t *, int64_t *, int * );
void atlAxtrm( AstFrameSet *, int *, int *, int *, double *, int * );
void atlCreat( const char *, AstObject *, int * );
void atlDumpFits( const char *, AstFitsChan *, int * );
void atlFindSky( AstFrame *, AstSkyFrame **, int *, int *, int * );
AstFrameSet *atlFrameSetSplit( AstFrameSet *, const char *, int **, int **, int * );
void atlGetParam( const char *, AstKeyMap *, int * );
void atlGetPixelParams( AstFrameSet *fset, int *dims, int degs, double *crpix, double *crval, double *cdelt, double *crota, int *status );
void atlHd2ky( HDSLoc *, AstKeyMap *, int * );
void atlKy2hd( AstKeyMap *, HDSLoc *, int * );
void atlKychk( AstKeyMap *, const char *, const char *, int * );
void atlMapPut1C( AstKeyMap *, const char *, const char *, int, int, const char *, int * );
void atlMgfts( int, AstFitsChan *, AstFitsChan *, AstFitsChan **, int * );
void atlMklut( int, int, int, int, AstFrame *, double *, AstMapping **, int * );
void atlPairAxes( AstFrameSet *, AstFrameSet *, double *, const char *, int *, int * );
void atlPlroi( AstPlot *, AstKeyMap **, int * );
void atlPtftd( AstFitsChan *, const char *, double, const char *, int * );
void atlPtfti( AstFitsChan *, const char *, int, const char *, int * );
void atlPtftl( AstFitsChan *, const char *, int, const char *, int * );
void atlPtftr( AstFitsChan *, const char *, float, const char *, int * );
void atlPtfts( AstFitsChan *, const char *, const char *, const char *,  int * );
void atlRmblft( AstFitsChan *, int * );
void atlShow( AstObject *, const char *, const char *, int * );
void atlTolut( AstMapping *, double, double, double, const char *, AstMapping **, int * );
void atlWcspx( AstKeyMap *, AstKeyMap *, double[3], double, double, AstFrameSet **, int * );

#endif
