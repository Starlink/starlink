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

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*/

#include "f77.h"
#include "ast.h"

/* Public Constants */
/* ---------------- */

/* Maximum number of axes to consider. This should be equal to 
   NDF__MXDIM. */
enum { ATL__MXDIM  = 7 };


/* Public function prototypes */
/* -------------------------- */
int atlMapGet1S( AstKeyMap *, const char *, int, int, int *, char *, int * );
void atlAxtrm( AstFrameSet *, int *, int *, int *, double *, int * );
void atlKychk( AstKeyMap *, const char *, const char *, int * );
void atlMapPut1S( AstKeyMap *, const char *, const char *, int, int, const char *, int * );
void atlMgfts( int, AstFitsChan *, AstFitsChan *, AstFitsChan **, int * );
void atlMklut( int, int, int, int, AstFrame *, double *, AstMapping **, int * );
void atlPlroi( AstPlot *, AstKeyMap **, int * );
void atlPtfti( AstFitsChan *, const char *, int, const char *, int * );
void atlPtftl( AstFitsChan *, const char *, int, const char *, int * );
void atlPtftr( AstFitsChan *, const char *, float, const char *, int * );
void atlPtfts( AstFitsChan *, const char *, const char *, const char *,  int * );
void atlRmblft( AstFitsChan *, int * );
void atlShow( AstObject *, const char *, const char *, int * );
void atlTolut( AstMapping *, double, double, double, const char *, AstMapping **, int * );
void atlWcspx( AstKeyMap *, AstKeyMap *, double[3], double, double, AstFrameSet **, int * );
void atlGetParam( const char *, AstKeyMap *, int * );


#endif
