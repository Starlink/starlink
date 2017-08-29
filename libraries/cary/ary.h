#if !defined( ARY_INCLUDED )   /* Protect against multiple inclusion*/
#define ARY_INCLUDED 1

/*
*  Name:
*     ary.h

*  Purpose:
*     Defines the public interface for the ARY library.

*  Description:
*     This file defines all the public prototypes and data types
*     provided by the C version of ARY.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     22-JUN-2017 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
*     All Rights Reserved.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

#include "star/hds.h"

/* Include public data types and constants */
#include "ary_types.h"

/* Prototypes for public functions */
void aryAnnul( Ary **ary, int *status );
void aryBad( Ary *ary, char check, char bad, int *status );
void aryBase( int iary1, int iary2, int *status );
void aryBound( Ary *ary, int ndimx, int lbnd, int ubnd, int ndim,
               int *status );
void aryClone( int iary1, int iary2, int *status );
void aryCmplx( Ary *ary, char cmplx, int *status );
void aryCopy( int iary1, int place, int iary2, int *status );
void aryDelet( Ary *ary, int *status );
void aryDelta( int iary1, int zaxis, const char *type, float minrat,
               int place, float zratio, int iary2, int *status );
void aryDim( Ary *ary, int ndimx, int dim, int ndim, int *status );
void aryDupe( int iary1, int place, int iary2, int *status );
void aryFind( HDSLoc *loc, const char *name, Ary **ary, int *status );
void aryForm( Ary *ary, const char *form, int *status );
void aryFtype( Ary *ary, const char *ftype, int *status );
void aryGtdlt( Ary *ary, int zaxis, const char *ztype, float zratio,
               int *status );
void aryImprt( const char *loc, Ary *ary, int *status );
void aryIsacc( Ary *ary, const char *access, char isacc, int *status );
void aryIsbas( Ary *ary, char base, int *status );
void aryIsmap( Ary *ary, char mapped, int *status );
void aryIstmp( Ary *ary, char temp, int *status );
void aryLoc( Ary *ary, HDSLoc *loc, int *status );
int aryLocked( const Ary *ary, int *status );
void aryMap( Ary *ary, const char *type, const char *mmod, int pntr,
             int el, int *status );
void aryMapz( Ary *ary, const char *type, const char *mmod, int rpntr,
              int ipntr, int el, int *status );
void aryMsg( const char *token, Ary *ary );
void aryNdim( Ary *ary, int ndim, int *status );
void aryNew( const char *ftype, int ndim, int lbnd, int ubnd, int place,
             Ary *ary, int *status );
void aryNewp( const char *ftype, int ndim, int ubnd, int place, Ary *ary,
              int *status );
void aryNoacc( const char *access, Ary *ary, int *status );
void aryOffs( int iary1, int iary2, int mxoffs, int offs, int *status );
void aryPlace( const char *loc, const char *name, int place, int *status );
void aryReset( Ary *ary, int *status );
void arySame( int iary1, int iary2, char same, char isect, int *status );
void arySbad( char bad, Ary *ary, int *status );
void arySbnd( int ndim, int lbnd, int ubnd, Ary *ary, int *status );
void arySctyp( Ary *ary, const char *type, int *status );
void arySect( int iary1, int ndim, int lbnd, int ubnd, int iary2,
              int *status );
void aryShift( int nshift, int shift, Ary *ary, int *status );
void arySize( Ary *ary, int npix, int *status );
void arySsect( int iary1, int iary2, int iary3, int *status );
void aryState( Ary *ary, char state, int *status );
void aryStype( const char *ftype, Ary *ary, int *status );
void aryTemp( int place, int *status );
char aryTrace( char newflg );
void aryType( Ary *ary, const char *type, int *status );
void aryUnlock( Ary *ary, int *status );
void aryUnmap( Ary *ary, int *status );
void aryValid( Ary *ary, char valid, int *status );
void aryVerfy( Ary *ary, int *status );

#endif
