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
void aryBad( Ary *ary, int check, int *bad, int *status );
void aryBase( Ary *ary1, Ary **ary2, int *status );
void aryBound( Ary *ary, int ndimx, hdsdim *lbnd, hdsdim *ubnd, int *ndim,
               int *status );
void aryClone( Ary *ary1, Ary **ary2, int *status );
void aryCmplx( Ary *ary, int *cmplx, int *status );
void aryCopy( Ary *ary1, AryPlace **place, Ary **ary2, int *status );
void aryDelet( Ary **ary, int *status );
void aryDelta( Ary *ary1, int zaxis, const char *type, float minrat, AryPlace **place, float *zratio, Ary **ary2, int *status );
void aryDim( Ary *ary, int ndimx, hdsdim *dim, int *ndim, int *status );
void aryDupe( Ary *iary1, AryPlace **place, Ary **iary2, int *status );
void aryFind( HDSLoc *loc, const char *name, Ary **ary, int *status );
void aryForm( Ary *ary, char form[ARY__SZFRM+1], int *status );
void aryFtype( Ary *ary,  char ftype[ARY__SZFTP+1], int *status );
void aryGtdlt( Ary *ary, int *zaxis, char ztype[DAT__SZTYP+1], float *zratio, int *status );
void aryImprt( HDSLoc *loc, Ary **ary, int *status );
void aryIsacc( Ary *ary, const char *access, int *isacc, int *status );
void aryIsbas( Ary *ary, int *base, int *status );
void aryIsmap( Ary *ary, int *mapped, int *status );
void aryIstmp( Ary *ary, int *temp, int *status );
void aryLoc( Ary *ary, HDSLoc **loc, int *status );
int aryLocked( const Ary *ary, int *status );
void aryMap( Ary *ary, const char *type, const char *mmod, void **pntr,
             size_t *el, int *status );
void aryMapz( Ary *ary, const char *type, const char *mmod, void **rpntr,
              void **ipntr, size_t *el, int *status );
void aryMsg( const char *token, Ary *ary );
void aryNdim( Ary *ary, int *ndim, int *status );
void aryNew( const char *ftype, int ndim, const hdsdim *lbnd, const hdsdim *ubnd,
             AryPlace **place, Ary **ary, int *status );
void aryNewp( const char *ftype, int ndim, int ubnd, int place, Ary *ary,
              int *status );
void aryNoacc( const char *access, Ary *ary, int *status );
void aryOffs( int iary1, int iary2, int mxoffs, int offs, int *status );
void aryPlace( HDSLoc *loc, const char *name, AryPlace **place, int *status );
void aryReset( Ary *ary, int *status );
void arySame( Ary *ary1, Ary *ary2, int *same, int *isect, int *status );
void arySbad( int bad, Ary *ary, int *status );
void arySbnd( int ndim, int lbnd, int ubnd, Ary *ary, int *status );
void arySctyp( Ary *ary, const char *type, int *status );
void arySect( Ary *ary1, int ndim, const hdsdim *lbnd, const hdsdim *ubnd, Ary **ary2, int *status );
void aryShift( int nshift, int shift, Ary *ary, int *status );
void arySize( Ary *ary, int npix, int *status );
void arySsect( int iary1, int iary2, int iary3, int *status );
void aryState( Ary *ary, int *state, int *status );
void aryStype( const char *ftype, Ary *ary, int *status );
void aryTemp( AryPlace **place, int *status );
int aryTrace( int newflg );
void aryType( Ary *ary, const char *type, int *status );
void aryUnlock( Ary *ary, int *status );
void aryUnmap( Ary *ary, int *status );
int aryValid( Ary *ary, int *status );
void aryVerfy( Ary *ary, int *status );

/* Now include the expanded generic prototypes. */
#include "ary_cgen.h"


#endif
