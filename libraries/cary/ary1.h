#if !defined( ARY1_INCLUDED )   /* Protect against multiple inclusion*/
#define ARY1_INCLUDED 1

/*
*  Name:
*     ary1.h

*  Purpose:
*     Defines the private interface used by ARY.

*  Description:
*     This file defines all the prototypes and data types
*     (private or public) used within the C version of ARY.

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

/* Include the public interface, since it may be used internally as well
   as externally. */
#include "ary.h"

/* Include private data types and constants */
#include "ary1_types.h"

/* Prototypes for private functions */
Ary *ary1Expid( AryACB *acb, int *status );
AryACB *ary1Anl( AryACB *acb, int *status );
AryACB *ary1Id2ac( const Ary *ary );
AryACB *ary1Impid( const Ary *ary, int checklock, int readonly, int *status );
char *ary1Get0C( const HDSLoc *loc, int *status );
char ary1Defr( const AryDCB *dcb, int *status );
char ary1Intyp( const char *type, int *status );
int ary1DCBLock( AryDCB *DCB, int oper, int rdonly, int *status );
int ary1IsValid( AryObject *object, int *status );
void *ary1Ffs( AryBlockType type, int *status );
void *ary1Nxtsl( AryBlockType type, int slot, int *next, int *status );
void *ary1Rls( AryObject *object, int *status );
void ary1Antmp( HDSLoc **loc, int *status );
void ary1Ccpy( const char *cin, size_t len, char *cout, int *status );
void ary1Chscn( const char *name, int *status );
void ary1Cmtmp( const char *type, int ndim, const hdsdim  *dim, HDSLoc **loc, void **pntr, int *status );
void ary1Crnba( AryDCB *dcb, AryACB **acb, int *status );
void ary1Danl( char dispos, AryDCB **dcb, int *status );
void ary1Dbad( AryDCB *dcb, int *status );
void ary1Dbnd( AryDCB *dcb, int *status );
void ary1Dfrm( AryDCB *dcb, int *status );
void ary1Dimp( HDSLoc *loc, AryDCB **dcb, int *status );
void ary1Dlshp( HDSLoc *loc, int mxdim, hdsdim *dim, int *ndim, int *status );
void ary1Dmod( AryDCB *dcb, int *status );
void ary1Dp2s( AryDCB *dcb, int *status );
void ary1Dsbd( char bad, AryDCB *dcb, int *status );
void ary1Dsta( AryDCB *dcb, int *status );
void ary1Dtyp( AryDCB *dcb, int *status );
void ary1Gmrb( AryACB *acb, char *mtrex, char *mrfull, char *whole, int lmrb[ ARY__MXDIM ], int umrb[ ARY__MXDIM ], int lmtr[ ARY__MXDIM ], int umtr[ ARY__MXDIM ], int *status );
void ary1Hunmp( HDSLoc *loc, int *status );
void ary1Imp( HDSLoc *loc, AryACB **acb, int *status );
void ary1Inbnd( int ndim1, const hdsdim *lbnd1, const hdsdim *ubnd1, int ndim2, const hdsdim *lbnd2, const hdsdim *ubnd2, char *inside, int *status );
void ary1Iobw( const char *type, const char *inopt, size_t el, void *pntr, int *status );
void ary1Mpsw( AryACB *acb, HDSLoc *loc, const char *type, const char *inopt, HDSLoc **mloc, char *copy, void **pntr, int *status );
void ary1Ptn( char bad, int ndim, const hdsdim *lbnda, const hdsdim *ubnda, const char *type, const void *pntr, const hdsdim *lsub, const hdsdim *usub, const hdsdim *lbndd, const hdsdim *ubndd, const char *htype, HDSLoc *loc, char *dce, int *status );
void ary1Sbd( char bad, AryACB *acb, int *status );
void ary1Tcnam( HDSLoc *loc, char *name, int *status );
void ary1Temp( const char *type, int ndim, const hdsdim *dim, HDSLoc **loc, int *status );
void ary1Trace( const char *routin, int *status );
void ary1Ump( AryACB *acb, int *status );
void ary1Umps( AryACB *acb, int *status );
void ary1Upsr( char copy, HDSLoc **mloc, int *status );
void ary1Upsw( AryACB *acb, const char *type, char bad, char copy, HDSLoc *datloc, HDSLoc **mloc, void **pntr, char *dce, int *status );
void ary1Vbad( const char *type, size_t n, void *pntr, int *status );
void ary1Vzero( const char *type, size_t n, void *pntr, int *status );
void ary1Xsbnd( int ndim1, const hdsdim *lbnd1, const hdsdim *ubnd1, int ndim2, const hdsdim *lbnd2, const hdsdim *ubnd2, int ndim, int *lbnd, int *ubnd, char *exist, int *status );

/* Now include the expanded generic prototypes. */
#include "ary1_cgen.h"

#endif
