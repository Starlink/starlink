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
AryACB *ary1Anl( AryACB *acb, int *status );
AryObject *ary1Id2ac( const void *id_ptr, int isacb );
AryObject *ary1Impid( const void *id_ptr, int checklock, int readonly, int isacb, int *status );
char *ary1Get0C( const HDSLoc *loc, int *status );
int ary1DCBLock( AryDCB *DCB, int oper, int rdonly, int *status );
int ary1Defr( const AryDCB *dcb, int *status );
int ary1Intyp( const char *type, int *status );
int ary1IsValid( AryObject *object, int *status );
void *ary1Expid( AryObject *acb, int *status );
void *ary1Ffs( AryBlockType type, int *status );
void *ary1Nxtsl( AryBlockType type, int slot, int *next, int *status );
void *ary1Rls( AryObject *object, int *status );
void ary1Accok( AryACB *acb, const char *access, int *ok, int *status );
void ary1Annpl( int erase, AryPCB **pcb, int *status );
void ary1Antmp( HDSLoc **loc, int *status );
void ary1Bad( AryACB *acb, int check, int *bad, int *status );
void ary1Bpp( const char *type, size_t el, void *pntr, int *bad, int *status );
void ary1Ccpy( const char *cin, size_t len, char *cout, int *status );
void ary1Chacc( AryACB *acb, const char *access, int *status );
void ary1Chbpp( AryACB *acb, int *bad, int *status );
void ary1Chcma( AryACB *acb, const char *mode, int *status );
void ary1Chmod( AryACB *acb, const char *mode, int *status );
void ary1Chscn( const char *name, int *status );
void ary1Cln( AryACB *acb1, AryACB **acb2, int *status );
void ary1Cmtmp( const char *type, int ndim, const hdsdim  *dim, HDSLoc **loc, void **pntr, int *status );
void ary1Cpdlt( AryDCB *dcb1, AryDCB *dcb2, int *status );
void ary1Cpscl( AryDCB *dcb1, AryDCB *dcb2, int *status );
void ary1Cpy( AryACB *acb1, int temp, HDSLoc **loc, int expand, AryACB **acb2, int *status );
void ary1Cpync( HDSLoc *loc1, const char *name, HDSLoc *loc2, int *status );
void ary1Crnba( AryDCB *dcb, AryACB **acb, int *status );
void ary1Cut( AryACB *acb1, int ndim, const hdsdim *lbnd, const hdsdim *ubnd, AryACB **acb2, int *status );
void ary1Danl( int dispos, AryDCB **dcb, int *status );
void ary1Dbad( AryDCB *dcb, int *status );
void ary1Dbnd( AryDCB *dcb, int *status );
void ary1Dcpy( AryDCB *dcb1, int temp, HDSLoc **loc, AryDCB **dcb2, int *status );
void ary1Dcre( int defer, const char *type, int cmplx, int ndim, const hdsdim *lbnd, const hdsdim *ubnd, int temp, HDSLoc *loc, AryDCB **dcb, int *status );
void ary1Dcrep( int defer, const char *type, int ndim, const hdsdim *ubnd, int temp, HDSLoc **loc, AryDCB **dcb, int *status );
void ary1Del( AryACB **acb, int *status );
void ary1Dfppl( HDSLoc *locp, const char *name, HDSLoc **loc, int *status );
void ary1Dfrm( AryDCB *dcb, int *status );
void ary1Dimp( HDSLoc *loc, AryDCB **dcb, int *status );
void ary1Dlshp( HDSLoc *loc, int mxdim, hdsdim *dim, int *ndim, int *status );
void ary1Dmod( AryDCB *dcb, int *status );
void ary1Dobj( AryDCB *dcb, int *status );
void ary1Dp2s( AryDCB *dcb, int *status );
void ary1Drst( AryDCB *dcb, int *status );
void ary1Dsbd( int bad, AryDCB *dcb, int *status );
void ary1Dsbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, AryDCB *dcb, int *same, int *drx, hdsdim *lx, hdsdim *ux, int *status );
void ary1Dscl( AryDCB *dcb, int *status );
void ary1Dsft( int nshift, const hdsdim *shift, AryDCB *dcb, int *status );
void ary1Dsta( AryDCB *dcb, int *status );
void ary1Dstp( const char *type, int cmplx, AryDCB *dcb, int *dce, int *status );
void ary1Dtyp( AryDCB *dcb, int *status );
void ary1Dvfy( AryDCB *dcb, int *status );
void ary1Extyp( AryDCB *dcb, char type[ARY__SZTYP+1], int *status );
void ary1Gmrb( AryACB *acb, int *mtrex, int *mrfull, int *whole, hdsdim lmrb[ ARY__MXDIM ], hdsdim umrb[ ARY__MXDIM ], hdsdim lmtr[ ARY__MXDIM ], hdsdim umtr[ ARY__MXDIM ], int *status );
void ary1Gtdlt( AryDCB *dcb, int *zaxis, char ztype[ARY__SZTYP+1], float *zratio, int *status );
void ary1Gtn( int bad, const char *htype, HDSLoc *loc, int ndim, const hdsdim *lbndd, const hdsdim *ubndd, const hdsdim *lsub, const hdsdim *usub, const char *atype, const hdsdim *lbnda, const hdsdim *ubnda, int pad, HDSLoc *scloc, void *pntr, int *dce, int *status );
void ary1Hunmp( HDSLoc *loc, int *status );
void ary1Imp( HDSLoc *loc, AryACB **acb, int *status );
void ary1Inbnd( int ndim1, const hdsdim *lbnd1, const hdsdim *ubnd1, int ndim2, const hdsdim *lbnd2, const hdsdim *ubnd2, int *inside, int *status );
void ary1Iobw( const char *type, const char *inopt, size_t el, void *pntr, int *status );
void ary1Maps( AryACB *acb, const char *type, int cmplx, const char *mode, const char *inopt, void **dpntr, void **ipntr, int *status );
void ary1Mpsr( AryACB *acb, HDSLoc *loc, const char *type, const char *mode, int bad, HDSLoc **mloc, int *copy, void **pntr, int *dce, int *status );
void ary1Mpsw( AryACB *acb, HDSLoc *loc, const char *type, const char *inopt, HDSLoc **mloc, int *copy, void **pntr, int *status );
void ary1Nel( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, size_t *el, int *status );
void ary1NewOr( HDSLoc *loc, int ndim, HDSLoc **locor, int *status );
void ary1Pbnd( AryACB *acb, int *prim, int *status );
void ary1Ptn( int bad, int ndim, const hdsdim *lbnda, const hdsdim *ubnda, const char *type, const void *pntr, const hdsdim *lsub, const hdsdim *usub, const hdsdim *lbndd, const hdsdim *ubndd, const char *htype, HDSLoc *loc, int *dce, int *status );
void ary1Rebnd( int defer, HDSLoc *paren, const char *name, const char *type, int state, int ndim, const hdsdim *lbnd, const hdsdim *ubnd, int nndim, const hdsdim *nlbnd, const hdsdim *nubnd, HDSLoc **loc, int *same, int *drx, hdsdim *lx, hdsdim *ux, int *status );
void ary1Retyp( HDSLoc *paren, const char *name, const char *type, char state, char bad, int ndim, const hdsdim *dim, const char *ntype, char defer, HDSLoc **loc, int *dce, int *status );
void ary1S2dlt( HDSLoc *loc1, int zaxis, const char *type, HDSLoc *loc2, float *zratio, int *status );
void ary1Sbd( int bad, AryACB *acb, int *status );
void ary1Sbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, AryACB *acb, int *status );
void ary1Sft( int nshift, const hdsdim *shift, AryACB *acb, int *status );
void ary1Stp( const char *type, int cmplx, AryACB *acb, int *status );
void ary1Tcnam( HDSLoc *loc, char *name, int *status );
void ary1Temp( const char *type, int ndim, const hdsdim *dim, HDSLoc **loc, int *status );
void ary1Trace( const char *routin, int *status );
void ary1Ump( AryACB *acb, int *status );
void ary1Umps( AryACB *acb, int *status );
void ary1Undlt( HDSLoc *loc1, int ndim_in, const hdsdim *lbnd, const hdsdim *ubnd, void *pntr, char *bad_out, int *status );
void ary1Upsr( int copy, HDSLoc **mloc, int *status );
void ary1Upsw( AryACB *acb, const char *type, int bad, int copy, HDSLoc *datloc, HDSLoc **mloc, void **pntr, int *dce, int *status );
void ary1Vbad( const char *type, size_t n, void *pntr, int *status );
void ary1Vbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, int *status );
void ary1Vftp( const char *ftype, size_t len, char *type, int *cmplx, int *status );
void ary1Vmmd( const char *mmod,  char mode[ARY__SZMOD+1], char inopt[ARY__SZIOP+1], int *status );
void ary1Vscl( HDSLoc *loc, int *status );
void ary1Vtyp( const char *type, char vtype[ARY__SZTYP+1], int *status );
void ary1Vzero( const char *type, size_t n, void *pntr, int *status );
void ary1Xsbnd( int ndim1, const hdsdim *lbnd1, const hdsdim *ubnd1, int ndim2, const hdsdim *lbnd2, const hdsdim *ubnd2, int ndim, hdsdim *lbnd, hdsdim *ubnd, int *exist, int *status );

/* Now include the expanded generic prototypes. */
#include "ary1_cgen.h"

#endif
