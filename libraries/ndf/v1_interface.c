#include <limits.h>
#include "ary.h"
#include "ndf.h"
#include "mers.h"
#include "sae_par.h"
#include "ndf_err.h"
#include "ndf1_types.h"


/*
*+
* Name:
*    v1_interface.c

* Purpose:
*    Wrapper function for stand-alone C functions that provide legacy
*    NDF V1 interface.

* Description:
*    This file wrappers that provide the NDF V1 interfaces for those
*    non-ADAM C functions that changed interface in going from V1 to V2.
*    This is for the benefit of legacy systems that have not yet updated
*    to V2.

* Copyright:
*    Copyright (C) 2018 East Asian Observatory
*    All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

* Authors:
*     DSB: David S Berry (EAO)

* History:
*     10-AUG-2018 (DSB):
*       Original version.

*-
*/


void ndfAmap_v1( int indf, const char *comp, int iaxis, const char *type,
                 const char *mmod, void *pntr[], int *el, int *status ){
   size_t el2;

   ndfAmap_( indf, comp, iaxis, type, mmod, pntr, &el2, status );

   if( el2 > INT_MAX && *status == SAI__OK ) {
      ndfMsg( "N", indf );
      *status = NDF__TOOBIG;
      errRep( " ", "ndfAmap: NDF '^N' has too many pixels for NDF V1", status );
      errRep( " ", "Re-link your software with V2 of the NDF library.", status );
   } else {
      *el = (int) el2;
   }
}


void ndfBound_v1( int indf, int ndimx, int lbnd[], int ubnd[], int *ndim,
                  int *status ){
   hdsdim lbnd2[ NDF__MXDIM ];
   hdsdim ubnd2[ NDF__MXDIM ];
   int i;

   ndfBound_( indf, NDF_MIN(ndimx, NDF__MXDIM), lbnd2, ubnd2, ndim, status );

   if( *status == SAI__OK ) {
      for( i = 0; i < NDF_MIN(ndimx, *ndim); i++ ) {
         if( lbnd2[i] < -INT_MAX || ubnd2[i] > INT_MAX ) {
            ndfMsg( "N", indf );
            msgSeti( "I", i + 1 );
            *status = NDF__TOOBIG;
            errRep( " ", "ndfBound: Axis ^I of NDF '^N' has too many pixels for NDF V1", status );
            errRep( " ", "Re-link your software with V2 of the NDF library.", status );
            break;
         } else {
            lbnd[ i ] = (int) lbnd2[ i ];
            ubnd[ i ] = (int) ubnd2[ i ];
         }
      }
      for( ; i < ndimx; i++ ) {
         lbnd[ i ] = 1;
         ubnd[ i ] = 1;
      }
   }
}

void ndfDim_v1( int indf, int ndimx, int dim[], int *ndim, int *status ){
   hdsdim dim2[ NDF__MXDIM ];
   int i;

   ndfDim_( indf, NDF_MIN(ndimx, NDF__MXDIM), dim2, ndim, status );

   if( *status == SAI__OK ) {
      for( i = 0; i < NDF_MIN(ndimx, *ndim); i++ ) {
         if( dim2[i] > INT_MAX ) {
            ndfMsg( "N", indf );
            msgSeti( "I", i + 1 );
            *status = NDF__TOOBIG;
            errRep( " ", "ndfDim: Axis ^I of NDF '^N' has too many pixels for NDF V1", status );
            errRep( " ", "Re-link your software with V2 of the NDF library.", status );
            break;
         } else {
            dim[ i ] = (int) dim2[ i ];
         }
      }
      for( ; i < ndimx; i++ ) {
         dim[ i ] = 1;
      }
   }
}

void ndfMap_v1( int indf, const char *comp, const char *type,
                const char *mmod, void *pntr[], int *el, int *status ){
   size_t el2;

   ndfMap_( indf, comp, type, mmod, pntr, &el2, status );

   if( el2 > INT_MAX && *status == SAI__OK ) {
      ndfMsg( "N", indf );
      *status = NDF__TOOBIG;
      errRep( " ", "ndfMap: NDF '^N' has too many pixels for NDF V1", status );
      errRep( " ", "Re-link your software with V2 of the NDF library.", status );
   } else {
      *el = (int) el2;
   }
}

void ndfMapql_v1( int indf, int **pntr, int *el, int *bad, int *status ){
   size_t el2;

   ndfMapql_( indf, pntr, &el2, bad, status );

   if( el2 > INT_MAX && *status == SAI__OK ) {
      ndfMsg( "N", indf );
      *status = NDF__TOOBIG;
      errRep( " ", "ndfMapql: NDF '^N' has too many pixels for NDF V1", status );
      errRep( " ", "Re-link your software with V2 of the NDF library.", status );
   } else {
      *el = (int) el2;
   }
}


void ndfMapz_v1( int indf, const char *comp, const char *type,
                 const char *mmod, void *rpntr[], void *ipntr[], int *el,
                 int *status ){
   size_t el2;

   ndfMapz_( indf, comp, type, mmod, rpntr, ipntr, &el2, status );

   if( el2 > INT_MAX && *status == SAI__OK ) {
      ndfMsg( "N", indf );
      *status = NDF__TOOBIG;
      errRep( " ", "ndfMapz: NDF '^N' has too many pixels for NDF V1", status );
      errRep( " ", "Re-link your software with V2 of the NDF library.", status );
   } else {
      *el = (int) el2;
   }
}

void ndfNew_v1( const char *ftype, int ndim, const int lbnd[],
                const int ubnd[], int *place, int *indf, int *status ){

   hdsdim lbnd2[ NDF__MXDIM ];
   hdsdim ubnd2[ NDF__MXDIM ];
   int i;

   for( i = 0; i < NDF_MIN(ndim, NDF__MXDIM); i++ ) {
      lbnd2[ i ] = (size_t) lbnd[ i ];
      ubnd2[ i ] = (size_t) ubnd[ i ];
   }

   ndfNew_( ftype, ndim, lbnd2, ubnd2, place, indf, status );
}

void ndfNewp_v1( const char *ftype, int ndim, const int ubnd[],
                 int *place, int *indf, int *status ){

   hdsdim ubnd2[ NDF__MXDIM ];
   int i;

   for( i = 0; i < NDF_MIN(ndim, NDF__MXDIM); i++ ) {
      ubnd2[ i ] = (size_t) ubnd[ i ];
   }

   ndfNewp_( ftype, ndim, ubnd2, place, indf, status );
}


void ndfSbnd_v1( int ndim, const int lbnd[], const int ubnd[], int indf,
                 int *status ){

   hdsdim lbnd2[ NDF__MXDIM ];
   hdsdim ubnd2[ NDF__MXDIM ];
   int i;

   for( i = 0; i < NDF_MIN(ndim, NDF__MXDIM); i++ ) {
      lbnd2[ i ] = (size_t) lbnd[ i ];
      ubnd2[ i ] = (size_t) ubnd[ i ];
   }

   ndfSbnd_( ndim, lbnd2, ubnd2, indf, status );
}


void ndfSect_v1( int indf1, int ndim, const int lbnd[], const int ubnd[],
                 int *indf2, int *status ){

   hdsdim lbnd2[ NDF__MXDIM ];
   hdsdim ubnd2[ NDF__MXDIM ];
   int i;

   for( i = 0; i < NDF_MIN(ndim, NDF__MXDIM); i++ ) {
      lbnd2[ i ] = (size_t) lbnd[ i ];
      ubnd2[ i ] = (size_t) ubnd[ i ];
   }

   ndfSect_( indf1, ndim, lbnd2, ubnd2, indf2, status );
}


void ndfShift_v1( int nshift, const int shift[], int indf, int *status ){
   hdsdim shift2[ NDF__MXDIM ];
   int i;

   for( i = 0; i < NDF_MIN(nshift, NDF__MXDIM); i++ ) {
      shift2[ i ] = (size_t) shift[ i ];
   }

   ndfShift_( nshift, shift2, indf, status );
}


void ndfSize_v1( int indf, int *npix, int *status ){
   size_t npix2;

   ndfSize_( indf, &npix2, status );

   if( npix2 > INT_MAX && *status == SAI__OK ) {
      ndfMsg( "N", indf );
      *status = NDF__TOOBIG;
      errRep( " ", "ndfSize: NDF '^N' has too many pixels for NDF V1", status );
      errRep( " ", "Re-link your software with V2 of the NDF library.", status );
   } else {
      *npix = (int) npix2;
   }
}


void ndfSsary_v1( int iary1, int indf, int *iary2, int *status ){
   Ary *ary1 = aryI2A( iary1 );
   Ary *ary2 = NULL;
   ndfSsary_( ary1, indf, &ary2, status );
   *iary2 = aryA2I( ary2 );
}


void ndfXiary_v1( int indf, const char *xname, const char *cmpt,
                  const char *mode, int *iary, int *status ){
   Ary *ary = NULL;
   ndfXiary_( indf, xname, cmpt, mode, &ary, status );
   *iary = aryA2I( ary );
}


void ndfXnew_v1( int indf, const char *xname, const char *type, int ndim,
                 const int dim[], HDSLoc **loc, int *status ){

   hdsdim dim2[ NDF__MXDIM ];
   int i;

   for( i = 0; i < NDF_MIN(ndim, NDF__MXDIM); i++ ) {
      dim2[ i ] = (size_t) dim[ i ];
   }

   ndfXnew_( indf, xname, type, ndim, dim2, loc, status );
}

