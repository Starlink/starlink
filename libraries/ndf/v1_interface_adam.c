#include "ary.h"
#include "ndf.h"

/*
*+
* Name:
*    v1_interface_adam.c

* Purpose:
*    Wrapper function for ADAM C functions that provide legacy NDF V1 interface.

* Description:
*    This file wrappers that provide the NDF V1 interfaces for those
*    ADAM C functions that changed interface in going from V1 to V2.
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

void ndfCreat_v1( const char *param, const char *ftype, int ndim,
                  const int lbnd[], const int ubnd[], int *indf, int *status ){
   hdsdim lbnd2[ NDF__MXDIM ];
   hdsdim ubnd2[ NDF__MXDIM ];
   int i;

   for( i = 0; i < ndim; i++ ) {
      lbnd2[ i ] = (size_t) lbnd[ i ];
      ubnd2[ i ] = (size_t) ubnd[ i ];
   }

   ndfCreat_( param, ftype, ndim, lbnd2, ubnd2, indf, status );
}


void ndfCrep_v1( const char *param, const char *ftype, int ndim,
                 const int ubnd[], int *indf, int *status ){
   hdsdim ubnd2[ NDF__MXDIM ];
   int i;

   for( i = 0; i < ndim; i++ ) {
      ubnd2[ i ] = (size_t) ubnd[ i ];
   }

   ndfCrep_( param, ftype, ndim, ubnd2, indf, status );
}

