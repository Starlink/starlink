#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"

void ary1Rebnd( int defer, HDSLoc *paren, const char *name,
                const char *type, int state, int ndim,
                const hdsdim *lbnd, const hdsdim *ubnd, int nndim,
                const hdsdim *nlbnd, const hdsdim *nubnd, HDSLoc **loc,
                int *same, int *drx, hdsdim *lx, hdsdim *ux, int *status ) {
/*
*+
*  Name:
*     ary1Rebnd

*  Purpose:
*     Change the bounds of an HDS object containing an array component.

*  Synopsis:
*     void ary1Rebnd( int defer, HDSLoc *paren, const char *name,
*                     const char *type, int state, int ndim,
*                     const hdsdim *lbnd, const hdsdim *ubnd, int nndim,
*                     const hdsdim *nlbnd, const hdsdim *nubnd, HDSLoc **loc,
*                     int *same, int *drx, hdsdim *lx, hdsdim *ux,
*                     int *status )

*  Description:
*     This function changes the bounds of a primitive numeric HDS object
*     containing data for an array component, while (optionally)
*     preserving its contents. If the contents are to be preserved,
*     then the data will be re-distributed in the data object and
*     padded with "bad" values (if necessary) to retain the initial
*     N-dimensional distribution of data, but stored in the reshaped
*     array. Note that no data values can be preserved (i.e. the new
*     array will contain only "bad" values) if the old and new array
*     bounds do not intersect.

*  Parameters:
*     defer
*        If non-zero, then the array being resized has not yet been created.
*        In this case, this routine behaves as normal except that no
*        attempt is made to actually resize the HDS object.
*     paren
*        HDS locator to the object's parent structure.
*     name
*        HDS name of the object whose bounds are to be changed.
*     type
*        Data type of the object whose bounds are to be changed. This
*        must be a primitive numeric HDS data type string (case
*        insensitive).
*     state
*        The HDS state of the data object (non-zero for defined, zero
*        for undefined). This argument determines whether the contents
*        of the object are to be preserved. They are only preserved if
*        its value is non-zero.
*     ndim
*        Initial number of object dimensions.
*     lbnd
*        Initial lower bounds of the array whose component is stored in
*        the data object.
*     ubnd
*        Initial upper bound of the array whose component is stored in
*        the data object.
*     nndim
*        New number of data object dimensions.
*     nlbnd
*        New lower bounds for the array.
*     nubnd
*        New upper bounds for the array.
*     loc
*        HDS locator to the object whose bounds are to be changed. Note
*        that changing its bounds may involve erasing the original
*        object and creating a new one, so this locator may be changed.
*        If "defer" is non-zero, the supplied value is ignored, and is
*        unchanged on exit.
*     same
*        Returns a non-zero value if the new array bounds are the same
*        as the old array bounds, so that the routine had nothing to do.
*     drx
*        This argument returns a non-zero value if the data regions of
*        the old and new arrays intersect, so that at least some of the
*        original data has been retained in the new array. A value of
*        zero is returned if "state" is set to zero or if "defer" is
*        set non-zero
*     lx
*        If "drx" is returned with a non-zero value, then this argument
*        returns the lower bounds of the region in the new array which
*        contains data retained from the old array.
*     ux
*        If "drx" is returned with a non-zero value, then this argument
*        returns the upper bounds of the region in the new array which
*        contains data retained from the old array.
*     status
*        The global status.

*  Implementation Deficiencies:
*     -  This routine requires a parent locator in order to function, so
*     it cannot be used on a top-level HDS object.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

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

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   HDSLoc *locs=NULL;         /* Locator to vector slice */
   HDSLoc *locv=NULL;         /* Locator to vectorised object */
   HDSLoc *tloc=NULL;         /* Temporary object locator */
   hdsdim dim[ARY__MXDIM];    /* Object dimensions */
   hdsdim lslice;             /* Lower bound of vector slice */
   hdsdim uslice;             /* Upper bound of vector slice */
   int cpydat;                /* Whether temporary data copy is needed */
   int dce;                   /* Whether data conversion errors occur */
   int full;                  /* Temporary data copy fills new array? */
   int i;                     /* Loop counter for dimensions */
   int mindim;                /* Usable number of axes */
   int newobj;                /* Whether a new data object is needed */
   int there;                 /* Does the named component exist? */
   size_t el;                 /* Number of data elements */
   size_t stride;             /* Stride of final object dimension */
   void *pntr;                /* Pointer to mapped data */
   void *tpntr;               /* Pointer to mapped temporary data copy */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Determine whether the new array bounds are the same as the old bounds.
   First see if the number of dimensions are the same. */
   *same = (nndim == ndim);
   if( *same ){

/* If so, then test the bounds of each dimension for equality. */
      for( i = 0; i < ndim; i++ ){
         if( ( lbnd[ i ] != nlbnd[ i ] ) ||
             ( ubnd[ i ] != nubnd[ i ] ) ){
            *same = 0;
            break;
         }
      }
   }

/* If the new bounds are the same as the old ones, then no reshaping of the
   array is necessary. Set values for the returned arguments. */
   if( *same ){
      *drx = 1;
      for( i = 0; i < nndim; i++ ){
         lx[ i ] = nlbnd[ i ];
         ux[ i ] = nubnd[ i ];
      }
      for( ; i < ARY__MXDIM; i++ ){
         lx[ i ] = 1;
         ux[ i ] = 1;
      }

/* Ensure that the data object is in the expected state by resetting it if
   appropriate. */
      if( !state && !defer ) datReset( *loc, status );

/* If the new and old bounds are not the same, then see if it is necessary
   to create a new data object to hold the new array (as opposed to simply
   altering the existing object). A new object is necessary if the number
   of dimensions will change, as HDS does not support this. */
   } else {
      newobj = ( nndim != ndim );

/* If the number of dimensions match, then the same data object can be
   re-used only if the dimension sizes of all except the last dimension
   are the same in the new array as the old one. Check each of these sizes
   for equality. */
      if( !newobj ){
         for( i = 0; i < ndim - 1; i++ ){
            if( ( ubnd[ i ] - lbnd[ i ] ) !=
                ( nubnd[ i ] - nlbnd[ i ] ) ){
               newobj = 1;
               break;
            }
         }
      }

/* Now decide whether it will be necessary to make a temporary copy of the
   array's data while its bounds are altered. This will not be necessary
   if the data values are currently undefined. */
      if( !state || defer ){
         cpydat = 0;
         *drx = 0;

/* If the data values are defined, then see if there is any region in
   common between the new array and the old array. */
      } else {
         ary1Xsbnd( ndim, lbnd, ubnd, nndim, nlbnd, nubnd, ARY__MXDIM, lx,
                    ux, drx, status );
         if( *status == SAI__OK ){

/* If a new data object is to be created, then a temporary copy of the data
   will be required if the new array will contain any part of the data
   from the old array. */
            if( newobj ){
               cpydat = *drx;

/* Otherwise (i.e. the same object will be re-used) if there is no region
   in common between the new and old arrays, then a temporary copy will
   not be needed. */
            } else if( !*drx ){
               cpydat = 0;

/* In all other cases, a temporary copy of the data will be needed unless
   the data values will occupy the same elements in the new array as in
   the old. This is true if all dimension bounds except the final upper
   bound are the same in the new and old arrays. Test for bounds equality
   in all except the final dimension. */
            } else {
               cpydat = 0;
               for( i = 0; i < ndim - 1; i++ ){
                  if( ( lbnd[ i ] != nlbnd[ i ] ) ||
                      ( ubnd[ i ] != nubnd[ i ] ) ){
                     cpydat = 1;
                     break;
                  }
               }

/* Test for lower bounds equality in the final dimension. */
               cpydat = cpydat || ( lbnd[ ndim - 1 ] != nlbnd[ ndim - 1 ] );
            }
         }
      }

/* If a temporary copy of the data is required, then determine the
   dimension sizes of the region of data in common between the old and new
   arrays. */
      if( *status == SAI__OK ){
         if( cpydat ){
            mindim = ( ndim < nndim ) ? ndim : nndim;
            for( i = 0; i < mindim; i++ ){
               dim[ i ] = ux[ i ] - lx[ i ] + 1;
            }

/* Create and map workspace to hold this region of data. */
            ary1Cmtmp( type, mindim, dim, &tloc, &tpntr, status );

/* Read the appropriate part of the old array into this workspace. */
            ary1Gtn( 0, type, *loc, ndim, lbnd, ubnd, lx, ux, type,
                     lx, ux, 0, NULL, tpntr, &dce, status );
         }

/* Calculate the dimension sizes for the new array. */
         for( i = 0; i < nndim; i++ ){
            dim[ i ] = nubnd[ i ] - nlbnd[ i ] + 1;
         }

/* Do not change the HDS object if the creation of the HDS data array has
   been deferred. */
         if( !defer ){

/* If a new data object is required, then annul the locator to the original
   object and erase it. */
            if( newobj ){
               datAnnul( loc, status );
               datThere( paren, name, &there, status );
               if( there ) datErase( paren, name, status );

/* Create the new object and obtain a locator to it. */
               datNew( paren, name, type, nndim, dim, status );
               datFind( paren, name, loc, status );

/* If the same data object is being re-used, then alter its shape. */
            } else {
               datAlter( *loc, nndim, dim, status );

/* Ensure that the object is in the expected state by resetting it if
   appropriate. */
               if( !state ) datReset( *loc, status );
            }

/* If the array's data values are defined, and a new data object has been
   created and/or a temporary data copy was made, then the (new) object
   may require initialisation before the data are returned if these will
   not fill it entirely. */
            if( state ){
               if( newobj || cpydat ){

/* Check to see if the temporary data copy will completely fill the object
   so that there is no need to initialise. */
                  full = cpydat;
                  if( full ){
                     for( i = 0; i < nndim; i++ ){
                        if( ( lx[ i ] > nlbnd[ i ] ) ||
                            ( ux[ i ] < nubnd[ i ] ) ){
                           full = 0;
                           break;
                        }
                     }
                  }

/* If the object will not be filled by the returned data, then map it and
   initialise it to "bad" values, then unmap it. */
                  if( !full ){
                     datMapV( *loc, type, "WRITE", &pntr, &el, status );
                     ary1Vbad( type, el, pntr, status );
                     ary1Hunmp( *loc, status );
                  }

/* If the same data object has been re-used and the data have remained in
   their original locations (i.e. no temporary copy was necessary) but the
   final dimension has been increased, then there will be a region at the
   end of the object to be initialised. */
               } else if( nubnd[ nndim - 1 ] > ubnd[ nndim - 1 ] ){

/* Calculate the stride for the object's final dimension (the amount by
   which the vectorised array index increases when the index of the final
   dimension increases by 1). */
                  stride = 1;
                  for( i = 0; i < nndim - 1; i++ ){
                     stride *= ( nubnd[ i ] - nlbnd[ i ] + 1 );
                  }

/* Calculate the first and last elements of the region to be initialised in
   the vectorised data object. */
                  lslice = stride*( ubnd[ nndim - 1 ] - nlbnd[ nndim - 1 ] + 1 )+ 1;
                  uslice = stride*( nubnd[ nndim - 1 ] - nlbnd[ nndim - 1 ] + 1 );

/* If there is a gap between the two arrays (i.e. they do not overlap on
   the last pixel axis), ubnd[ nndim - 1 ] will be less than nlbnd[ nndim - 1 ]
   and so the above lslice value will be negative, causing datSlice to
   report an error. */
                  lslice = ( 1 > lslice ) ? 1 : lslice;

/* Vectorise the data object. */
                  datVec( *loc, &locv, status );

/* Locate a slice from this vector containing the region to be initialised. */
                  datSlice( locv, 1, &lslice, &uslice, &locs, status );

/* Map the slice and set it to "bad" values. */
                  datMapV( locs, type, "WRITE", &pntr, &el, status );
                  ary1Vbad( type, el, pntr, status );

/* Annul the slice and vector locators. */
                  datAnnul( &locs, status );
                  datAnnul( &locv, status );
               }

/* If a temporary copy of data is being held, then write it back into the
   appropriate region of the new object. Then erase the workspace. */
               if( cpydat ){
                  ary1Ptn( 0, nndim, lx, ux, type, tpntr, lx, ux,
                           nlbnd, nubnd, type, *loc, &dce, status );
                  ary1Antmp( &tloc, status );
               }
            }
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Rebnd", status );

}
