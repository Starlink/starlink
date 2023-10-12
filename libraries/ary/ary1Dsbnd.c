#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Dsbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, AryDCB *dcb,
                int *same, int *drx, hdsdim *lx, hdsdim *ux, int *status ) {
/*
*+
*  Name:
*     ary1Dsbnd

*  Purpose:
*     Change the bounds of an array data object.

*  Synopsis:
*     void ary1Dsbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd,
*                     AryDCB *dcb, int *same, int *drx, hdsdim *lx,
*                     hdsdim *ux, int *status ) {

*  Description:
*     This function changes the lower and upper bounds of an array data
*     object identified by its DCB entry. If the array's data values
*     are defined, then any data which lie within both the original and
*     new array bounds will be preserved in the correct pixel location
*     within the re-structured array. Any new pixel locations created
*     by the imposition of new bounds will be filled with the bad value
*     (unless the array's data values are undefined). The bad pixel
*     flag is not altered by this routine.

*  Parameters:
*     ndim
*        The new number of array dimensions.
*     lbnd
*        The new array of lower pixel index bounds for the array.
*     ubnd
*        The new array of upper pixel index bounds for the array.
*     dcb
*        The DCB for the array.
*     same
*        Returned non-zeri if the new array bounds are the same as the
*        old bounds (in which case the routine will have returned
*        without action as there is nothing to do).
*     drx
*        Returned non-zero if there is at least 1 pixel value
*        which lies within both the old and new pixel index bounds.
*        A value of zero is returned if the array's data values are
*        undefined.
*     lx
*        The lower pixel index bounds of the region in common between
*        the old and new array bounds. Not used if "drx" is zero.
*     ux
*        The upper pixel index bounds of the region in common between
*        the old and new array bounds. Not used if "drx" is zero.
*     status
*        The global status.

* Prior Requirements:
*     -  The DCB mutex must be locked.

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
   HDSLoc *loc=NULL;          /* Origin component locator */
   HDSLoc *locp=NULL;         /* Parent structute locator */
   HDSLoc *loct=NULL;         /* Temporary locator */
   char name[DAT__SZNAM+1];   /* Object name */
   hdsdim dim;                /* Size of the origin component */
   int chorig;                /* Whether the origin has changed */
   int cvt;                   /* Whether form conversion is needed */
   int defer;                 /* Creation of data array deferred? */
   int i;                     /* Loop counter for dimensions */
   int there;                 /* Whether origin component present */

   ARY__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that form information is available in the DCB. */
   ary1Dfrm( dcb, status );

/* Set a flag indicating if creation of the HDS primitive data array has
   been deferrred until the array is mapped. */
   defer = ary1Defr( dcb, status );

/* Handle each form of array in turn... */
   if( *status == SAI__OK ){

/* Primitive arrays.
   ================ */
      if( !strcmp( dcb->form, "PRIMITIVE" ) ){

/* Ensure that data type, state and bounds information is available for the
   data object in the DCB. */
         ary1Dtyp( dcb, status );
         ary1Dsta( dcb, status );
         ary1Dbnd( dcb, status );
         if( *status == SAI__OK ){

/* See if conversion from primitive to simple storage form is required. */
            cvt = 0;
            for( i = 0; i < ndim; i++ ){

/* Conversion is required if any of the new lower array bounds is not equal
   to 1. */
               if( lbnd[ i ] != 1 ){
                  cvt = 1;
                  break;
               }
            }

/* If no conversion is needed, then the bounds of the primitive array must
   be changed. This may involve erasing the existing object and creating a
   new one, so annul the non-imaginary component locator which will be
   re-acquired later. */
            if( !cvt ){
               if( dcb->dloc ) datAnnul( &dcb->dloc, status );

/* Obtain a locator to the array's parent structure and obtain the name of
   the array. */
               datParen( dcb->loc, &locp, status );
               datName( dcb->loc, name, status );

/* Change the array bounds, possibly obtaining a new data object locator as
   a result. */
               ary1Rebnd( defer, locp, name, dcb->type, dcb->state, dcb->ndim,
                          dcb->lbnd, dcb->ubnd, ndim, lbnd, ubnd, &dcb->loc,
                          same, drx, lx, ux, status );

/* Derive a new non-imaginary component locator by cloning the data object
   locator. We do not do this if the creation of the HDS data array has
   been deferred since the a null value for DLOC is one of the things that
   flags a deferred array (see ary1Defr). */
               if( !defer ) datClone( dcb->loc, &dcb->dloc, status );

/* Annul the parent locator. */
               datAnnul( &locp, status );

/* If conversion from primitive to simple storage form is needed, then
   perform the conversion. */
            } else {
               ary1Dp2s( dcb, status );

/* Report context information if the conversion failed. */
               if( *status != SAI__OK ){
                  errRep( " ", "Unable to perform implicit conversion from "
                          "'PRIMITIVE' to 'SIMPLE' array storage form.",
                          status );
               } else {

/* Otherwise, change the bounds of the non-imaginary data component in what
   is now a simple array. */
                  ary1Rebnd( defer, dcb->loc, "DATA", dcb->type, dcb->state,
                             dcb->ndim, dcb->lbnd, dcb->ubnd, ndim, lbnd,
                             ubnd, &dcb->dloc, same, drx, lx, ux, status );

/* Create an ORIGIN component in the data object and enter the new origin
   values. */
                  ary1NewOr( dcb->loc, ndim, &loct, status );
                  HDSDIM_CODE(datPut1)( loct, ndim, lbnd, status );
                  datAnnul( &loct, status );
               }
            }
         }

/* Simple and scaled arrays.
   ========================= */
      } else if( !strcmp( dcb->form, "SIMPLE" ) ||
                 !strcmp( dcb->form, "SCALED" ) ){

/* Ensure that data type, state and bounds information is available for the
   data object in the DCB. */
         ary1Dtyp( dcb, status );
         ary1Dsta( dcb, status );
         ary1Dbnd( dcb, status );

/* Change the bounds of the non-imaginary data component. */
         ary1Rebnd( defer, dcb->loc, "DATA", dcb->type, dcb->state, dcb->ndim,
                    dcb->lbnd, dcb->ubnd, ndim, lbnd, ubnd, &dcb->dloc, same,
                    drx, lx, ux, status );

/* If the array holds complex data, then change the bounds of the imaginary
   data component. */
         if( dcb->complex ){
            ary1Rebnd( defer, dcb->loc, "IMAGINARY_DATA", dcb->type,
                       dcb->state, dcb->ndim, dcb->lbnd,
                       dcb->ubnd, ndim, lbnd, ubnd, &dcb->iloc,
                       same, drx, lx, ux, status );
         }

/* See if the array origin has changed. It will have done if the number of
   dimensions has changed. */
         if( *status == SAI__OK ){
            chorig = ( ndim != dcb->ndim );

/* Otherwise, check the new lower bound of the array in each dimension to
   see if that has changed. */
            if( !chorig ){
               for( i = 0; i < ndim; i++ ){
                  if( lbnd[ i ] != dcb->lbnd[ i ]){
                     chorig = 1;
                     break;
                  }
               }
            }

/* If the origin has changed, then see if an ORIGIN component is present in
   the data object. */
            if( chorig ){
               datThere( dcb->loc, "ORIGIN", &there, status );
               if( *status == SAI__OK ){

/* If not, then create one with the required number of elements.
   Either way, obtain a locator to the ORIGIN component. */
                  if( !there ) {
                     ary1NewOr( dcb->loc, ndim, &loc, status );
                  } else {
                     datFind( dcb->loc, "ORIGIN", &loc, status );
                  }

/* If this component was initially present, but now has the wrong number of
   elements, then change the number of elements. */
                  if( there && ( ndim != dcb->ndim ) ){
                     dim = ndim;
                     datAlter( loc, 1, &dim, status );
                  }

/* Enter the origin values. */
                  HDSDIM_CODE(datPut1)( loc, ndim, lbnd, status );

/* Annul the origin locator. */
                  datAnnul( &loc, status );
               }
            }
         }


/* Delta arrays
   ============ */
      } else if( !strcmp( dcb->form, "DELTA" ) ){

/* Delta arrays cannot be changed, so report an error if this routine is
   called. */
         if( *status == SAI__OK ){
            *status = ARY__CMPAC;
            datMsg( "A", dcb->loc );
            errRep( " ", "The array ^A is stored using DELTA compression "
                    "and therefore its bounds cannot be changed (DELTA "
                    "compressed arrays are read-only).", status );
         }


/* If the DCB form value was not recognised, then report an error. */
      } else {
         *status = ARY__FATIN;
         msgSetc( "B", dcb->form );
         errRep( " ", "Invalid array form '^B' found in Data Control Block "
                 "(internal programming error).", status );
      }
   }

/* Modify the DCB bounds information to reflect the changes made to the
   data object. */
   if( *status == SAI__OK ){
      for( i = 0; i < ndim; i++ ){
         dcb->lbnd[ i ] = lbnd[ i ];
         dcb->ubnd[ i ] = ubnd[ i ];
      }

/* Pad the bounds with 1's if necessary. */
      for( ; i < ARY__MXDIM; i++ ){
         dcb->lbnd[ i ] = 1;
         dcb->ubnd[ i ] = 1;
      }
      dcb->ndim = ndim;
   }

/* Note if bounds information is now available in the DCB. */
   dcb->kbnd = ( *status == SAI__OK );

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dsbnd", status );

}
