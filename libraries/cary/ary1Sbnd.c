#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Sbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, AryACB *acb,
               int *status ) {
/*
*+
*  Name:
*     ary1Sbnd

*  Purpose:
*     Set new array bounds for an ACB entry.

*  Synopsis:
*     void ary1Sbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd,
*                    AryACB *acb, int *status )

*  Description:
*     This function sets new bounds for an array identified by its ACB
*     entry. If the array is a section, then its bounds are simply
*     changed and no other ACB entries are affected. However, if the
*     array is a base array, then the actual data object bounds are
*     altered and this change is reflected in all other ACB entries
*     which refer to it. If the array's data values are defined, then
*     any pixels lying within both the old and new bounds will be
*     retained and any new pixels introduced will be assigned the "bad"
*     value.  Pixels excluded by the new bounds are lost, and cannot be
*     regained by further changes to the array bounds (in the case of
*     changes to a base array, they are lost permanently and can never
*     be recovered).  If bad pixels are introduced (i.e. if the new
*     bounds extend outside the old bounds), then the array's bad pixel
*     flag will be updated to reflect this.

*  Parameters:
*     ndim
*        New number of dimensions for the array.
*     lbnd
*        Array of new lower bounds for the array.
*     ubnd
*        Array of new upper bounds for the array.
*     acb
*        Index to the array's ACB entry.
*     status
*        The global status.

*  Notes:
*     -  The bounds of an array section cannot be changed while mapped
*     access to it is in effect. In the case of a base array, no access
*     to any part of it can be in effect.

*  Side Effects:
*     -  Any data lost as a result of changing the bounds of a base
*     array will cease to be available to any array which refers to that
*     base array.
*     -  If bad pixels are introduced into a base array as a result of
*     calling this routine, then the bad pixel flag will be set and
*     this may be apparent through enquiries made about other arrays
*     which refer to the same base array.

*  Implementation Deficiencies:
*     -  This routine takes a rather pessimistic view of bad pixels; it
*     considers that if bad pixels are introduced by changing the array
*     bounds then they may be positioned anywhere within the new
*     bounds.

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
   AryACB *acbt;              /* ACB to test */
   AryDCB *dcb;               /* Data object */
   AryDCB *dcbt;              /* DCB to test */
   hdsdim ldtw[ARY__MXDIM];   /* New lower data transfer window bounds */
   hdsdim lx[ARY__MXDIM];     /* Lower bounds of retained data region */
   hdsdim udtw[ARY__MXDIM];   /* New upper data transfer window bounds */
   hdsdim ux[ARY__MXDIM];     /* Upper bounds of retained data region */
   int bad;                   /* Bad pixels may have been introduced? */
   int drx;                   /* Whether data have been retained */
   int i;                     /* Loop counter for dimensions */
   int iacbt;                 /* Index of test ACB */
   int next;                  /* Next ACB entry to test */
   int same;                  /* New bounds same as the old ones? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the the bounds of an array section are being changed, then check that
   the section is not mapped for access. */
   if( acb->cut ){
      if( acb->mcb ){

/* Report an error if it is. */
         *status = ARY__ISMAP;
         dcb = acb->dcb;
         datMsg( "ARRAY", dcb->loc );
         errRep( " ", "The array structure ^ARRAY is mapped for access through"
                 "the identifier supplied (possible programming error).",
                 status );

/* Subtract the ACB pixel index shifts from the new bounds to convert them
   to reference frame pixel indices. */
      } else {
         for( i = 0; i < ndim; i++ ){
            ldtw[ i ] = lbnd[ i ] - acb->shift[ i ];
            udtw[ i ] = ubnd[ i ] - acb->shift[ i ];

/* Set the ACB bounds to the new values. */
            acb->lbnd[ i ] = lbnd[ i ];
            acb->ubnd[ i ] = ubnd[ i ];
         }

/* Pad with 1's if necessary. */
         for( ; i < ARY__MXDIM; i++ ){
            ldtw[ i ] = 1;
            udtw[ i ] = 1;
            acb->lbnd[ i ] = 1;
            acb->ubnd[ i ] = 1;
         }

/* Set the new number of dimensions. */
         acb->ndim = ndim;

/* If a data transfer window exists, then find the intersection of this
   window with the new array bounds (converted to the reference frame
   pixel index system). Note whether a data transfer window now exists. */
         if( acb->dtwex ){
            ary1Xsbnd( ARY__MXDIM, ldtw, udtw, ARY__MXDIM, acb->ldtw,
                       acb->udtw, ARY__MXDIM, acb->ldtw, acb->udtw,
                       &acb->dtwex, status );
         }
      }

/* If the array is a base array, then the actual data object bounds must be
   altered. Obtain an index to the data object entry in the DCB. */
   } else {
      dcb = acb->dcb;

/* Check that there is no current access to the array. */
      if( ( dcb->nread != 0 ) || ( dcb->nwrite != 0 ) ){

/* Report an error if there is. */
         *status = ARY__ISMAP;
         datMsg( "ARRAY", dcb->loc );
         errRep( " ", "The base array structure ^ARRAY is mapped for access,"
                 "perhaps through another identifier (possible programming"
                 "error).", status );

/* Set the new bounds for the data object. */
      } else {
         ary1Dsbnd( ndim, lbnd, ubnd, dcb, &same, &drx, lx, ux, status );
         if( *status == SAI__OK ){

/* If the new bounds were the same as the old ones, then there is nothing
   more to do. */
            if( !same ){

/* Otherwise, loop to find all the ACB entries which are affected by the
   change. We lock a mutex first to ensure that no other thread is currently
   accessing the slot array. */
               ARY__ACB_LOCK_MUTEX;
               iacbt = -1;
               next = 0;
               while( 1 ) {
                  acbt = ary1Nxtsl( ARY__ACBTYPE, iacbt, &next, status );
                  if( ( *status == SAI__OK ) && ( next != -1 ) ){
                     iacbt = next;

/* For each ACB entry, obtain a pointer to the data object. Check if the
   ACB describes a base array and refers to the data object which has
   just been altered. */
                     dcbt = acbt->dcb;
                     if( !acbt->cut && dcbt == dcb ){

/* If so, then store the new bounds in the ACB entry. */
                        for( i = 0; i < ndim; i++ ){
                           acbt->lbnd[ i ] = lbnd[ i ];
                           acbt->ubnd[ i ] = ubnd[ i ];
                        }

/* Pad with 1's if necessary. */
                        for( ; i< ARY__MXDIM; i++ ){
                           acbt->lbnd[ i ] = 1;
                           acbt->ubnd[ i ] = 1;
                        }

/* Store the new number of dimensions. */
                        acbt->ndim = ndim;
                     }
                  } else {
                     break;
                  }
               }
               ARY__ACB_UNLOCK_MUTEX;

/* Check to see if any bad pixels may have been introduced due to the
   change in bounds. There will be bad pixels if no data values have been
   retained. */
               bad = !drx;

/* Otherwise, check the bounds of the region of retained data against the
   new bounds to see if padding with bad pixels has been necessary. */
               if( ! bad ){
                  for( i = 0; i < acb->ndim; i++ ){
                     if( ( lx[ i ] > acb->lbnd[ i ] ) ||
                         ( ux[ i ] < acb->ubnd[ i ] ) ){
                        bad = 1;
                        break;
                     }
                  }
               }

/* If bad pixels may have been introduced, then set the bad pixel flag. */
               if( bad ) ary1Sbd( 1, acb, status );
            }
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Sbnd", status );

}
