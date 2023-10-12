#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void arySame( Ary *ary1, Ary *ary2, int *same, int *isect, int *status ) {
/*
*+
*  Name:
*     arySame

*  Purpose:
*     Enquire if two arrays are part of the same base array.

*  Synopsis:
*     void arySame( Ary *ary1, Ary *ary2, int *same, int *isect,
*                   int *status )

*  Description:
*     This function determines whether two array identifiers refer to
*     parts of the same base array.  If so, it also determines whether
*     they intersect.

*  Parameters:
*     ary1
*        Identifier for the first array (or array section).
*     ary2
*        Identifier for the second array (or array section).
*     same
*        Returned holding a boolean flag indicating whether the identifiers
*        refer to parts of the same base array.
*     isect
*        Returned holding a boolean flag indicating whether the arrays
*        intersect.
*     status
*        The global status.

*  Notes:
*     -  Two arrays (or array sections) are counted as intersecting if
*     (i) they both refer to the same base array and (ii) altering
*     values in one of the arrays can result in the values in the other
*     array changing in consequence.

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
   AryACB *acb1;              /* 1st array index in the ACB */
   AryACB *acb2;              /* 2nd array index in the ACB */
   AryDCB *dcb1;              /* Index to DCB entry for array 1 */
   AryDCB *dcb2;              /* Index to DCB entry for array 2 */
   hdsdim lmrb[ARY__MXDIM];   /* Lower bounds of mapping region */
   hdsdim lmtr1[ARY__MXDIM];  /* Lower bounds mapping tr. region 1 */
   hdsdim lmtr2[ARY__MXDIM];  /* Lower bounds mapping tr. region 2 */
   hdsdim lx[ARY__MXDIM];     /* Lower bound of intersection region */
   hdsdim umrb[ARY__MXDIM];   /* Upper bounds of mapping region */
   hdsdim umtr1[ARY__MXDIM];  /* Upper bounds mapping tr. region 1 */
   hdsdim umtr2[ARY__MXDIM];  /* Upper bounds mapping tr. region 2 */
   hdsdim ux[ARY__MXDIM];     /* Upper bound of intersection region */
   int mrful1;                /* 1st mapping transfer region full? */
   int mrful2;                /* 2nd mapping transfer region full? */
   int mtrex1;                /* 1st mapping transfer region exists? */
   int mtrex2;                /* 2nd mapping transfer region exists? */
   int whole1;                /* 1st array mapped whole? */
   int whole2;                /* 2nd array mapped whole? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the two array identifiers. */
   acb1 = (AryACB *) ary1Impid( ary1, 1, 1, 1, status );
   acb2 = (AryACB *) ary1Impid( ary2, 1, 1, 1, status );

/* Obtain indices to the two data object entries in the DCB. */
   if( *status == SAI__OK ) {
      ARY__DCB_LOCK_MUTEX;

      dcb1 = acb1->dcb;
      dcb2 = acb2->dcb;

/* See if the two arrays refer to the same data object. */
      *same = ( dcb1 == dcb2 );

/* If not, then they cannot intersect. */
      if( ! same ) {
         *isect = 0;

/* If they refer to the same data object, then calculate their mapping
   region bounds, as if they were going to be mapped for access. */
      } else {
         ary1Gmrb( acb1, &mtrex1, &mrful1, &whole1, lmrb, umrb, lmtr1, umtr1,
                   status );
         ary1Gmrb( acb2, &mtrex2, &mrful2, &whole2, lmrb, umrb, lmtr2, umtr2,
                   status );
         if( *status == SAI__OK ) {

/* If either array does not have a mapping transfer window, then they
   cannot intersect. */
            if( !( mtrex1 && mtrex2 ) ) {
               *isect = 0;

/* Otherwise, they intersect if their mapping transfer windows intersect. */
            } else {
               ary1Xsbnd( ARY__MXDIM, lmtr1, umtr1, ARY__MXDIM, lmtr2, umtr2,
                          ARY__MXDIM, lx, ux, isect, status );
            }
         }
      }

      ARY__DCB_UNLOCK_MUTEX;
   }

/* If an error occurred, then report context information and call the
   error tracing routine. */
   if( *status != SAI__OK ) {
      errRep( " ", "arySame: Error determining if two array identifiers "
              "refer to parts of the same base array.", status );
      ary1Trace( "arySame", status );
   }

}
