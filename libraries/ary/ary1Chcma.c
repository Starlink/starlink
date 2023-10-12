#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include "ary_ast.h"
#include <string.h>

void ary1Chcma( AryACB *acb, const char *mode, int *status ) {
/*
*+
*  Name:
*     ary1Chcma

*  Purpose:
*     Check for conflicting mapped access to an array.

*  Synopsis:
*     void ary1Chcma( AryACB *acb, const char *mode, int *status )

*  Description:
*     This function checks that conflicting mapped access to an array
*     will not occur if the requested access is granted.  Separate
*     distinct regions of the same array may be individually mapped
*     with any access mode, and multiple mapped READ access to
*     overlapping regions of an array is also allowed. However, WRITE
*     or UPDATE access to any part of an array may not overlap
*     spatially with any other mapped access to the same array. The
*     routine reports an error if such a conflict will occur. Otherwise
*     it returns without action.

*  Parameters:
*     acb
*        The ACB for the array which is to be mapped.
*     mode
*        The access mode required
*     status
*        The global status.

*  Prior requirements:
*     -  The array mapping region information must first have been set
*     up in the MCB for the new array to be mapped.
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
   AryACB *acbt;              /* ACB entry to test */
   AryDCB *dcb;               /* Data object entry in DCB */
   AryDCB *dcbt;              /* DCB entry to test */
   AryMCB *mcb;               /* Mapping entry in MCB */
   AryMCB *mcbt;              /* MCB entry to test */
   char needwr;               /* Whether write access is needed */
   char umode[ARY__SZMOD+1];  /* Upper case access mode string */
   hdsdim lx[ARY__MXDIM];     /* Lower bound of intersection region */
   hdsdim ux[ARY__MXDIM];     /* Upper bound of intersection region */
   int iacbt;                 /* Index of ACB entry to test */
   int isect;                 /* Whether array bounds intersect */
   int next;                  /* Next ACB slot number */

   ARY__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* There can be no conflict if the requested mapping does not have a mapping
   transfer region. */
   mcb = acb->mcb;
   if( mcb->mtrex ){

/* Find the number of current read and write mapped accesses. There can be
   no conflict if these numbers are zero. */
      dcb = acb->dcb;
      if( ( dcb->nwrite > 0 ) || ( dcb->nread > 0 ) ){

/* Decide if the requested mapping will need to write to the data object. */
         needwr = !strcasecmp( mode, "WRITE" ) || !strcasecmp( mode, "UPDATE" );

/* Loop to inspect all other active entries in the ACB for possible
   conflicts.  We use a mutex to ensure that this search is only being
   performed in one thread at any one time. **/
         ARY__ACB_LOCK_MUTEX;
         next = 0;
         iacbt = -1;
         while( 1 ) {
            acbt = ary1Nxtsl( ARY__ACBTYPE, iacbt, &next, status );
            if( ( *status == SAI__OK ) && ( next != -1 ) ){
               iacbt = next;

/* Get the DCB and MCB indices for each entry being tested. */
               dcbt = acbt->dcb;
               mcbt = acbt->mcb;

/* Select entries which differ from the one requesting access, but which
   refer to the same actual data object and currently have data mapped. */
               if( ( acbt != acb ) && ( dcbt == dcb ) && ( mcbt != NULL ) ){

/* There can only be a conflict if the ACB entry being tested has a mapping
   transfer window and either the requested mapping, or the one being
   tested needs to write to the data object. */
                  if( ( mcbt->mtrex ) && ( needwr ||
                                           !strcmp( mcbt->amm, "WRITE" ) ||
                                           !strcmp( mcbt->amm, "UPDATE" ) ) ){

/* If there may still be a conflict, then see if the contending mapping
   transfer regions overlap spatially. */
                     ary1Xsbnd( ARY__MXDIM, mcb->lmtr, mcb->umtr,
                                ARY__MXDIM, mcbt->lmtr, mcbt->umtr,
                                ARY__MXDIM, lx, ux, &isect, status );

/* If they intersect, then there is a conflict, so report an error. */
                     if( isect ){
                        *status = ARY__CFLAC;
                        datMsg( "ARRAY", dcb->loc );
                        astChrCase( mode, umode, 1, sizeof(umode) );
                        msgSetc( "MODE", umode );
                        errRep( " ", "Requested ^MODE access to the array "
                                "^ARRAY conflicts with existing mapped access "
                                "to the same data object (possible programming "
                                "error).", status );
                        break;
                     }
                  }
               }

            } else {
               break;
            }
         }

/* Allow the next thread to proceed with the above search. */
         ARY__ACB_UNLOCK_MUTEX;
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Chcma", status );

}
