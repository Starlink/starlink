#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

void ary1Bad( AryACB *acb, int check, int *bad, int *status ) {
/*
*+
*  Name:
*     ary1Bad

*  Purpose:
*     Determine whether bad pixels may be present for an ACB entry.

*  Synopsis:
*     void ary1Bad( AryACB *acb, int check, int *bad, int *status )

*  Description:
*     This function obtains the value of the bad pixel flag for an array
*     with an entry in the ACB, taking account of the possibility that
*     the array may be mapped for access. If requested, an explicit
*     check on the presence of bad pixels will be made.

*  Parameters:
*     acb
*        Index to the array entry in the ACB.
*     check
*        If "check" is set to zero, then the routine will simply return
*        the value of the bad pixel flag (indicating whether bad pixels
*        may be present in the array). If "check" is set non-zero, then an
*        explicit check will be performed if necessary, to see if bad
*        values are actually present in the data.
*     bad
*        Returned holding the bad pixel flag value.
*     status
*        The global status.

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
*     5-AUG-2020 (DSB):
*        Fix bug that could result in bad values to be treated literally, caused 
*        by failing to dereference "bad".

*-
*/

/* Local variables: */
   AryDCB *dcb;               /* Data object */
   AryMCB *mcb;               /* Mapping Control Block  */
   hdsdim lmrb[ARY__MXDIM];   /* Lower mapping region bounds */
   hdsdim lmtr[ARY__MXDIM];   /* Lower mapping transfer region bounds */
   hdsdim umrb[ARY__MXDIM];   /* Upper mapping region bounds */
   hdsdim umtr[ARY__MXDIM];   /* Upper mapping transfer region bounds */
   int mrfull;                /* Mapping region full of data? */
   int mtrex;                 /* Mapping transfer region exists? */
   int sure;                  /* Whether bad pixel presence is certain */
   int whole;                 /* Mapping region is whole object? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a pointer to the associated MCB entry. A non-NULL pointer
   indicates that the array is currently mapped for access. */
   mcb = acb->mcb;
   if( mcb ){

/* If it is mapped and the mapping transfer region completely fills the
   mapping region, then the value of the bad pixel flag is determined by
   whether there may be "bad" values in the mapping transfer region. */
      if( mcb->mrful ){
         *bad = mcb->bad;

/* If the mapping transfer region does not completely fill the mapping
   region, but a mapping transfer region does exist, then account must
   also be taken of whether there may be "bad" values in the padding
   region which surrounds the mapping transfer region. */
      } else if( mcb->mtrex ){
         *bad = ( mcb->bad || mcb->pbad );

/* If no mapping transfer region exists, then only the padding region need
   be considered. */
      } else {
         *bad = mcb->pbad;
      }

/* If the mapped data may contain bad pixels and an explicit check is
   required, then perform this check. */
      if( *bad && check ){
         ary1Chbpp( acb, bad, status );
      }

/* If the array is not mapped for access, then obtain the data object
   (DCB) and ensure that state information is available for it. */
   } else {
      dcb = acb->dcb;
      ary1Dsta( dcb, status );
      if( *status == SAI__OK ){

/* If the array is in an undefined state, then there are definitely bad
   pixels present. */
         if( !dcb->state ){
            *bad = 1;
            sure = 1;

/* Otherwise, obtain an initial value of the bad pixel flag from the ACB
   entry. This indicates whether there may be bad values in the data
   transfer window (if it exists). Bad pixels are not certain to be
   present, however. */
         } else {
            *bad = acb->bad;
            sure = 0;
         }

/* If BAD is not 1, then check whether a data transfer window exists;
   the bad pixel flag is 1 if it does not. In this case, the presence
   of bad pixels is certain. */
         if( !*bad ){
            *bad = (acb->dtwex == 0);
            sure = 1;

/* If the bad pixel flag is still not 1, then a further check must be
   made to see whether the data would be padded (with "bad" values) if it
   were accessed. Obtain mapping region bounds information for the ACB
   entry, as if the array were going to be mapped. */
            if( !*bad ){
               ary1Gmrb( acb, &mtrex, &mrfull, &whole, lmrb, umrb, lmtr,
                         umtr, status );

/* The bad pixel flag is 1 if the mapping region is not completely
   filled by the mapping transfer region. In this case, the presence of
   bad pixels is certain. */
               if( *status == SAI__OK ){
                  *bad = (mrfull == 0);
                  sure = 1;
               }
            }
         }
      }

/* If bad pixels may be present, but not for certain, and an explicit check
   is required, then check to see if a data transfer window exists. If so,
   then the presence of bad pixels is certain, so no check need be
   performed. */
      if( *status == SAI__OK ){
         if( *bad && ( !sure ) && check ){
            if( acb->dtwex ){

/* Obtain mapping region bounds information as if the array were going to
   be mapped. */
               ary1Gmrb( acb, &mtrex, &mrfull, &whole, lmrb, umrb, lmtr,
                         umtr, status );

/* If the mapping transfer region does not completely fill the mapping
   region, then the presence of bad pixels is certain, so no check need be
   made.  However, if there is still uncertainty, then perform an explicit
   check for bad pixels. */
               if( mrfull ){
                  ary1Chbpp( acb, bad, status );
               }
            }
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Bad", status );

}
