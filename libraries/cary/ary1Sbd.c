#include <pthread.h>
#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

/* A mutex used to ensure that only one thread is searching the list of
   ACB entries at any one time. The mutex is declared in ary1Ffs.c. */
extern pthread_mutex_t Ary_ACB_mutex;
#define LOCK_MUTEX pthread_mutex_lock( &Ary_ACB_mutex );
#define UNLOCK_MUTEX pthread_mutex_unlock( &Ary_ACB_mutex );

void ary1Sbd( char bad, AryACB *acb, int *status ) {
/*
*+
*  Name:
*     ary1Sbd

*  Purpose:
*     Set the bad pixel flag for an array.

*  Synopsis:
*     void ary1Sbd( char bad, AryACB *acb, int *status )

*  Description:
*     The routine sets a bad pixel flag value for a given ACB. A call
*     to this routine constitutes a declaration about the presence
*     (or otherwise) of "bad" values in an array and, hence, their
*     presence (or otherwise) in the data region with which the array
*     is associated via its mapping transfer region. The routine
*     starts by modifying the specified ACB bad pixel flag entry. Then,
*     if appropriate, it updates the actual data object bad pixel flag.
*     Finally, it checks other ACB entries which refer to the same data
*     object and modifies their bad pixel flags if this proves
*     necessary as a result of overlapping mapping transfer regions.

*  Parameters:
*     bad
*        Bad pixel flag value to be set.
*     acb
*        Pointer to the ACB for the array.
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

*-
*/

/* Local variables: */
   AryACB *acbt;              /* Pointer to the test ACB */
   AryDCB *dcb;               /* Pointer to the DCB */
   AryDCB *dcbt;              /* Test DCB */
   char insect;               /* Whether bounds intersect */
   char inside;               /* Whether bounds lie inside */
   char mrfull;               /* Mapping region filled by data? */
   char mtrex;                /* Mapping transfer region exists? */
   char whole;                /* Whole data object to be mapped? */
   hdsdim lmrb[ARY__MXDIM];   /* Lower mapping region bounds */
   hdsdim ltest[ARY__MXDIM];  /* Lower bounds of test region */
   hdsdim lx[ARY__MXDIM];     /* Lower bounds of intersection region */
   hdsdim ubad[ARY__MXDIM];   /* Upper bounds of "bad pixel data" */
   hdsdim umrb[ARY__MXDIM];   /* Upper mapping region bounds */
   hdsdim utest[ARY__MXDIM];  /* Upper bounds of test region */
   hdsdim ux[ARY__MXDIM];     /* Upper bounds of intersection region */
   int iacbt;                 /* Index of test ACB */
   int lbad[ARY__MXDIM];      /* Lower bounds of "bad pixel data" */
   int next;                  /* Next ACB slot which is in use */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a pointer to the DCB and ensure that state information is
   available for it. */
   dcb = acb->dcb;
   ary1Dsta( dcb, status );
   if( *status == SAI__OK ){

/* There is nothing to do if the data object is in an undefined state; its
   bad pixel flag is then always true and cannot be over-ridden. If it
   is in a defined state, then set the bad pixel flag in the ACB
   appropriately. */
      if( dcb->state ){
         acb->bad = bad;

/* If the ACB has a data transfer window associated with it, then
   obtain mapping region bounds information for it, as if it were going to
   be mapped for access. The resulting mapping transfer region indicates
   the region of actual data to which the array has access. */
         if( acb->dtwex ){
            mtrex = 0;
            ary1Gmrb( acb, &mtrex, &mrfull, &whole, lmrb, umrb, lbad, ubad,
                      status );

/* If the mapping transfer region exists, then the the array has access to
   actual data, so the bad pixel flag for the data object may need to be
   changed. */
            if( ( *status == SAI__OK ) && mtrex ){

/* If the bad pixel flag is true, then there may be "bad" values in the
   mapping transfer region, so set the data object flag accordingly. */
               if( bad ){
                  ary1Dsbd( 1, dcb, status );

/* If the bad pixel flag is false, then there are no "bad" values in the
   mapping transfer region, but the data object flag can only be set
   accordingly if this comprises the whole data object. */
               } else {
                  if( whole ) ary1Dsbd( 0, dcb, status );
               }

/* The effect on other ACB entries, whose mapping transfer regions may
   overlap must now be considered. Loop to consider all other ACB entries
   which might be affected. */
               LOCK_MUTEX;
               iacbt = -1;
               next = 0;
               while( 1 ){
                  acbt = ary1Nxtsl( ARY__ACBTYPE, iacbt, &next, status );
                  if( ( *status == SAI__OK ) && ( next != -1 ) ){
                     iacbt = next;

/* Select other ACBs which refer to the same data object and have a
   data transfer window associated with them. */
                     dcbt = acbt->dcb;
                     if( ( dcbt == dcb ) && ( acbt != acb ) && acbt->dtwex ){

/* Calculate mapping region bounds information for each ACB entry being
   tested. */
                        mtrex = 0;
                        ary1Gmrb( acbt, &mtrex, &mrfull, &whole, lmrb, umrb,
                                  ltest, utest, status );

/* The entry can only be affected if its mapping transfer region exists;
   otherwise it has no actual data associated with it. */
                        if( ( *status == SAI__OK ) && mtrex ){

/* If the bad pixel flag is true, then data associated with the ACB
   being tested may contain "bad" values if its mapping transfer region
   intersects with that of the initial ACB. Test if this is so, and
   set its flag accordingly. */
                           if( bad ){
                              insect = 0;
                              ary1Xsbnd( ARY__MXDIM, lbad, ubad, ARY__MXDIM,
                                         ltest, utest, ARY__MXDIM, lx, ux,
                                         &insect, status );
                              if( ( *status == SAI__OK ) && insect ){
                                 acbt->bad = 1;
                              }

/* If the bad pixel flag is false, then we can only be sure there are no
   "bad" values in the data associated with the ACB being tested if
   its mapping transfer region lies entirely inside that of the initial
   ACB. Test if this is so, and set its flag accordingly. */
                           } else {
                              inside = 0;
                              ary1Inbnd( ARY__MXDIM, lbad, ubad, ARY__MXDIM,
                                         ltest, utest, &inside, status );
                              if( ( *status == SAI__OK ) && inside ){
                                 acbt->bad = 0;
                              }
                           }
                        }
                     }
                  }
               }
               UNLOCK_MUTEX;
            }
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Sbd", status );

}
