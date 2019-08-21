#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Dbad( NdfACB *acb, int check, int *bad, int *status ){
/*
*+
*  Name:
*     ndf1Dbad

*  Purpose:
*     Determine the bad pixel flag for the data component of an NDF.

*  Synopsis:
*     void ndf1Dbad( NdfACB *acb, int check, int *bad, int *status )

*  Description:
*     This function obtains the logical value of the bad pixel flag for the
*     data component of an NDF, taking account of the possibility that the
*     array may be mapped for access. An explicit check of the array for
*     bad pixels may be specified if required. The NDF is identified by its
*     ACB entry.

*  Parameters:
*     acb
*        Pointer to the NDF's ACB entry.
*     check
*        Whether an explicit check for the presence of bad pixels should be
*        performed if it appears that they may be present.
*     *bad
*        Returned holding the bad pixel flag.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   Ary *qid;             /* ARY_ system quality identifier */
   int there;            /* Whether the data values are defined */
   size_t el;            /* Number of mapped values */
   unsigned char badbit; /* Quality bad-bits mask */
   void *pntr;           /* Pointer to mapped values */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if the data component is currently mapped for access. */
   if( acb->dmap ) {

/* If so, then obtain the bad pixel flag for the mapped values. */
      *bad = acb->dmbad;

/* If bad pixels may be present and an explicit check is required, then
   determine the number of mapped elements and check the mapped
   non-imaginary variance values for bad pixels. */
      if( *bad && check ) {
         arySize( acb->did, &el, status );
         ndf1Bpp( acb->dmtyp, el, acb->dmdpt, bad, status );
         if( *status == SAI__OK ) {

/* If no bad pixels were found and complex values are mapped, then check
   the imaginary component similarly. */
            if( ( !*bad ) && acb->dmcpx ) ndf1Bpp( acb->dmtyp, el, acb->dmipt,
                                                   bad, status );
         }
      }

/* If the data component is not mapped for access, then see if its
   values are defined. */
   } else {
      aryState( acb->did, &there, status );
      if( *status == SAI__OK ) {

/* If data values are undefined, then "bad" is non-zero. */
         if( !there ) {
            *bad = 1;

/* If data values are defined, then obtain the bad pixel flag via the
   ARY_ system. */
         } else {
            aryBad( acb->did, check, bad, status );
            if( *status == SAI__OK ) {

/* If no bad pixels were found but the quality masking flag is set, then
   the quality component must also be checked. */
               if( ( !*bad ) && acb->qmf ) {

/* Obtain the quality bad-bits mask value and check it is not zero. No
   bad pixels can be introduced by the quality component if it is. */
                  ndf1Gtbb( acb, &badbit, status );
                  if( *status == SAI__OK ) {
                     if( badbit != 0 ) {

/* Obtain the state of the quality component. */
                        if( *status == SAI__OK ) {
                           ndf1Qsta( acb, bad, status );

/* If quality is present it may introduce bad pixels, but if an
   explicit check is required, then clone the ARY_ system quality array
   identifier and map it to obtain the quality values (it must be
   cloned in case quality is already mapped through the current ACB
   entry). */
                           if( *status == SAI__OK ) {
                              if( *bad && check ) {
                                 aryClone( acb->qid, &qid, status );
                                 aryMap( qid, "_UBYTE", "READ", &pntr, &el,
                                         status );

/* If access to the quality values could not be obtained, then report
   context information. */
                                 if( *status != SAI__OK ) {
                                    errRep( " ", "Unable to access the "
                                            "NDF's quality component to "
                                            "check for bad pixels.", status );

/* Inspect the quality array to see if bad pixels are actually present. */
                                 } else {
                                    ndf1Qbpp( badbit, el, pntr, bad, status );
                                 }

/* Annul the cloned ARY_ system quality identifier, thereby unmapping
   it. */
                                 aryAnnul( &qid, status );
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dbad", status );

}

