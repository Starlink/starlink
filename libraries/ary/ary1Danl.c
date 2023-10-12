#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Danl( int dispos, AryDCB **dcb, int *status ) {
/*
*+
*  Name:
*     ary1Danl

*  Purpose:
*     Perform an "annul" operation on a data object.

*  Synopsis:
*     void ary1Danl( int dispos, AryDCB **dcb, int *status )

*  Description:
*     The routine performs an "annul" operation on a DCB entry and
*     optionally disposes of the associated data object. This operation
*     is normally required when an ACB entry is annulled.  The
*     reference count for the data object is decremented and if this is
*     still non-zero, then no further action is taken. However, if the
*     reference count reaches zero, and the DISPOS argument is set to
*     .TRUE., then all locators contained in the DCB entry are annulled
*     (thereby removing any reference to the data object) and the DCB
*     entry is released. If the DISPOS argument is set to .TRUE., the
*     data object will also be disposed of according to the disposal
*     mode specified in the DCB (it is either kept or deleted). If the
*     reference count reaches zero and the DISPOS argument is .FALSE.,
*     then the DCB entry is released, but the data object is not
*     disposed of.

*  Parameters:
*     dispos
*        Whether to dispose of the data object. A value of zero
*        indicates that the data object will remain in use by the ARY_
*        system; the intention being simply to release the specified
*        DCB entry.
*     dcb
*        Pointer to the DCB to be anulled. If the data object reference
*        count falls to zero, then the DCB will be released and a value
*        of NULL will be returned for this argument (if the 'dispos'
*        argument is set to non-zero, the data object will also be
*        disposed of). Otherwise this argument will be unchanged on exit.
*     status
*        The global status.

* Prior Requirements:
*     - The DCB mutex must be locked.

*  Notes:
*     -  The routine attempts to execute even if STATUS is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.
*     -  An error will be reported and STATUS set if a call to this
*     routine results in an attempt to dispose of a data object whose
*     disposal mode is 'KEEP', but whose values are undefined when the
*     ARY_ system has UPDATE access to it (and could therefore have
*     written values to it); the disposal will nevertheless succeed. No
*     such error will be reported if the DISPOS argument is set to
*     zero or if UPDATE access is not available.

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
   int tstat;                 /* Temporary status variable */

   ARY__DCB_ASSERT_MUTEX;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* Decrement the data object reference count. */
   *status = SAI__OK;
   (*dcb)->refcount--;

/* If the reference count falls to zero, then the DCB entry must be
   released. */
   if( (*dcb)->refcount <= 0 ){

/* Ensure that form information is available in the DCB. */
      ary1Dfrm( *dcb, status );

/* If the data object is to be disposed of and has a disposal mode of
   'KEEP', then ensure that access mode and state information (i.e.
   whether its data values are defined or undefined) is available in the
   DCB. */
      if( dispos && !strcmp( (*dcb)->dispose, "KEEP" ) ){
         ary1Dmod( *dcb, status );
         ary1Dsta( *dcb, status );
      }
      if( *status == SAI__OK ){

/* Primitive arrays.
   ================ */
         if( !strcmp( (*dcb)->form, "PRIMITIVE" ) ){

/* If a data component locator has been acquired for the DCB, then annul
   it. */
            if( (*dcb)->ktype && (*dcb)->dloc ){
               datAnnul( &(*dcb)->dloc, status );
            }

/* Simple, scaled and delta arrays.
   ================================ */
         } else if( !strcmp( (*dcb)->form, "SIMPLE" ) ||
                    !strcmp( (*dcb)->form, "SCALED" ) ||
                    !strcmp( (*dcb)->form, "DELTA" ) ){

/* If data component locators have been acquired for the DCB, then annul
   the non-imaginary component locator. */
            if( (*dcb)->ktype && (*dcb)->dloc ){
               datAnnul( &(*dcb)->dloc, status );

/* If it exists, then annul the imaginary component locator as well. */
               if( (*dcb)->complex ){
                  datAnnul( &(*dcb)->iloc, status );
               }
            }

/* Annul any object holding scale information. */
            if( (*dcb)->kscl && (*dcb)->scloc ){
               datAnnul( &(*dcb)->scloc, status );
            }

/* If the DCB form information was not recognised, then report an error. */
         } else {
            *status = ARY__FATIN;
            msgSetc( "BADFORM", (*dcb)->form );
            errRep( "ARY1_DANL_FORM",
                    "Unsupported array form '^BADFORM' found in Data Control "
                    "Block (internal programming error).", status );
         }

/* If the data object is to be disposed of and has a disposal mode of
   'KEEP', then check that it is in a "defined" state. If not, and UPDATE
   access to the data object is available, then an error will be reported
   after the array has been released, so assign the array name to a
   message token for use in constructing the error message. */
         if( dispos && !strcmp( (*dcb)->dispose, "KEEP" ) ){
            if( ( !(*dcb)->state ) && ( !strcmp( (*dcb)->mode, "UPDATE" ) ) ){
               datMsg( "ARRAY", (*dcb)->loc );
            }

/* Release the array by annulling the main locator to it. */
            datAnnul( &(*dcb)->loc, status );

/* If an array is in an undefined state and UPDATE access is available,
   then report an error, as this probably indicates a programming error. */
            if( *status == SAI__OK ){
               if( ( !(*dcb)->state ) &&
                   ( !strcmp( (*dcb)->mode, "UPDATE" ) ) ){
                  *status = ARY__UNDEF;
                  errRep( "ARY1_DANL_UNDEF",
                          "The array ^ARRAY has been released from the ARY_ "
                          "system in an undefined state (possible "
                          "programming error).", status );
               }
            }

/* If the array is being disposed of with a disposal mode other than 'KEEP,
   then annul its locator as if it were a temporary object (it may be a
   temporary object, or it may be an a non-temporary object which is to be
   deleted). This will cause it to be erased. In this case, it does not
   matter if the data values are undefined. */
         } else if( dispos ){
            ary1Antmp( &(*dcb)->loc, status );

/* If the array is not to be disposed of, then simply annul the DCB locator
   to it. */
         } else {
            datAnnul( &(*dcb)->loc, status );
         }

/* Clear the data object file and path name entries from the DCB. */
         (*dcb)->file[ 0 ] = 0;
         (*dcb)->path[ 0 ] = 0;

/* Release the DCB slot associated with the data object and reset the dcb
   argument to NULL. */
         *dcb = ary1Rls( (AryObject *) *dcb, status );
      }
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Danl", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
