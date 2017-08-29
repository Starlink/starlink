#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Umps( AryACB *acb, int *status ) {
/*
*+
*  Name:
*     ary1Umps

*  Purpose:
*     Unmap a simple array.

*  Synopsis:
*     void ary1Umps( AryACB *acb, int *status )

*  Description:
*     The routine unmaps a simple array which has previously been
*     mapped for access and updates the associated data object
*     (including its mapping reference count and state if appropriate).
*     If the routine completes successfully, then the MCB slot used for
*     the mapping is released and the ACB entry is marked as no longer
*     being mapped.

*  Parameters:
*     acb
*        Pointer to the ACB.
*     status
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Prior Requirements:
*     -  This routine should only be called for an ACB entry which has
*     previously been used to obtain mapped access to an array and has
*     not since been unmapped.

*  Side Effects:
*     -  Write-back of data by this routine may affect the data values
*     which are accessed via other ACB entries, where the associated
*     arrays refer to the same data object and overlap spatially.
*     -  If data conversion errors occur during write-back of data,
*     then the data object bad pixel flag may be updated (if a mapping
*     transfer region exists). This may affect other ACB entries where
*     the associated arrays refer to the same data object and overlap
*     with the affected data region.

*  Notes:
*     -  This routine may also be used for unmapping primitive arrays.

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
   AryDCB *dcb;               /* Pointer to DCB */
   AryMCB *mcb;               /* Pointer to MCB */
   HDSLoc *mloc;              /* Locator to mapped imaginary data */
   char bad;                  /* Possible "bad" values written back? */
   char copy;                 /* Whether mapping involves a "copy" */
   char dce;                  /* Non-imaginary data conversion error? */
   char idce;                 /* Imaginary data conversion error? */
   int tstat;                 /* Temporary status value */
   void *pntr;                /* Pointer to mapped imaginary data */

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* Obtain pointers to the associated MCB and DCB. */
   *status = SAI__OK;
   mcb = acb->mcb;
   dcb = acb->dcb;

/* If the MCB pointer is NULL, then the array is not mapped, so
   report an error. */
   if( ! mcb ){
      *status = ARY__NTMAP;
      datMsg( "ARRAY", dcb->loc );
      errRep( " ", "The array ^ARRAY is not currently mapped for access "
              "through the specified identifier (possible programming error).",
              status );

/* If the array is mapped for READ access, then unmap the non-imaginary
   component. No values will be written back to the data object. */
   } else {
      if( !strcmp( mcb->amm, "READ" )){
         ary1Upsr( mcb->dcopy, &mcb->dloc, status );

/* Similarly unmap the imaginary component if this was mapped. */
         if( mcb->complex ){
            ary1Upsr( mcb->icopy, &mcb->iloc, status );
         }

/* Decrement the data object mapping count. */
         if( *status == SAI__OK ){
            dcb->nread--;
         }

/* If the array is mapped for WRITE or UPDATE access, then unmap the
   non-imaginary component. If there is a mapping transfer region, then
   the values lying within it will be written back into the data object. */
      } else if( !strcmp( mcb->amm, "WRITE" ) ||
                 !strcmp( mcb->amm, "UPDATE" ) ){
         dce = 0;
         ary1Upsw( acb, mcb->type, mcb->bad, mcb->dcopy, dcb->dloc,
                   &mcb->dloc, &mcb->dpntr, &dce, status );

/* If access was given to a complex component and there is a complex
   component in the data object, then unmap this component in the same
   way, causing values within the mapping transfer region to be written
   back. */
         idce = 0;
         if( mcb->complex ){
            if( dcb->complex ){
               ary1Upsw( acb, mcb->type, mcb->bad, mcb->icopy, dcb->iloc,
                         &mcb->iloc, &mcb->ipntr, &idce, status );

/* If access to a complex component was given, but there is no such
   component in the data object, then unmap it as if it were mapped for
   READ access. No data values are then written back and any associated
   temporary object is erased. */
            } else {
               ary1Upsr( mcb->icopy, &mcb->iloc, status );
            }

/* If access was only obtained to the non-imaginary component, but an
   imaginary component exists in the data object, then the data type
   conversion rules require that values of the imaginary component lying
   within the mapping transfer region should be set to zero in the data
   object. Only do this if a mapping transfer region exists. */
         } else {
            if( dcb->complex && mcb->mtrex ){

/* Map the imaginary component for WRITE access, specifying initialisation
   to zero, then unmap it so that the zeros are written back into the data
   object. Choose the data type so as to avoid any type conversion
   overheads. */
               ary1Mpsw( acb, dcb->iloc, dcb->type, "ZERO", &mloc, &copy,
                         &pntr, status );
               ary1Upsw( acb, dcb->type, 0, copy, dcb->iloc, &mloc,
                         &pntr, &idce, status );
            }
         }

/* Set the DCB state to "defined". */
         if( *status == SAI__OK ){
            dcb->state = 1;
            dcb->kstate = 1;

/* If the array was mapped for WRITE access, then decrement the count of
   current mappings which write to the data object. */
            if( !strcmp( mcb->amm, "WRITE" )){
               dcb->nwrite--;

/* In UPDATE mode, decrement both the read and write mapping counts. */
            } else if( !strcmp( mcb->amm, "UPDATE" ) ){
               dcb->nread--;
               dcb->nwrite--;
            }

/* Note if any data conversion errors occurred while unmapping data mapped
   with WRITE or UPDATE access. Determine whether any "bad" values may
   have been written back into the data object and update the bad pixel
   flag accordingly. */
            dce = dce || idce;
            bad = mcb->bad || dce;
            ary1Sbd( bad, acb, status );
         }

/* If the active mapping mode entry in the MCB is not valid, then report an
   error. */
      } else {
         *status = ARY__FATIN;
         msgSetc( "BADMAP", mcb->amm );
         errRep( " ", "Invalid active mapping mode '^BADMAP' found in Mapping"
                 "Control Block (internal programming error).", status );
      }
   }

/* If the unmapping operation was successful, then release the MCB and
   reset the ACB mapping pointer to NULL, indicating that the array is
   no longer mapped. */
   if( *status == SAI__OK ){
      ary1Rls( (AryObject *) mcb, status );
      acb->mcb = NULL;
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Umps", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
