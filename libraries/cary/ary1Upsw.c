#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"

void ary1Upsw( AryACB *acb, const char *type, char bad, char copy,
               HDSLoc *datloc, HDSLoc **mloc, void **pntr, char *dce,
               int *status ) {
/*
*+
*  Name:
*     ary1Upsw

*  Purpose:
*     Unmap simple array component mapped for WRITE (or UPDATE) access.

*  Synopsis:
*     void ary1Upsw( AryACB *acb, const char *type, char bad, char copy,
*                    HDSLoc *datloc, HDSLoc **mloc, void **pntr, char *dce,
*                    int *status )

*  Description:
*     The routine unmaps a component of a simple array which has
*     previously been mapped for WRITE (or UPDATE) access. This causes
*     the mapped data lying within the mapping transfer region (if it
*     exists) to be written back into the actual data object and
*     associated memory locations to be released.

*  Parameters:
*     acb
*        Pointer to the ACB.
*     type
*        The data type used for mapped access; a primitive numeric HDS
*        type string (case insensitive).
*     bad
*        Whether to check for "bad" values when performing data type
*        conversion on data lying within the mapping transfer region.
*     copy
*        Whether mapped access is via a "copy" of the data; this
*        indicates whether or not the locator 'mloc' is associated with a
*        temporary object.
*     datloc
*        HDS locator to the actual data object component into which
*        data should be written back if this is necessary.
*     mloc
*        Address of a locator to the HDS object mapped to provide memory
*        locations for the mapped data. This locator will be annulled and
*        reset to NULL by this routine. If it is associated with a
*        temporary object, then this will be erased.
*     pntr
*        Address of a pointer to the mapped data; the pointer is reset to
*        NULL by this routine.
*     dce
*        Address of a flag that is returned indicating whether any data
*        conversion errors occurred.
*     status
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if 'status' is set on
*     entry, although no additional error report will be made if it
*     subsequently fails under these circumstances.
*     -  No ACB or MCB structures are updated by this routine.

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
   AryDCB *dcb;               /* Pointer to the DCB */
   AryMCB *mcb;               /* Pointer to the MCB */
   int tstat;                 /* Temporary status value */

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* If mapped access is via a "copy", then obtain the pointer to the MCB. */
   *status = SAI__OK;
   if( copy ){
      mcb = acb->mcb;

/* If a mapping transfer region exists, then write back the data lying
   inside this region. */
      if( mcb->mtrex ){
         dcb = acb->dcb;
         ary1Ptn( bad, ( acb->ndim > dcb->ndim ) ? acb->ndim : dcb->ndim,
                  mcb->lmrb, mcb->umrb, type, *pntr, mcb->lmtr,
                  mcb->umtr, dcb->lbnd, dcb->ubnd, dcb->type, datloc,
                  dce, status );
      }

/* Annul the temporary object holding the copy (causing it to be erased). */
      ary1Antmp( mloc, status );

/* If mapped access is direct via HDS, then annul the associated locator
   (causing the data to be unmapped). */
   } else {
      datAnnul( mloc, status );
   }

/* Reset the mapped data pointer to NULL. */
   *pntr = NULL;

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Upsw", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
