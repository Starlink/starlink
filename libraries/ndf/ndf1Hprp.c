#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Hprp( NdfDCB *dcb1, int prop, NdfDCB *dcb2, int *status ){
/*
*+
*  Name:
*     ndf1Hprp

*  Purpose:
*     Propagate history information from one NDF to another.

*  Synopsis:
*     void ndf1Hprp( NdfDCB *dcb1, int prop, NdfDCB *dcb2, int *status )

*  Description:
*     This function propagates attributes and information from one NDF"s
*     history structure to a new one which is being created.

*  Parameters:
*     dcb1
*        Pointer to the input NDF.
*     prop
*        Whether history information is to be propagated.
*     dcb2
*        Pointer to the output NDF. This must not already contain a history
*        component (this function does not check for this).
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the output DCB entry's history locators to null values. */
   dcb2->hloc = NULL;
   dcb2->hrloc = NULL;

/* If the history component is being propagated, then ensure that
   history structure information is available for the input DCB entry. */
   if( prop ) {
      ndf1Dh( dcb1, status );
      if( *status == SAI__OK ) {

/* Check that an input history component exists. Otherwise there is
   nothing more to do. */
         if( dcb1->hloc ) {

/* Create a new output history structure and obtain a locator for it,
   storing this in the DCB. */
            datNew( dcb2->loc, "HISTORY", "HISTORY", 0, NULL, status );
            datFind( dcb2->loc, "HISTORY", &dcb2->hloc, status );

/* Propagate the CREATED component from the input history structure. */
            ndf1Cpync( dcb1->hloc, "CREATED", dcb2->hloc, status );

/* Propagate the sorted flag. */
            dcb2->hsort = dcb1->hsort;

/* Propagate the CURRENT_RECORD value and component from the input
   structure. */
            dcb2->hnrec = dcb1->hnrec;
            ndf1Cpync( dcb1->hloc, "CURRENT_RECORD", dcb2->hloc, status );

/* Propagate the UPDATE_MODE value and component from the input
   structure. */
            dcb2->humod = dcb1->humod;
            ndf1Cpync( dcb1->hloc, "UPDATE_MODE", dcb2->hloc, status );

/* Propagate the RECORDS component from the input structure and obtain a
   locator for the copy, storing this in the DCB. */
            ndf1Cpync( dcb1->hloc, "RECORDS", dcb2->hloc, status );
            datFind( dcb2->hloc, "RECORDS", &dcb2->hrloc, status );

/* Propagate the EXTEND_SIZE value and component from the input
   sructure. */
            dcb2->hext = dcb1->hext;
            ndf1Cpync( dcb1->hloc, "EXTEND_SIZE", dcb2->hloc, status );

/* Propagate remaining history status informaton. */
            dcb2->hdef = dcb1->hdef;
            dcb2->htlen = dcb1->htlen;
            dcb2->htime = dcb1->htime;

/* Note whether the output DCB history information is up to date. */
            dcb2->kh = ( *status == SAI__OK );
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hprp", status );

}

