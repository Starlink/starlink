#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ary_err.h"
#include "ndf_err.h"
#include "star/util.h"
#include <string.h>
#include "mers.h"

void ndf1Cmpac( NdfDCB *dcb, const char *comp, int *status ){
/*
*+
*  Name:
*     ndf1Cmpac

*  Purpose:
*     Check for an error accessing a compressed array

*  Synopsis:
*     void ndf1Cmpac( NdfDCB *dcb, const char *comp, int *status )

*  Description:
*     This function check to see if an error has occurred whilst accessing
*     a compressed (and therefore read-only) array component of an NDF. If
*     such an error has been reported by the ARY system, then the error is
*     annulled and re-reported using more NDF-centric message.

*  Parameters:
*     dcb
*        Pointer to the NDF entry in the DCB.
*     comp
*        Pointer to a null terminated string holding the array component
*        name; "DATA", "QUALITY" or "VARIANCE" (or "ERROR") (case
*        insensitive). The name should not include any white space.
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

/* Local Variables: */
   const char *cname;      /* Array name */
   const char *form;        /* Array storage form */

/* Return if the error status does not indicate that an error occurred
   accessing a compressed array. */
   if( *status != ARY__CMPAC ) return;

/* Get the name of the array to include in the error emssage. Also get he
   storage form. */
   if( ndf1Simlr( comp,  1, 0, "DATA", NDF__MINAB ) ) {
      cname = "DATA";
      form = dcb->dfrm;

   } else if( ndf1Simlr( comp,  1, 0, "ERRORS", NDF__MINAB ) ) {
      cname = "ERROR";
      form = dcb->vfrm;

   } else if( ndf1Simlr( comp,  1, 0, "QUALITY", NDF__MINAB ) ) {
      cname = "QUALITY";
      form = dcb->qfrm;

   } else if( ndf1Simlr( comp,  1, 0, "VARIANCE", NDF__MINAB ) ) {
      cname = "VARIANCE";
      form = dcb->vfrm;
   } else {
      cname = NULL;
      form = NULL;
   }

/* Do nothing if the NDF component is not an array component (something
   has gone wrong!). */
   if( cname ) {

/* Annull the original error message. */
      errAnnul( status );

/* Re-report the error. */
      *status = NDF__CMPAC;
      datMsg( "N", dcb->loc );
      msgSetc( "A", cname );
      msgSetc( "F", form );
      errRep( " ", "The ^A array of the NDF '^N' is stored using ^F "
              "compression and therefore cannot be changed (^F compressed "
              "arrays are read-only).", status );
   }
}

