#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Evmsg( const char *token, NdfDCB *dcb ){
/*
*+
*  Name:
*     ndf1Evmsg

*  Purpose:
*     Assign text describing a standard NDF event to a specified message
*     token.

*  Synopsis:
*     void ndf1Evmsg( const char *token, NdfDCB *dcb )

*  Description:
*     This function creates a string describing the supplied NDF that can
*     be used as the descriptive text associated with an NDF event (see
*     ndf1Event), and then assigns it to a specified message token.
*
*     The string contains the following fields, separated by double colons
*     ("::"):
*
*     - The NDF path
*     - The foreign format code (if any)
*
*     Any trailing blank fields are omitted, together with the associated
*     "::" delimiters.

*  Parameters:
*     token
*        Pointer to a null terminated string holding the name of the
*        message token to use.
*     dcb
*        Pointer to the DCB entry to be used.

*  Notes:
*     - This function has no STATUS parameter and does not perform normal
*     error checking. If it should fail, then no value will be assigned to
*     the message token and this will be apparent in the final message.

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
   NdfFCB *fcb;          /* Pointer to an object describing foreign format
                           code */

/* If there is no foreign format file associated with the data object,
   then assign the name of the NDF to a message token, using the data
   object locator stored in the DCB. */
   fcb = dcb->fcb;
   if( !fcb ) {
      datMsg( token, dcb->loc );

/* Otherwise, assign the name of the associated foreign file, as stored
   in the DCB, and append the foreign format code. */
   } else {
      msgSetc( token, dcb->forfl );
      msgSetc( token, "::" );
      msgSetc( token, fcb->name );
   }

}

