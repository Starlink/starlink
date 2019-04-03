#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Dmsg( const char *token, NdfDCB *dcb ){
/*
*+
*  Name:
*     ndf1Dmsg

*  Purpose:
*     Assign the name of an NDF stored in the DCB to a message token.

*  Synopsis:
*     void ndf1Dmsg( const char *token, NdfDCB *dcb )

*  Description:
*     This function assigns the full name (including the file name) of an
*     NDF data object to a message token for use with the ERR_ and MSG_
*     functions (SUN/104). The NDF is identified by the index of its entry
*     in the DCB.

*  Parameters:
*     token
*        Pointer to a null terminated string holding the name of the
*        message token.
*     dcb
*        Pointer to the NDF data object entry in the DCB.

*  Notes:
*     This function has no "status" parameter and does not perform normal
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

/* If there is no foreign format file associated with the data object,
   then assign the name of the NDF to a message token, using the data
   object locator stored in the DCB. */
   if( dcb->fcb == 0 ) {
      datMsg( token, dcb->loc );

/* Otherwise, assign the name of the associated foreign file, as stored
   in the DCB. */
   } else {
      msgSetc( token, dcb->forfl );
   }

}

