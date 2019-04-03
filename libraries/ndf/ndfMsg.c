#include "ndf1.h"
#include "ndf.h"
#include "ndf1_types.h"

void ndfMsg_( const char *token, int indf ){
/*
*+
*  Name:
*     ndfMsg

*  Purpose:
*     Assign the name of an NDF to a message token.

*  Synopsis:
*     void ndfMsg( const char *token, int indf )

*  Description:
*     This function assigns the name of an NDF to a message token (in a
*     form which a user will understand) for use in constructing messages
*     with the ERR_ and MSG_ functions (see SUN/104).

*  Parameters:
*     token
*        Pointer to a null terminated string holding the name of the
*        message token.
*     indf
*        NDF identifier.

*  Notes:
*     -  This function has no STATUS parameter and performs no error
*     checking. If it should fail, then no assignment to the message token
*     will be made and this will be apparent in the final message.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( NULL );

/* Convert the NDF identifier into an ACB index. */
   acb = (NdfACB *) ndf1Id2ac( indf, 1 );

/* If this succeeded, then assign the NDF name to a message token. */
   if( acb ) ndf1Amsg( token, acb );

/* Restablish the original AST status pointer */
   NDF_FINAL

}

