#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"

void aryMsg( const char *token, Ary *ary ) {
/*
*+
*  Name:
*     aryMsg

*  Purpose:
*     Assign the name of an array to a message token.

*  Synopsis:
*     void aryMsg( const char *token, Ary *ary )

*  Description:
*     This function assigns the name of an array to a message token (in
*     a form which a user will understand) for use in constructing
*     messages with the MSG_ and ERR_ routines (see SUN/104).

*  Parameters:
*     token
*        Name of the message token.
*     ary
*        Array identifier.

*  Notes:
*     -  This routine has no "status" argument and performs no error
*     checking. If it should fail, then no assignment to the message
*     token will be made and this will be apparent in the final
*     message.

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
   AryACB *acb;
   AryDCB *dcb;

/* Convert the array identifier into an ACB pointer. */
   acb = (AryACB *) ary1Id2ac( ary, 1 );

/* If this succeeded, then obtain a pointer to the data object. */
   if( acb ){
      ARY__DCB_LOCK_MUTEX;

      dcb = acb->dcb;

/* Use the data object locator to assign the object name to the message
   token. */
      datMsg( token, dcb->loc );

      ARY__DCB_UNLOCK_MUTEX;
   }

}
