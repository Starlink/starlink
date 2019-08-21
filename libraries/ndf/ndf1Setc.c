#include "ndf1.h"
#include "mers.h"

void ndf1Setc( const char *value, const char *token ){
/*
*+
*  Name:
*     ndf1Setc

*  Purpose:
*     Assign a character value to a message token.

*  Synopsis:
*     void ndf1Setc( const char *value, const char *token )

*  Description:
*     This function assigns a character value to a message token by calling
*     the function "msgSetc". It exists solely to reverse the parameter
*     order of "msgSetc" so that mapped character strings may be passed as
*     the "value" parameter on UNIX systems.

*  Parameters:
*     value
*        Pointer to a null terminated string holding the character value to
*        me assigned.
*     token
*        Pointer to a null terminated string holding the message token
*        name.

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

/* Assign the token value. */
   msgSetc( token, value );

}

