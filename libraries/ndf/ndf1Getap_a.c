#include "ndf1.h"
#include "sae_par.h"
#include "star/task_adam.h"

void ndf1Getap( char *appn, size_t appn_length, int *status ){
/*
*+
*  Name:
*     ndf1Getap

*  Purpose:
*     Get the name of the currently-executing application.

*  Synopsis:
*     void ndf1Getap( char *appn, size_t appn_length, int *status )

*  Description:
*     This function returns the name of the currently-running application,
*     left justified. The returned value will be truncated without error if
*     the variable supplied is too short.

*  Parameters:
*     appn
*        Pointer to an array in which to return a null terminated string
*        holding application name.
*     appn_length
*        The length of the supplied 'appn' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     This version is specific to the ADAM software environment. It returns
*     the current ADAM action name.

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain the current action name. */
   taskGetName( appn, appn_length, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Getap", status );

}

