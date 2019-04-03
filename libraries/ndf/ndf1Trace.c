#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Trace( const char *routin, int *status ){
/*
*+
*  Name:
*     ndf1Trace

*  Purpose:
*     Provide error traceback reporting for the NDF_ library.

*  Synopsis:
*     void ndf1Trace( const char *routin, int *status )

*  Description:
*     If error tracing is enabled, then when this function is called with a
*     bad "status" value it will report an error message containing the
*     name of the function which called it. It is intended to be used at
*     the end of each function in the NDF_ library.  A traceback of the
*     function calling sequence is then obtained when a bad "status" value
*     is set in response to an error condition, as a result of each
*     function exiting in sequence.

*  Parameters:
*     routin
*        Pointer to a null terminated string holding the name of the
*        calling function.
*     *status
*        The global status. The function does not report an error if this
*        is set to SAI__OK or if error tracing is disabled.

*  Notes:
*     -  Error tracing can be enabled or disabled by means of calls to
*     ndfTune (or ndfTrace, which is now obsolete).
*     -  This function is intended to be called from Fortran. No equivalent
*     error tracing facility is currently available for those NDF_
*     functions which are written in C.

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

/* Check that the status value is bad, otherwise there is nothing more
   to do. */
   if( *status != SAI__OK ) {

/* Wait for access to the TCB. */
      NDF__TCB_LOCK_MUTEX

/* If the error tracing flag is set, then an error report must be made. */
      if( Ndf_TCB_etflg ) {
         errRepf( " ", ".....error exit from function %s", status, routin );
      }

/* Allow other threads to access the TCB. */
      NDF__TCB_UNLOCK_MUTEX
   }
}

