#include "sae_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfTrace_( int newflg, int *oldflg ){
/*
*+
*  Name:
*     ndfTrace

*  Purpose:
*     Set the internal NDF_ system error-tracing flag.

*  Synopsis:
*     void ndfTrace( int newflg, int *oldflg )

*  Description:
*     This function sets an internal flag in the NDF_ system which enables
*     or disables error-tracing messages. If this flag is set to non-zero,
*     then any error occurring within the NDF_ system will be accompanied
*     by error messages indicating which internal functions have exited
*     prematurely as a result. If the flag is set to zero, this internal
*     diagnostic information will not appear and only standard error
*     messages will be produced.

*  Parameters:
*     newflg
*        The new value to be set for the error-tracing flag.
*     *oldflg
*        Returned holding the previous value of the flag.

*  Notes:
*     -  THIS ROUTINE IS OBSOLETE. The internal error tracing flag
*     (referred to above) corresponds with the TRACE tuning parameter used
*     by ndfTune and ndfGtune, so the same effect can be obtained by
*     substituting these two functions.

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

/* Ensure the NDF library has been initialised. */
   NDF_INIT( NULL );

/* The TCB values are held in global variables, so use a mutex to ensure
   the current thread has sole access. */
   NDF__TCB_LOCK_MUTEX

/* Return the previous value of the error-tracing flag. */
   *oldflg = Ndf_TCB_etflg;

/* Set the new value. */
   Ndf_TCB_etflg = newflg;

/* Release the mutex. */
   NDF__TCB_UNLOCK_MUTEX;

/* Restablish the original AST status pointer */
   NDF_FINAL
}

