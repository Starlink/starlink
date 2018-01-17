#include "sae_par.h"
#include "ary.h"
#include "mers.h"

void ary1Trace( const char *routine, int *status ) {
/*
*+
*  Name:
*     ary1Trace

*  Purpose:
*     Provide error traceback reporting for the ARY_ library.

*  Synopsis:
*     void ary1Trace( const char *routine, int *status )

*  Description:
*     If error tracing is enabled, then when this routine is called
*     with a bad STATUS value, it will report an error message
*     containing the name of the routine which called it. It is
*     intended to be used at the end of each routine in the ARY_
*     library.  A traceback of the routine calling sequence is then
*     obtained when a bad STATUS value is set in response to an error
*     condition, as a result of each routine exiting in sequence.

*  Parameters:
*     routine
*        The name of the calling routine.
*     status
*        The global status. The routine does not report an error if
*        this is set to SAI__OK.

*  Notes:
*     -  Error tracing can be enabled or disabled by means of calls to
*     AryTrace.

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
*     22-JUN-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Check the STATUS value is bad and error tracing is enabled. */
   if( ( *status != SAI__OK ) && aryTrace( -1 ) ){

/* Define a message token for the routine name. */
      msgSetc( "ROUTINE", routine );

/* Report an error traceback message. */
      errRep( " ", ".....error exit from routine ^ROUTINE", status );
   }

}
