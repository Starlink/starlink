#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf1_types.h"
#include "ems_par.h"
#include "ems.h"

void ndf1Hlerr( int *status ){
/*
*+
*  Name:
*     ndf1Hlerr

*  Purpose:
*     Log error message information for NDF history recording.

*  Synopsis:
*     void ndf1Hlerr( int *status )

*  Description:
*     If called with "status" set, the function extracts any pending EMS_
*     error messages and stores them in the Error Logging Block (ELB) for
*     subsequent recording in NDF history records. The EMS_ error table is
*     restored to its previous state by re-reporting the error messages so
*     that none are lost. If "status" is not set on entry, the ELB is
*     simply cleared of error messages.

*  Parameters:
*     *status
*        The global status.

*  Notes:
*     This function calls EMS_ functions (rather then ERR_ functions). This
*     is in order to avoid interference from environment-specific
*     interpretation of error message text (e.g. the ADAM environment may
*     replace reported error messages with an externally supplied version),
*     which might make it impossible to restore the error stack to its
*     original state.

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

/* Local Variables: */
   char msg[ EMS__SZMSG + 1 ];     /* Error message text */
   char name[ EMS__SZPAR + 1 ];    /* Error name */
   int istat;            /* Temporary status value */
   int lname;            /* Length of message name */
   int lmsg;             /* Length of error message */

/* Get a pointer to the structure holding thread-specific data values for
   the current thread. */
   NDF_GETTSD

/* Initialise the number of pending error messages and the logged
   status value. Then simply return if "status" is not set. */
   NDF_TSD(elbNerr) = 0;
   NDF_TSD(elbStat) = *status;
   if( *status == SAI__OK ) return;

/* If "status" is set, then inquire whether there are any error messages
   pending. */
   emsStat( &istat );

/* If not, then simply leave the error count at zero. Otherwise, loop to
   load pending error messages into the ELB until no error status is
   returned (indicating that all the messages have been read). */
   if( istat != SAI__OK ) {
      emsEload( name, &lname, msg, &lmsg, &istat );
      while( istat != SAI__OK ){
         lmsg = NDF_MAX( 1, lmsg );

/* Re-report each error (using the associated name and status value) so
   that the error stack eventually ends up with the same contents as it
   started with. */
         emsSetc( "MSG", msg );
         emsRep( " ", "^MSG", &istat );

/* If there is still room, enter each message into the ELB. */
         if( NDF_TSD(elbNerr) < NDF__MXERR ) {
            strcpy( NDF_TSD(elbMsg)[ NDF_TSD(elbNerr) ], msg );
            NDF_TSD(elbNerr)++;
         }
         emsEload( name, &lname, msg, &lmsg, &istat );

      }
   }
}

