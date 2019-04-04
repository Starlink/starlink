#define _POSIX_SOURCE  1 /* Declare POSIX source */
#define _POSIX2_SOURCE 1 /* (if available) for POSIX2 "system" call */

#include <sys/wait.h>    /* WIFEXITED, WEXITstatus macros */
#include <errno.h>       /* For errno */
#include <stdlib.h>      /* Define malloc, free, system */

#include "sae_par.h"     /* Standard SAE constants */
#include "ems.h"         /* ems_ error reporting routines */
#include "ems_par.h"     /* ems_ public constants */
#include "ndf1.h"        /* Internal NDF definitions */
#include "ndf_err.h"     /* Internal NDF definitions */


void ndf1Docmd( const char *cmd, int *status ){
/*
*+
*  Name:
*     ndf1Docmd

*  Purpose:
*     Execute an operating system command.

*  Synopsis:
*     ndf1Docmd( const char *cmd, int *status )

*  Description:
*     This function executes an operating system command for performing
*     data format conversion (in another process) and checks the
*     returned status for success.

*  Parameters:
*     cmd
*        A pointer to a null terminated string holding the command to
*        be executed.
*     *status
*        The global status.

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
*        VMS support removed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   int result;       /* Function result */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Execute the command. */
   result = system( cmd );

/* N.B. This interpretation of the result returned by the system function
   call is based on the POSIX.2 draft (ANSI C does not define the return
   value), so this interpretation may not be correct for all systems. */

/* If a child process cannot be created, then report an appropriate error. */
   if ( result == -1 ){
      *status = NDF__EXCER;
      emsSyser( "MESSAGE", errno );
      emsRep( " ", "Unable to create a child process to execute "
              "an external command - ^MESSAGE", status );

/* If the command processor could not execute, then report an appropriate
   error. */
   } else if( WIFEXITED( result ) && ( WEXITSTATUS( result ) == 127 ) ) {
      *status = NDF__EXCER;
      emsRep( " ", "Command interpreter could not execute in response to "
              "a call to \"system\" to execute an external command.", status );
      emsSeti( "STS", result );
      emsRep( " ", "\"system\" call returned the error status ^STS.", status );

/* If the command interpreter returned an error value, then report an */
/* error. */
   } else if( result ) {
      *status = NDF__EXCER;
      emsSeti( "STS", result );
      emsRep( " ", "Command interpreter invoked by a call to \"system\" to "
              "execute an external command returned an error status of ^STS.",
              status );
   }

/* If an error has occurred, then report contextual information showing the */
/* format conversion command to be executed. */
   if ( *status != SAI__OK ) {
      emsSetnc( "CMD", cmd, EMS__SZTOK );
      emsRep( " ", "Command was: ^CMD", status );
   }

/* If necessary, call the error tracing function. */
   if ( *status != SAI__OK ) ndf1Trace( "ndf1_docmd", status );

}
