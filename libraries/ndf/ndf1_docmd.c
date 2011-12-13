#define _POSIX_SOURCE 1		 /* Declare POSIX source		    */
#define _POSIX2_SOURCE 1         /* (if available) for POSIX2 "system" call */

#if defined( vms )		 /* VMS version include files:		    */
#include <processes.h>		 /* Define system function		    */
#include <stsdef.h>		 /* VMS status codes			    */

#else				 /* POSIX version include files:	    */
#include <sys/wait.h>		 /* WIFEXITED, WEXITSTATUS macros	    */
#endif

#include <errno.h>		 /* For errno				    */
#include <stdlib.h>		 /* Define malloc, free, system		    */

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* ems_ error reporting routines	    */
#include "ems_par.h"		 /* ems_ public constants		    */
#include "f77.h"		 /* Fortran 77 <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */

   F77_SUBROUTINE(ndf1_docmd)( CHARACTER(CMD),
			       INTEGER(STATUS)
			       TRAIL(CMD) )
   {
/*
*+
*  Name:
*     NDF1_DOCMD

*  Purpose:
*     Execute an operating system command.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_DOCMD( CMD, STATUS )

*  Description:
*     The routine executes an operating system command for performing
*     data format conversion (in another process) and checks the
*     returned status for success.

*  Arguments:
*     CMD = CHARACTER * ( * ) (Given)
*        The command to be executed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is intended to be called from Fortran.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-OCT-1993 (RFWS):
*        Original version.
*     9-NOV-1993 (RFWS):
*        Added POSIX.2 interpretation of returned value from "system"
*        call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Given:							    */
      GENPTR_CHARACTER(CMD)

/* Status:								    */
      GENPTR_INTEGER(STATUS )

/* Local Variables:							    */
      char *cmd;		 /* Pointer to command string		    */
      int i;			 /* Loop counter for characters		    */
      int result;		 /* Function result			    */
      size_t size;		 /* Amount of space to allocate		    */

/*.									    */

/* Check inherited global status.					    */
      if ( *STATUS != SAI__OK ) return;

/* Allocate space to store the command string.				    */
      size = (size_t) CMD_length + (size_t) 1 ;
      cmd = (char *) malloc( size );

/* Check for errors.							    */
      if ( cmd == NULL )
      {
         *STATUS = NDF__NOMEM;
	 emsSeti( "NBYTES", (int) size );
	 emsErrno( "MESSAGE", errno );
	 emsRep( "NDF1_DOCMD_1",
	            "Unable to allocate a block of ^NBYTES bytes of memory \
- ^MESSAGE", STATUS );
      }

/* If OK, copy the command into the allocated space and append a null.	    */
      else
      {
         for ( i = 0; i < (int) CMD_length; i++ )
         {
            cmd[ i ] = (char) CMD[ i ];
         }
         cmd[ i ] = '\0';

/* Execute the command.							    */
         result = system( cmd );

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* Check for a returned error status.					    */
	 if ( !( result & STS$M_SUCCESS ) )
         {
	    *STATUS = NDF__EXCER;
	    emsSyser( "MESSAGE", result );
	    emsRep( "NDF1_DOCMD_2",
	               "Error occurred while executing an external command \
- ^MESSAGE.", STATUS );
	 }

/* POSIX version:							    */
/* =============							    */
/* N.B. This interpretation of the result returned by the system function   */
/* call is based on the POSIX.2 draft (ANSI C does not define the return    */
/* value), so this interpretation may not be correct for all systems.	    */
#else

/* If a child process cannot be created, then report an appropriate error.  */
	 if ( result == -1 )
	 {
	    *STATUS = NDF__EXCER;
	    emsSyser( "MESSAGE", errno );
	    emsRep( "NDF1_DOCMD_3",
	               "Unable to create a child process to execute an \
external command - ^MESSAGE",
		       STATUS );
	 }

/* If the command processor could not execute, then report an appropriate   */
/* error.								    */
	 else if( WIFEXITED( result ) && ( WEXITSTATUS( result ) == 127 ) )
         {
	    *STATUS = NDF__EXCER;
	    emsRep( "NDF1_DOCMD_4",
	               "Command interpreter could not execute in response to \
a call to \"system\" to execute an external command.",
                       STATUS );
	    emsSeti( "STS", result );
	    emsRep( "NDF1_DOCMD_5",
	               "\"system\" call returned the error status ^STS.",
                       STATUS );
	 }

/* If the command interpreter returned an error value, then report an	    */
/* error.								    */
	 else if( result )
         {
	    *STATUS = NDF__EXCER;
	    emsSeti( "STS", result );
	    emsRep( "NDF1_DOCMD_6",
	               "Command interpreter invoked by a call to \"system\" \
to execute an external command returned an error status of ^STS.",
		       STATUS );
	 }
#endif

/* If an error has occurred, then report contextual information showing the */
/* format conversion command to be executed.				    */
	 if ( *STATUS != SAI__OK )
	 {
	    emsSetnc( "CMD", cmd, EMS__SZTOK );
	    emsRep( "NDF1_DOCMD_7", "Command was: ^CMD", STATUS );
	 }

/* Free the allocated space.						    */
         free( (void *) cmd );
      }

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "ndf1_docmd", STATUS );

/* Exit the function.							    */
      return;
   }
