#include "cnf.h"
#include "f77.h"
#include "ems.h"
#include "sae_par.h"
#include <stdio.h>
#include <stdlib.h>

F77_SUBROUTINE(one_exec)( CHARACTER(command), INTEGER(status) TRAIL(command) )
/*
*+
*   Name:
*      ONE_EXEC
*
*   Purpose:
*      Executes a shell command
*
*   Language:
*      C, designed to be called from Fortran.
*
*   Description:
*      This routine gives FORTRAN programs access to the UNIX
*      system(3) command. See the system(3) man page for how to
*      construct a suitable command.
*
*   Invocation:
*      CALL ONE_EXEC(COMMAND, STATUS)
*
*   Arguments:
*      COMMAND = CHARACTER * ( * ) (Given)
*         The command to be executed.
*      STATUS = INTEGER (Given and Returned)
*         The global status. Set to SAI__ERROR if command fails.
*
*   Authors:
*      PDRAPER: P.W. Draper (STARLINK - Durham University)
*      RTP: Roy Platon (STARLINK)
*
*   History:
*      27-MAR-1995 (PDRAPER):
*         Original version.
*      02-AUG-2000 (RTP)
*         Changed names to add to Odds & Ends Library
*
*/
{
   GENPTR_CHARACTER(command)
   GENPTR_INTEGER(status)
      
   char *cmd;
   int ret;
  
   if ( *status != SAI__OK ) return;
  
   cmd = cnf_creim( command, command_length ); /* Import the FORTRAN string. */
   ret = system( cmd );
   if ( ret != 0 ) {
      *status = SAI__ERROR;
      (void) ems_rep_c( "CCD1_EXECERR", "Error executing command", status );
   }
   return;
}
/* $Id$ */
