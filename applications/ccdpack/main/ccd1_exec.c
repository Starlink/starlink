#include "cnf.h"
#include "f77.h"
#include "ems.h"
#include "sae_par.h"
#include <stdio.h>
#include <stdlib.h>

F77_SUBROUTINE(ccd1_exec)( CHARACTER(command), INTEGER(status) TRAIL(command) )
/*
 *   Name:
 *      ccd1_exec
 *
 *   Purpose:
 *      Executes a command
 *
 *   Description:
 *      C - routine to enable access to a system-level routine for
 *      executing a command.
 *
 *
 *   Arguments:
 *      command = character * ( * ) (Given)
 *         The command to be executed.
 *      status = integer (Given and Returned)
 *         The global status. Set to SAI__ERROR if command fails.
 *
 *   Authors:
 *      PDRAPER: P.W. Draper (STARLINK - Durham University)
 *
 *   History:
 *      27-MAR-1995 (PDRAPER):
 *         Original version.
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
