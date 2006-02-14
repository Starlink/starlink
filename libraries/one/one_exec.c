#include "cnf.h"
#include "f77.h"
#include "ems.h"
#include "one_err.h"
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
*      TIMJ: Tim Jenness (JAC, Hawaii)
*
*   History:
*      27-MAR-1995 (PDRAPER):
*         Original version.
*      02-AUG-2000 (RTP):
*         Changed names to add to Odds & Ends Library.
*         Imported from CCDPACK:ccd1_main.c
*      28-AUG-2004 (TIMJ):
*         Remove leftover CCD-ism.
*         Include failed command in error message.
*      29-SEP-2004 (TIMJ):
*         Free C string after use
*     13-FEB-2006 (TIMJ):
*        Use cnfFree since this is easier to control when changing
*        malloc library.

*
*/
{
   GENPTR_CHARACTER(command)
   GENPTR_INTEGER(status)

   char *cmd = NULL;
   int ret;
  
   if ( *status != SAI__OK ) return;
  
   cmd = cnfCreim( command, command_length ); /* Import the FORTRAN string. */
   if (cmd == NULL) {
     *status = ONE__MALLOCERR;
     emsRep( "ONE_EXECERR", 
	     "Error allocating temp memory whilst executing command.", 
	     status );
   }
   ret = system( cmd );
   cnfFree( cmd );
   if ( ret != 0 ) {
      *status = ONE__EXECERROR;
      emsSetc( "COMND", cmd );
      (void) emsRep( "ONE_EXECERR", "Error executing command '^COMND'", 
	status );
   }
   return;
}
/* $Id$ */
