/* Subroutine:  psx_getppid( pid, status )
*+
*  Name:
*     PSX_GETPPID

*  Purpose:
*     Gets the process ID of the parent process

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_GETPPID( PID, STATUS )

*  Description:
*     The routine obtains the process identification number of the parent
*     process and returns the value in PID.

*  Arguments:
*     PID = INTEGER (Returned)
*        The value of the process ID of the parent process.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  If a program that calls this routine is run several times,
*        then unlike GETPID, it will always return the same process ID
*        as all the processes will have the same parent.

*  References:
*     -  POSIX standard (1988), section 4.1.1

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     23-JUN-2000 (AJC):
*        Remove refs to VMS in prologue
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/

#include <config.h>

/* Global Constants:		.					    */

#if defined(vms)
#include <unixlib>		 /* VMS definintions for Unix functions	    */
#else
#  if HAVE_SYS_TYPES_H
#    include <sys/types.h>
#  endif
#  if HAVE_UNISTD_H
#    include <unistd.h>
#  endif
#endif

#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_getppid)( INTEGER(pid), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(pid)
   GENPTR_INTEGER(status)

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Get the parent process ID.						    */

   *pid = (F77_INTEGER_TYPE)getppid();

}
