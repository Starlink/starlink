/* Subroutine:  psx_getegid( gid, status )
*+
*  Name:
*     PSX_GETEGID

*  Purpose:
*     Gets the effective group ID

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_GETEGID( GID, STATUS )

*  Description:
*     The routine obtains the effective group identification number of the
*     calling process and returns the value in GID.

*  Arguments:
*     GID = INTEGER (Returned)
*        The value of the effective group ID.
*     STATUS = INTEGER (Given)
*        The global status.

*  References:
*     -  POSIX standard (1988), section 4.2.1
      
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


/* Global Constants:		.					    */

#if defined(vms)
#include <unixlib>		 /* VMS definintions for Unix functions	    */
#else
#include <sys/types.h>		 /* Unix system functions		    */
#endif

#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_getegid)( INTEGER(gid), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(gid)
   GENPTR_INTEGER(status)

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Get the effective group ID.						    */

   *gid = (F77_INTEGER_TYPE)getegid();

}
