/* Subroutine:  psx_getgid( gid, status )
*+
*  Name:
*     PSX_GETGID

*  Purpose:
*     Gets the real group ID

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_GETGID( GID, STATUS )

*  Description:
*     The routine obtains the real group identification number of the calling
*     process and returns the value in GID.

*  Arguments:
*     GID = INTEGER (Returned)
*        The value of the real group ID.
*     STATUS = INTEGER (Given)
*        The global status.

*  References:
*     -  POSIX standard (1988), section 4.2.1

*  Notes:
*     - On unsupported platforms this function return 0.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

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
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     23-JUN-2000 (AJC):
*        Remove refs to VMS in prologue
*     20-JUL-2004 (PWD):
*        Added behaviour for unsupported platforms.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/

#include <config.h>

/* Global Constants:		.					    */

#if defined(vms)
#  include <unixlib>		 /* VMS definintions for Unix functions	    */
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


F77_SUBROUTINE(psx_getgid)( INTEGER(gid), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(gid)
   GENPTR_INTEGER(status)

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Get the real group ID.						    */
#if HAVE_GETGID
   *gid = (F77_INTEGER_TYPE)getgid();
#else
   *gid = (F77_INTEGER_TYPE)0;
#endif

}
