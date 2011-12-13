/* Subroutine:  psx_isatty( fildsc, istty, status )
*+
*  Name:
*     PSX_ISATTY

*  Purpose:
*     Determine if a file is a terminal

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_ISATTY( FILDSC, ISTTY, STATUS )

*  Description:
*     Determine if FILDSC is a valid file descriptor associated with a
*     terminal. ISTTY is set to TRUE if the file descriptor is
*     associated with a terminal and false otherwise.

*  Arguments:
*     FILDSC = INTEGER (Given)
*        The file descriptor, which is just an integer.
*     ISTTY = LOGICAL (Returned)
*        Is the file descriptor associated with a terminal?
*     STATUS = INTEGER (Given)
*        The global status.

*  Examples:
*        CALL PSX_ISATTY( 0, ISTTY, STATUS )
*     Is the standard input channel a terminal?

*  Notes:
*     -  On Unix the standard file descriptors are 0,1,2, for stdin,
*        stdout and stderr, respectively.

*  References:
*     -  POSIX standard (1988), section 4.7.2

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
*     {enter_new_authors_here}

*  History:
*     17-APR-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     23-JUN-2000 (AJC):
*        Remove refs to VMS in prologue
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/

#include <config.h>

/* Global Constants:							    */

#if HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include <stdio.h>		 /* Standard C I/O library		    */
#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_isatty)( INTEGER(fildsc), LOGICAL(istty), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(fildsc)
   GENPTR_LOGICAL(istty)
   GENPTR_INTEGER(status)

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Find if the file descriptor is a terminal.				    */
/* isatty can return -1 on a VMS system, so checking for isatty returning   */
/* +1 is necessary.							    */

   if( isatty( *fildsc ) == 1 )
      *istty = F77_TRUE;
   else
      *istty = F77_FALSE;

}
