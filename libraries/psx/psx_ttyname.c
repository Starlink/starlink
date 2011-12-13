/* Subroutine:  psx_ttyname( fildsc, tname, status )
*+
*  Name:
*     PSX_TTYNAME

*  Purpose:
*     Get the name of the terminal

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_TTYNAME( FILDSC, TNAME, STATUS )

*  Description:
*     Get the name of the terminal attached to the given file descriptor.

*  Arguments:
*     FILDSC = INTEGER (Given)
*        The file descriptor.
*     TNAME = CHARACTER * ( * ) (Returned)
*        The name of the terminal attached to FILDSC.
*     STATUS = INTEGER (Given)
*        The global status.

*  Examples:
*     CALL PSX_TTYNAME( 0, TNAME, STATUS )
*        When run on a Unix system, this will return something like
*        "/dev/ttyp2" (without the quotes).

*  Notes:
*     -  If a terminal name is not found, then a blank string is
*        returned in TNAME.
*     -  Under MinGW the name "CON" is always returned.

*  External Routines Used:
*     cnf: cnfExprt

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
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}

*  History:
*     25-APR-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     23-JUN-2000 (AJC):
*        Tidy refs to CNF routines
*        Remove refs to VMS in prologue
*     20-JUL-2004 (PWD):
*        Added default behaviour (for when ttyname isn't defined) and
*        return for MinGW.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/

#include <config.h>

/* Global Constants and External Functions.				    */

#if defined(vms)
#include <unixio.h>		 /* Unix I/O functions on VAX/VMS	    */
#else
#include <unistd.h>		 /* Unix I/O functions			    */
#endif

#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_ttyname)( INTEGER(fildsc), CHARACTER(tname),
                             INTEGER(status) TRAIL(tname) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(fildsc)
   GENPTR_CHARACTER(tname)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   char *name;			 /* A pointer returned by ttyname()	    */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Get the terminal name.						    */

#if defined(vms)
   if( ( *fildsc >= 0 ) && ( *fildsc <= 2 ) ) {
      name = ttyname();
   }
   else {
      name = 0;
   }

#elif HAVE_TTYNAME
   name = ttyname(*fildsc);
   /* Easiest way to check for MinGW is by looking for __MINGW32__ */

#elif __MINGW32__
   name = "CON";

#else
   name = NULL;

#endif

/* Export the C string to a FORTRAN string. If there is no terminal name,   */
/* export a blank string.						    */

   if( name ) {
      cnfExprt( name, tname, tname_length );
   }
   else
   {
      cnfExprt( " ", tname, tname_length );
   }

}
