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

*  External Routines Used:
*     cnf: cnfExprt

*  References:
*     -  POSIX standard (1988), section 4.7.2
      
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     25-APR-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     23-JUN-2000 (AJC):
*        Tidy refs to CNF routines
*        Remove refs to VMS in prologue
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/

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
   if( ( *fildsc >= 0 ) && ( *fildsc <= 2 ) )
      name = ttyname();
   else
      name = 0;
#else
   name = ttyname(*fildsc);
#endif

/* Export the C string to a FORTRAN string. If there is no terminal name,   */
/* export a blank string.						    */

   if( name )
      cnfExprt( name, tname, tname_length );
   else
   {
      cnfExprt( " ", tname, tname_length );
      
   }

}
