/* Subroutine:  psx_cuserid( user, status )
*+
*  Name:
*     PSX_CUSERID

*  Purpose:
*     Get the username

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_CUSERID( USER, STATUS )

*  Description:
*     This routine will get a username associated with the effective
*     user ID of the current process. If the username cannot be found,
*     a blank string is returned.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Returned)
*        The username
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  On a Unix system the translation from effective user ID to
*        username is performed. Since there can be several usernames
*        associated with a user ID, there is no guarantee that the value
*        returned will be unique.
*     -  The Unix function cuserid is no longer in the IEEE 1003.1-1990 
*        standard, so an alternative to this routine should be used.
*     -  If the first attempt to get the username fails, one more attempt
*        is made. This overcomes an occasional (timing?) problem on Linux.

*  External Routines Used:
*     cnf: cnfExprt

*  Known Changes Required:
*     There is a workaround for a bug in the include file stdio.h on
*     Ultrix. This should be removed when the bug is fixed. The bug
*     only shows up when using the c89 compiler.
*     Since cuserid is no longer in the IEEE 1003.1-1990 standard, this
*     routine will have to be revised in the future.
      
*  References:
*     -  POSIX standard (1988), section 4.2.4
*     -  POSIX standard (1990), section B.4.2.4
      
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     28-FEB-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*        Added workaround for bug in stdio.h file on the DECstation.
*     30-JUN-1992 (PMA):
*        Add inclusion of unistd.h for DEC OSF/1 as cuserid is no longer
*        in the IEEE 1003.1-1990 standard.
*     14-APR-1993 (PMA):
*        Change the test of whether or not to include the file
*        unistd.h to use my own macro _needs_cuserid.
*     16-JUN-2000 (AJC):
*        Trap NULL pointer returned by cuserid - try twice to overcome known
*        occasional problem on Linux. On second failure return a null string
*        for inter-platform consistency
*     23-JUN-2000 (AJC):
*        Tidy refs to CNF routines
*      9-JAN-2002 (AJC):
*        #define cuserid directly if defined _needs_cuserid
*        (formerly #included <unistd.h> but that didn't define it).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/


/* Global Constants:							    */

#include <stdio.h>		 /* Standard I/O routines		    */
#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */

#if defined(_needs_cuserid)
/* cuserid is no longer in 1003.1-1990	                                    */
extern char *cuserid( const char *__s ); 
#endif

#if defined(ultrix) && defined(__STDC__)    /* Temporary fix for bug in	    */
extern char *cuserid( const char *__s );    /* stdio.h on Ultrix.	    */
#endif


F77_SUBROUTINE(psx_cuserid)( CHARACTER(user), INTEGER(status) TRAIL(user) )
{

/* Pointers to Arguments:						    */

   GENPTR_CHARACTER(user)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   char tempuser[L_cuserid];	 /* Array to hold username		    */
   char *p_tempuser;		 /* Return value of calling cuserid	    */

/* Check the inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Get the username.							    */

   p_tempuser = cuserid( tempuser );
   if ( p_tempuser == NULL ) {
      p_tempuser = cuserid( tempuser );
      if ( p_tempuser == NULL ) {
         tempuser[0] = '\0';
         p_tempuser = tempuser;
      }
   }

/* Export the username to the Fortran string user.			    */

   cnfExprt( p_tempuser, user, user_length );

}
