/*+
 *  This is a temporary, crude, implementation of a Fortran callable
 *  routine PSX_GETCWD. I expect Starlink to eventually provide a
 *  proper version good for both VMS and UNIX, but for the moment I
 *  only need one that will work under UNIX, in order to get the routine
 *  DSAZ_INQ_NAME to work (the VMS version of this routine does not need
 *  to call PSX_GETCWD).  So, with no more appologies, here is a UNIX
 *  specific version, that will work with the Dec DECStation compiler
 *  and the SUN compiler - but possibly not with any others.
 *
 *  Usage:
 *      CALL GEN_GETCWD(CWD,STATUS)
 *
 *  Where:
 *      CWD is a character string that will receive the current working
 *          directory name as an absolute pathname.
 *      STATUS is an integer, used as an inherited status value. This
 *          routine returns immediately if the given status is not zero.
 *          If the routine fails to find out the current working
 *          directory or if the character string provided is too short
 *          to hold the c.w.d. name, then a status of 1 is returned.
 *
 *  27 Aug 1992 (KS / AAO).  Original version.
 *  28 Jul 1993 (HME / UoE, Starlink).  Changed to become a GEN_
 *     routine. Also use F77/CNF for a portable interface to Fortran.
 *+
 */

#include <stdio.h>
#include <sys/param.h>
#include "cnf.h"
#include "f77.h"

F77_SUBROUTINE(gen_getcwd)( CHARACTER(cwd), INTEGER(status) TRAIL(cwd) )
{
   GENPTR_CHARACTER(cwd)
   GENPTR_INTEGER(status)

   int i;                          /* Loop index to blank fill path */
   char path[MAXPATHLEN];          /* String long enough for longest path */

   if ( *status != 0 ) return;

   if ( getwd( path ) == NULL )
   {  *status = 1;                 /* An error */
   }
   else if ( strlen( path ) > cwd_length )
   {  *status = 1;                 /* Fortran string too short */
   }
   else
   {  (void) cnf_exprt( path, cwd, cwd_length );
   }
}
