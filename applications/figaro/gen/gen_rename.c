/*+
 * Name:
 *    gen_rename

 * Purpose:
 *    Renames a file.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Fortran-callable C funtion.

 * Invocation (from Fortran):
 *    RESULT = GEN_RENAME( OLD, NEW )

 * Description:
 *    This routine is used to rename a file. It is a Fortran interface
 *    to the C function `rename' as defined in <stdio.h>. There may be
 *    system restrictions on what can be renamed to what. E.g. the old
 *    VAX macro routine required that the file did not move between
 *    disks. Apparently this is no problem on SunOS and Ultrix, so I am
 *    told. On OSF/1 the file cannot move to another file system.
 *
 *    This routine would now probably be used on VMS as well as Unix.

 * Arguments:
 *    OLD = CHARACTER * ( * ) (Given)
 *       Name of the file to be renamed. Can include a directory
 *       specification, but no wild cards.
 *    NEW = CHARACTER * ( * ) (Given)
 *       New name of file.  Can include a directory specification,
 *       although this must be on the same disk as the original file.
 *       Cannot include wild cards.

 * Returned Value:
 *    GEN_RENAME = INTEGER
 *       This is the value returned by rename(). Solaris and OSF/1 agree
 *       on this being 0 for successful operation and -1 for an error.

 * Authors:
 *    ckl: ? (CIT)
 *    ks:  Keith Shortridge (AAO)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    ?? ??? 19?? (ckl):
 *       Original version.
 *    17 Dec 1992 (ks):
 *       This UNIX version produced as a minor repackaging of a version
 *       written by CKL/CIT. (All I changed was the error return code on
 *       success.)
 *    19 Jul 1995 (hme):
 *       Use CNF/F77 to make it portable. No longer return errno, but
 *       just the returned value from rename().
 *-
 */

#include <stdio.h>
#include "cnf.h"
#include "f77.h"

/*:
 */

F77_INTEGER_FUNCTION(gen_rename)( CHARACTER(old), CHARACTER(new)
   TRAIL(old) TRAIL(new) )
{
   GENPTR_CHARACTER(old)
   GENPTR_CHARACTER(new)

   char* p;
   char* q;
   int   i;

   p = cnf_creim( old, old_length );
   q = cnf_creim( new, new_length );

   if ( !p || !q ) return -1;

   i = rename( p, q );

   cnf_free(q); cnf_free(p);

   return (F77_INTEGER_TYPE)i;
}
