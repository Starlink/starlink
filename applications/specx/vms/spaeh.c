/*+
 * Name:
 *    spaeh

 * Purpose:
 *    Display prompt and get reply.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invocation from Fortran:
 *    CALL SPAEH( PROMPT, PROMPT_L, STRING, STRING_L, STATUS )

 * Description:
 *    This routine simply prints the given prompt on stdout without a
 *    newline character and gets any reply from stdin. The reply is got
 *    with fgets. It may be 132 characters long and must end with a
 *    newline or end-of-file character.

 * Arguments:
 *    prompt = CHARACTER() (Given)
 *       The Fortran prompt string to be printed before a reply is read.
 *    prompt_l = INTEGER() (Given)
 *       The declared length of prompt as a Fortran string.
 *    string = CHARACTER() (Returned)
 *       The Fortran reply string.
 *    string_l = INTEGER() (Given)
 *       The declared length of string as a Fortran string.
 *    status = INTEGER() (Given and Returned)
 *       The global status. If status is set on entry, this routine
 *       returns without action. The status is set by this routine (but
 *       no report made) if and only if the reply was a sole end-of-file
 *       character.

 * Returned value:
 *    None.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    01 Jul 1993 (hme):
 *       Original version.
 *-
 */

#include <stdio.h>
#include "cnf.h"
#include "f77.h"

#define SAI__ERROR 148013867
#define SAI__OK    0

/*:
 */

F77_SUBROUTINE(spaeh)( CHARACTER(prompt), INTEGER(prompt_l),
   CHARACTER(string), INTEGER(string_l), INTEGER(status) )
{
   GENPTR_CHARACTER(prompt)
   GENPTR_INTEGER(prompt_l)
   GENPTR_CHARACTER(string)
   GENPTR_INTEGER(string_l)
   GENPTR_INTEGER(status)

   char       cstring[133];      /* C version of Fortran strings */

/*.
 */

/* Check status on entry.
 */
   if ( *status != SAI__OK ) return;

/* Convert given prompt to a C string and print it on standard output.
 * We need a string format, so that the string itself may contain
 * e.g. a per cent sign.
 */
   (void) cnf_impbn( prompt, *prompt_l, 132, cstring );
   (void) printf( "%s", cstring );

/* Get at most 132 characters back as reply.
 * If fgets returns NULL, an EOF has been read without any text before
 * it. This will be reported back as an error in the status variable.
 */
   if ( fgets( cstring, 133, stdin ) == NULL )
   {  cstring[0] = '\0';
      *status = SAI__ERROR;
   }

/* If the reply ends with a newline, strip that off.
 */
   if ( cstring[cnf_lenc(cstring)-1] == '\n' )
      cstring[cnf_lenc(cstring)-1] = '\0';

/* Else (input ended without newline), better print a newline for more
 * tidy screen output.
 */
   else
      (void) printf( "\n" );

/* Convert the reply string to Fortran.
 */
   (void) cnf_exprt( cstring, string, *string_l );

/* Return.
 */
   return;
}
