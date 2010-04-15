/*+
*  Name:
*     run_dipso

*  Purpose:
*     Sets up signal handling and runs the dipso main routine.

*  Language:
*     ANSI C

*  Description:
*     This function sets up handlers for floating point exception (SIGFPE),
*     access violatyions (SIGSEGV), and interupt (SIGINT) signals, and then
*     activates the main dipso Fortran subroutine. The signal handling is
*     done is C because there seems to be no way of doing a long jump in
*     fortran (i.e. a GOTO from one subroutine back to a place in a higher
*     level subroutine). The standard C library functions "longjmp" and
*     "setjmp" are therefore used for this purpose. There is a restriction on
*     the use of these library functions in that the function into which the
*     jump is to be made must still be active at the time of the jump (i.e. it
*     must not have returned). The only place to put a function so that it is
*     guaranteed not to have returned is at the very top level (the dipso.f
*     routine would have done were it not for the fact that the destination of
*     the jump must be written in C). For this reason, it is necessary to put
*     a C layer on top of the main dipso subroutine.

*  Notes:
*     -  To produce the executable, this function must first be compiled
*     to produce an object module ("cc -c run_dipso.c"), and then linked in
*     with the other dipso fortran routines using f77 ("f77 -o dipso
*     run_dipso.o dipso.f ...").

*  Authors:
*     DSB: David Berry (STARLINK)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1994 (DSB):
*        Original version.
*     24-APR-2008 (PWD):
*        Use CNF RTL initialisations when FC_MAIN is just the normal
*        C main. This is necessary to pass the command-line arguments
*        to Fortran.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


#include <signal.h>
#include <setjmp.h>
#include "f77.h"

#include <config.h>

#if HAVE_FLOATINGPOINT_H
/* Suns have special floating point support which we can take advantage of */
#  include <floatingpoint.h>
#endif

void hand( int );
extern F77_SUBROUTINE(dipso)( INTEGER(n) );


/*  Global Variables: */
jmp_buf here;

/*  Entry Point: */

/* FC_MAIN can be a macro expanding to the entry point used by the
 * Fortran RTL. When that is true assume all Fortran initialisations are
 * done, otherwise we need to take more care.
 */
#if HAVE_FC_MAIN
   int FC_MAIN() {
#else
   int main( int argc, char *argv[] ) {
#endif

/*  Local Variables: */
      DECLARE_INTEGER(n);

/* If not a Fortran main, pass on the arguments. */
#if ! HAVE_FC_MAIN
      cnfInitRTL( argc, argv );
#endif

#if HAVE_IEEE_HANDLER
/*  Switch off ieee floating point exception handling, so that it doesn't
 *  interfere with the handling set up by the following calls to "signal".
 */
      ieee_handler( "set", "common", SIGFPE_ABORT );
      nonstandard_arithmetic();
#endif

/*  Use the setjmp function to define here to be the place to which the
 *  signal handling function will jump when a signal is detected. Zero is
 *  returned on the first invocation of setjmp. If a signal is detected,
 *  a jump is made into setjmp which then returns a positive signal
 *  identifier. */
      n = setjmp( here );

/*  Set up handlers for SIGFPE, SIGSEGV and SIGINT signals. If any of these
 *  signals occur, the function "hand" will be called. */
      signal( SIGINT, hand );
      signal( SIGSEGV, hand );
      signal( SIGFPE, hand );

/*  Now call the dipso main routine, passing zero on the initial entry, but
 *  a positive signal identifier if a subsequent entry is being made as a
 *  result of a trapped signal. */
      F77_CALL(dipso)( INTEGER_ARG( &n ) );

      return 0;
}




/*+
*  Name:
*     hand

*  Purpose:
*     Called when a floating point exception or interupt occurs.

*  Language:
*     ANSI C

*  Description:
*     This function is called when a floating point exception or interupt
*     signal is detected. It just jumps back to the location defined by
*     the global variable "here", returning the signal value.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
/*  Entry point: */
void hand( int sig ){

/*  Jump back to the "setjmp" call which defined the global variable
 *  "here", returning the signal identifier. */
      longjmp( here, sig);

}
