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
*     {enter_new_authors_here}

*  History:
*     20-SEP-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


#include <signal.h>
#include <setjmp.h>
#include "f77.h"

#if defined( sun4_Solaris )

#include <floatingpoint.h>


#elif defined( alpha_OSF1 ) || defined( mips ) || defined( linux )

#define main MAIN__   
void for_rtl_init_( int*, char **);
int for_rtl_finish_( void );


#elif defined( sun4 )

#include <floatingpoint.h>
#define main MAIN_

#endif

void hand( int );
extern F77_SUBROUTINE(dipso)( INTEGER(n) );


/*  Global Variables: */
jmp_buf here;      

/*  Entry Point: */
void main( int argc, char *argv[]){

/*  Local Variables: */
      DECLARE_INTEGER(n);

/*  Initialise the fortran run-time-library data structures (OSF + mips). */

#if defined( alpha_OSF1 ) || defined( mips )
      for_rtl_init_( &argc, argv );


/*  Initialise the fortran run-time-library data structures (sun). */

#elif defined( sun4 ) || defined ( sun4_Solaris )
      f_init();

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

/*  Free the data structures used by the fortran run-time-library */

#if defined( alpha_OSF1 ) || defined( mips )

      for_rtl_finish_();

#elif defined( sun4 ) || defined ( sun4_Solaris )

      f_exit();

#endif


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

/*  Entry point: */
void hand( int sig ){

/*  Jump back to the "setjmp" call which defined the global variable
 *  "here", returning the signal identifier. */
      longjmp( here, sig);

}
