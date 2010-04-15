#include <signal.h>
#include "f77.h"
#include "sae_par.h"

extern void hand( int sig );

/*
 *   Name:
 *      handlev
 *
 *   Purpose:
 *      Set up handlers for the requested signals.
 *
 *   Description:
 *      If the LEVEL argument is zero or less, then the default signal
 *      handlers are re-established. Otherwise, the "hand" routine is
 *      established as the handler for FPE INT and SEGV signals.
 */

F77_SUBROUTINE(handlev)( INTEGER( level), INTEGER( status ) ){
      GENPTR_INTEGER( level )
      GENPTR_INTEGER( status )
      if( *status == SAI__OK ){

         if( *level <= 0 ) {
            signal( SIGFPE, SIG_DFL );
            signal( SIGINT, SIG_DFL );
            signal( SIGSEGV, SIG_DFL );

         } else {
            signal( SIGFPE, hand );
            signal( SIGINT, hand );
            signal( SIGSEGV, hand );
         }

      }
}
