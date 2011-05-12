
/* Test that we can use Fortran to retrieve an argument string
   given to it from C, thereby making sure that the Runtime library
   has been initialised. */

#if HAVE_CONFIG_H
# include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "f77.h"

#if HAVE_FC_MAIN
void FC_MAIN ( void ) {}
#endif

F77_SUBROUTINE(ftestarg)(char *, int);

int main () {
   int status;
   int iargc = 2;
   char* argv[2] = { "command", "hello" };
   char retval[] = "     ";

   cnfInitRTL(iargc, argv);

   F77_LOCK( F77_CALL(ftestarg)( retval, strlen(retval)); )

   if (strcmp(retval, argv[1]) == 0) {
     printf("Correctly got '%s' and '%s'\n", argv[1], retval);
     status = EXIT_SUCCESS;
   } else {
     printf("Went horribly wrong. Got '%s' instead of '%s'\n", retval, argv[1]);
     status = EXIT_FAILURE;
   }

   return status;
}
