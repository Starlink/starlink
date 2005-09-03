#if HAVE_CONFIG_H
# include <config.h>
#endif

/* If a Fortran main is defined, provide a dummy entry point to
   satisfy potential linker problems */
#if HAVE_FC_MAIN
void FC_MAIN () {}
#endif

#include <stdlib.h>
#include "shl.h"
#include "cnf.h"

int main( int argc, char **argv )
{
  /* Make sure fortran is ready for us */
  cnfInitRTL( argc, argv );

  /* Really need to prompt for a library if -l has not been specified */
  return shl_standalone( NULL, 0, argc, argv );
}
