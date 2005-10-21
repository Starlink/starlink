#include "star/shl.h"
#include "f77.h"
#include <stdlib.h>

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* If a Fortran main is defined, provide a dummy entry point to
   satisfy potential linker problems */
#if HAVE_FC_MAIN
void FC_MAIN () {}
#endif


int main( int argc, char ** argv )
{
  /* Make sure fortran is ready for use */
  cnfInitRTL( argc, argv );

   return shl_standalone("ECHOMOP", 1, argc, argv );
}
