#if HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef FC_MAIN
void FC_MAIN () {}
#endif

#include <stdlib.h>
#include "shl.h"

int main( int argc, char **argv )
{
  /* Really need to prompt for a library if -l has not been specified */
  return shl_standalone( NULL, 0, argc, argv );
}
