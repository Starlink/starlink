

#include <stdlib.h>
#include "hlps.h"

int main( int argc, char **argv )
{
  /* Really need to prompt for a library if -l has not been specified */
  (void) hlps_standalone( NULL, argc, argv );
  return EXIT_SUCCESS;
}
