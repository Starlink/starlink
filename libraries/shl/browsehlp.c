

#include <stdlib.h>
#include "shl.h"

int main( int argc, char **argv )
{
  /* Really need to prompt for a library if -l has not been specified */
  (void) shl_standalone( NULL, argc, argv );
  return EXIT_SUCCESS;
}
