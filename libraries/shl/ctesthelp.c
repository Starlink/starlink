/* Simple test of the help library through the simplified C interface */

#include <stdlib.h>
#include "shl.h"

int main( int argc, char **argv )
{

  /* Really need to prompt for a library if -l has not been specified */
  return shl_standalone( "./demo", 0, argc, argv );
}
