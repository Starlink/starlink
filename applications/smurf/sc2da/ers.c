/* ers.c - replacement for DRAMA error reporting to allow standalone test
           programs to be built.

   Method :
    Print out the error string

   History :
    16Sep2004 : original (bdk)
*/

#include <stdio.h>
#include "Ers.h"


void ErsRep ( int flags, int *status, char *string )
{
   printf ( "%s\n", string );
}
