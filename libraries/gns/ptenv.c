/*  gns_$ptenv 

    Sets an environment variable
*/

#include <malloc.h>
#include <string.h>

void gns_1ptenv_( char *lognam, char *value, int llognam, int lvalue)
{
  char *pointer;  /* pointer to environment string */
  int length;     /* length of environment string  */

  length = llognam + lvalue + 3;
  pointer = (char*)malloc( length );
  strncpy( pointer, lognam, llognam );
  *(pointer+llognam) = '\0';
  strcat( pointer, "=" );
  strncat( pointer, value, lvalue );
  *(pointer+llognam + lvalue + 1) = '\0';

  putenv( pointer );

  return;
}
