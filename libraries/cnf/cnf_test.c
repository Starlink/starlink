/* Simple test of the release of CNF
*  We use the <> construct on #include so that only an installed version
*  of the file will be found.
*  CFLAGS should be set appropriately (usually -I$(INSTALL_INC)).
*  Also we include both files to check the #ifdef mechanism.
*/
#include <stdio.h>
#include <f77.h>                 /* CNF macros and prototypes               */
#include  <cnf.h>                 /* CNF macros and prototypes               */

int main()
{
   char *string;
   int length;

   string = "Twenty six characters long";
   length = cnf_lenc( string );

   printf( "The string \"%s\" is %d characters long\n", string, length );

   exit( 0 );
}
