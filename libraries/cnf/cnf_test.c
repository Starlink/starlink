/* Simple test of the release of CNF
*  We use the <> construct on #include so that only an installed version
*  of the file will be found.
*  CFLAGS should be set appropriately (usually -I$(INSTALL_INC)).
*  Also we include both files to check the #ifdef mechanism.
*/
#include <stdio.h>
#include <string.h>
#include <f77.h>                 /* CNF macros and prototypes               */
#include  <cnf.h>                 /* CNF macros and prototypes               */

int main()
{
   /* include trailing spaces */
   char input[] = "Twenty six characters long     ";
   char *string, *string2, nullstring, nullstring2;
   int clength,flength;
   void * dummy;

   /* Make sure that we include trailing blanks to properly test cnf_lenc */
   string = cnfMalloc( strlen( input ) + 1 );
   cnfImprt( input, strlen( input ), string );
   clength = cnfLenc( input );
   flength = cnfLenf( input, strlen(input) );
   printf( "The string \"%s\" is %d [%d] characters long\n", string, clength,
	   flength);


   /* Check C to fortran to C */
   string2 = cnfMalloc( strlen( input ) + 1 );
   cnfExprt( string, string2, strlen(input) );
   cnfImprt( string2, strlen(input), string );
   printf("Imprt, Exprt, Imprt: \"%s\"\n", string );

   /* Export/Import to itself */
   cnfExprt( string, string, strlen(input) );
   cnfImprt( string, strlen(input), string );

   /* NULL input string test */
   nullstring = NULL;
   cnfExprt( nullstring, string2, strlen(input) );
   cnfImprt( string2, strlen(input), string );
   if ( string[0] == '\0' ) {
       printf("Passed NULL handling test\n");
   } else {
       printf("FAIL: NULL handling test\n");
   }

   cnfFree( string );
   cnfFree( string2 );

   /* Make sure we can alloc and free */
   dummy = cnfMalloc( 10 );
   cnfFree( dummy );

   exit( 0 );
}
