/*
 *  Purpose:
 *     Tests the cut down version of AST used by WCSLIB for handling units
 *     strings.
 */

/* System header files. */
#include <stdio.h>

/* The following line causes the AST header files to make the internal
   "protected" interface available. The functions defined in unit.h are
   only available with in the protected interface. */
#define astCLASS wcslib

/* Include header files for the AST classes which are used below. */
#include "unit.h"
#include "mapping.h"

main(){
   int pass;
   char *label;
   double xin[3], xout[3];

   pass = 1;

/* Get the Mapping from a speed value in km/h to a log(speed) value
   in "log(m/s)". */
   AstMapping *map = astUnitMapper( "km/h", "log(m/s)", "speed", &label );

/* If no Mapping could be found, test has failed. */
   if( !map ) {
      pass = 0;
      printf("No Mapping returned by astUnitMapper\n");

/* If a Mapping was returned by astUnitMapper, it can be used with any of the
   methods defined by the Mapping class. See:

   http://www.starlink.ac.uk/~dsb/ast/sun211.htx/node450.html

   Here, we use the Mapping to transform three speed values (first is
   negative and so should produce a bad log(speed) value). */
   } else {
      xin[0] = -1.0;
      xin[1] = 1.0;
      xin[2] = 100.0;
      astTran1( map, 3, xin, 1, xout );

/* Check above transformation was succesful. */
      if( astOK ) {

/* Check the transformed values and label are correct. */
         if( xout[ 0 ] != AST__BAD ) {
            printf( "xout[0] wrong: %.*g should be %.*g\n",
                     DBL_DIG, xout[0], DBL_DIG, AST__BAD );
            pass = 0;

         } else if( fabs( xout[ 1 ] - (-0.556302500767287) ) > 1.0E-5 ) {
            printf( "xout[1] wrong: %.*g should be -0.556302500767287\n",
                    DBL_DIG, xout[1] );
            pass = 0;

         } else if( fabs( xout[ 2 ] - 1.44369749923271 ) > 1.0E-5 ) {
            printf( "xout[2] wrong: %.*g should be 1.44369749923271\n",
                    DBL_DIG, xout[2] );
            pass = 0;

         } else if( strcmp( label, "log( speed )" ) ) {
            printf( "label wrong: \"%s\" should be \"log( speed )\"\n",
                    label );
            pass = 0;
         }

      } else {
         printf( "Error on return from astTran1\n" );
         pass = 0;
      }
   }

/* Say whether the test has been passed or not */
   if( !pass ) {
      printf("\n   AST unit test failed\n\n");
   } else {
      printf("\n   AST unit test passed\n\n");
   }
}
