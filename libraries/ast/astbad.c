/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "ast.h"                 /* AST C interface definition */

/* C header files. */
/* --------------- */
#include <float.h>
#include <stdio.h>

/* Main function. */
/* ============== */
int main( int argc, char *argv[] ) {
/*
*+
*  Name:
*     astbad

*  Purpose:
*     Generate a string representing the AST__BAD value.

*  Type:
*     C program.

*  Description:
*     This program writes a string to standard output containing a
*     formatted decimal representation of the C double value
*     AST__BAD. This is intended for use in defining the AST__BAD
*     constant for use from languages other than C.
*
*     The value written should contain sufficient decimal digits so
*     that a routine that uses it to generate a value in another
*     language will produce exactly the same value as a C program
*     using the AST__BAD macro.

*  Arguments:
*     None.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     18-NOV-1997 (RFWS);
*        Original version.
*-
*/

/* Local Constants: */
#define BUFF_LEN ( 2 * DBL_DIG + 20 ) /* Buffer length */

/* Local Variables: */
   char buff[ BUFF_LEN + 1 ];    /* Buffer for formatted string */
   double ast__bad;              /* Value read back from string */
   int digits;                   /* Number of digits of precision */

/* Vary the precision over a reasonable range to see how many decimal
   digits are required. */
   for ( digits = DBL_DIG; digits <= ( 2 * DBL_DIG ); digits++ ) {

/* Format the AST__BAD value using this precision and then read it
   back. */
      (void) sprintf( buff, "%.*G", digits, AST__BAD );
      (void) sscanf( buff, "%lg", &ast__bad );

/* Quit looping when the original value is read back. */
      if ( ast__bad == AST__BAD ) break;
   }

/* Write the AST__BAD value to standard output, with one extra digit
   for good measure. */
   (void) printf( "%.*G\n", digits + 1, AST__BAD );

/* Exit. */
   return 0;
}
