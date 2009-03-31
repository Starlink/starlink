/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "pointset.h"           /* declaration of AST__BAD */

/* C header files. */
/* --------------- */
#include <float.h>
#include <stdio.h>

/* Main function. */
/* ============== */
int main( void ) {
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
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*     
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     18-NOV-1997 (RFWS);
*        Original version.
*     24-OCT-2000 (DSB):
*        Ensure that the number of digits used is at least the minimum
*        required by IEEE for a conversion from binary to string and back 
*        to binary to be an identity. 
*     31-MAR-2009 (TIMJ):
*        Does not take any arguments so don't try to read arguments.
*-
*/

/* Local Constants: */
#define BUFF_LEN ( 2 * DBL_DIG + 20 ) /* Buffer length */
#define IEEE_DIG 17                   /* Minimum number of digits required by 
                                         IEEE for conversion from binary to 
                                         string and back again to be an
                                         identity. */

/* Local Variables: */
   char buff[ BUFF_LEN + 1 ];    /* Buffer for formatted string */
   double ast__bad;              /* Value read back from string */
   int digits;                   /* Number of digits of precision */

/* Vary the precision over a reasonable range to see how many decimal
   digits are required. The initial number of digits is the larger of
   DBL_DIG and IEEE_DIG. */
   for ( digits = ( DBL_DIG > IEEE_DIG )?DBL_DIG:IEEE_DIG; 
         digits <= ( 2 * DBL_DIG ); digits++ ) {

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
