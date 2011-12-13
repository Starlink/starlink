/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "pointset.h"           /* declaration of AST__BAD etc */

/* C header files. */
/* --------------- */
#include <float.h>
#include <stdio.h>
#include <string.h>

/* Local Constants: */
#define BUFF_LEN ( 2 * DBL_DIG + 20 ) /* Buffer length */
#define IEEE_DIG 17                   /* Minimum number of digits required by
                                         IEEE for conversion from binary to
                                         string and back again to be an
                                         identity. */

/* Prototypes for local functions */
static void printdval( double );
static void printfval( float );

/* Main function. */
/* ============== */
int main( int argc, char *argv[] ) {
/*
*+
*  Name:
*     astbad

*  Purpose:
*     Generate a string representing an AST floating point constant.

*  Invocation:
*     astbad <value>

*  Type:
*     C program.

*  Description:
*     This program writes a string to standard output containing
*     a formatted decimal representation of a specified C floating point
*     constant defined by AST. This is intended for use in defining these
*     constants for use from languages other than C.
*
*     The value written should contain sufficient decimal digits so
*     that a routine that uses it to generate a value in another
*     language will produce exactly the same value as a C program
*     using the same macro.

*  Arguments:
*     value = LITERAL
*        The name of the constant to be printed: AST__BAD, AST__NAN or
*        AST__NANF. If not supplied, AST__BAD is printed.

*  Copyright:
*     Copyright (C) 2009-2011 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
*     18-JAN-2011 (DSB):
*        Extend to print other floating point constants as well as
*        AST__BAD.
*-
*/

/* Local Variables; */
   const char *name;        /* Pointer to name of constant to be printed */

/* Get the name of the constant to be printed. */
   if( argc > 1 ) {
      name = argv[1];
   } else {
      name = "AST__BAD";
   }

/* Print it. */
   if( !strcmp( name, "AST__BAD" ) ) {
      printdval( AST__BAD );

   } else if( !strcmp( name, "AST__NAN" ) ) {
      printdval( AST__NAN );

   } else if( !strcmp( name, "AST__NANF" ) ) {
      printfval( AST__NANF );

/* Issue an error message if the argument is unknown. */
   } else {
      (void) fprintf( stderr, "astbad: Unknown constant requested: %s\n",
                      name );
   }

/* Exit. */
   return 0;
}


/* Print a double precision value to standard output */
static void printdval( double val ){

/* Local Variables: */
   char buff[ BUFF_LEN + 1 ];    /* Buffer for formatted string */
   double newval;                /* Value read back from string */
   int digits;                   /* Number of digits of precision */

/* Vary the precision over a reasonable range to see how many decimal
   digits are required. The initial number of digits is the larger of
   DBL_DIG and IEEE_DIG. */
   for ( digits = ( DBL_DIG > IEEE_DIG )?DBL_DIG:IEEE_DIG;
         digits <= ( 2 * DBL_DIG ); digits++ ) {

/* Format the value using this precision and then read it back. */
      (void) sprintf( buff, "%.*G", digits, val );
      (void) sscanf( buff, "%lg", &newval );

/* Quit looping when the original value is read back. */
      if ( newval == val ) break;
   }

/* Write the value to standard output, with one extra digit for good
   measure. */
   (void) printf( "%.*G\n", digits + 1, val );
}

/* Print a single precision value to standard output */
static void printfval( float val ){

/* Local Variables: */
   char buff[ BUFF_LEN + 1 ];    /* Buffer for formatted string */
   float newval;                 /* Value read back from string */
   int digits;                   /* Number of digits of precision */

/* Vary the precision over a reasonable range to see how many decimal
   digits are required. The initial number of digits is FLT_DIG. */
   for ( digits = FLT_DIG; digits <= ( 2 * FLT_DIG ); digits++ ) {

/* Format the value using this precision and then read it back. */
      (void) sprintf( buff, "%.*G", digits, val );
      (void) sscanf( buff, "%g", &newval );

/* Quit looping when the original value is read back. */
      if ( newval == val ) break;
   }

/* Write the value to standard output, with one extra digit for good
   measure. */
   (void) printf( "%.*G\n", digits + 1, val );

}

