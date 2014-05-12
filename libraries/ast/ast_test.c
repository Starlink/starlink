/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "ast.h"                 /* AST C interface definition */

/* C header files. */
/* --------------- */
#include <stdio.h>

/* Main function. */
/* ============== */
int main( int argc, char *argv[] ) {
/*
*+
*  Name:
*     ast_test

*  Purpose:
*     Test installation of the AST library.

*  Type:
*     C program.

*  Description:
*     This program performs a simple test (without using graphics) of
*     the AST library, to check that it is correctly installed. It is
*     not an exhaustive test of the system.

*  Arguments:
*     None.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     19-NOV-1997 (RFWS);
*        Original version.
*-
*/

/* Local Constants: */
#define NCOORD 10                /* Number of coordinates to transform */

/* Local Variables: */
   AstFrameSet *cvt;             /* Pointer to conversion FrameSet */
   AstSkyFrame *sky1;            /* Pointer to first SkyFrame */
   AstSkyFrame *sky2;            /* Pointer to second SkyFrame */
   double xin[ NCOORD ];         /* Input coordinate array */
   double xout[ NCOORD ];        /* Output coordinate array */
   double yin[ NCOORD ];         /* Input coordinate array */
   double yout[ NCOORD ];        /* Output coordinate array */
   int i;                        /* Loop counter for coordinates */

/* Begin an AST context. */
   astBegin;

/* Create two SkyFrames. */
   sky1 = astSkyFrame( "system = FK4_NO_E, equinox = B1920, epoch = B1958" );
   sky2 = astSkyFrame( "system = ecliptic, equinox = J2001" );

/* Create a FrameSet describing the conversion between them. */
   cvt = astConvert( sky1, sky2, "" );

/* If successful, set up some input coordinates. */
   if ( cvt != AST__NULL ) {
      for ( i = 0; i < NCOORD; i++ ) {
         xin[ i ] = 0.1 * (double) i;
         yin[ i ] = 0.2 * (double) i;
      }

/* Display the FrameSet. */
      astShow( cvt );
      printf( "\n");

/* Activate reporting of coordinate transformations. */
      astSet( cvt, "Report = 1" );

/* Perform the forward transformation. */
      astTran2( cvt, 10, xin, yin, 1, xout, yout );
      printf( "\n");

/* Perform the inverse transformation. */
      astTran2( cvt, 10, xout, yout, 0, xin, yin );
   }

/* End the AST context. */
   astEnd;

/* Return an error status. */
   return astOK ? 0 : 1;

/* Undefine local macros. */
#undef NCOORD
}
