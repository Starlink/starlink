/*
*  Name:
*     grf_perl.c

*  Purpose:
*     Implement the grf module for the Perl interface

*  Description:
*     This file implements the low level graphics functions required
*     by the rest of AST, by reporting errors when called.
*
*     For the Perl interface, each of the standard GRF callbacks
*     is forwarded on to Perl callback. The call stack is adjusted
*     depending on which routine has been called.

*  Inheritance:
*     This module is not a class and does not inherit.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils
*     Perl code Copyright (C) 2004 Particle Physics and Research Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (Starlink)
*     TIMJ: Tim Jenness (JAC)

*  History:
*     23-OCT-1996 (DSB):
*        Original version.
*     13-NOV-1996 (DSB):
*        Modified to issue error messages using astError instead of printf.
*     25-FEB-2004 (TIMJ):
*        Convert from grf_null.c to grf_perl.c
*        Do all the hard stuff.
*/

/* Header files */
/* ============ */
#include "ast.h"
#include "grf.h"           /* Declare the functions in this module */

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */
#include "ppport.h"
#ifdef __cplusplus
}
#endif

/* Have one global hash that contains the SV* reference to the CV */

static HV * GraphCB;

/* Function Prototypes */
/* =================== */
static void Report( const char * );
void Perl_astGrfInit();

/* Function definitions */
/* ==================== */

/* This function creates the hash necessary to store the
   graphics callbacks. Should be called from the BOOT section */
void Perl_astGrfInit () {
  GraphCB  = newHV();
}

/* This function allows a XS routine to store something in our hash */

void Perl_astGrfSet ( AstPlot * plot, char * type, CV * callback  ) {

}


int astGFlush( void ){
   Report( "astGFlush");
   return 0;
}

int astGLine( int n, const float *x, const float *y ){
   Report( "astGLine" );
   return 0;
}

int astGQch( float *chv, float *chh ){
   Report( "astGQch" );
   return 0;
}

int astGMark( int n, const float *x, const float *y, int type ){
   Report( "astGMark" );
   return 0;
}

int astGText( const char *text, float x, float y, const char *just,
              float upx, float upy ){
   Report( "astGText" );
   return 0;
}               

int astGTxExt( const char *text, float x, float y, const char *just,
               float upx, float upy, float *xb, float *yb ){
   Report( "astGTxExt" );
   return 0;
}               

int astGAttr( int attr, double value, double *old_value, int prim ){
   Report( "astGAttr" );
   return 0;
}

static void Report( const char *name ){
   astError( AST__GRFER, "%s: No graphics facilities are available.", name );
   astError( AST__GRFER, "Re-link using an option such as '-pgplot' with "
             "the ast_link script." );
}
