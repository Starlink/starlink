/*
*  Name:
*     grf_null.c

*  Purpose:
*     Implement the grf module if no graphics system is available.

*  Description:
*     This file implements the low level graphics functions required
*     by the rest of AST, by reporting errors when called.

*  Inheritance:
*     This module is not a class and does not inherit.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     23-OCT-1996 (DSB):
*        Original version.
*     13-NOV-1996 (DSB):
*        Modified to issue error messages using astError instead of printf.
*/

/* Header files */
/* ============ */
#include "grf.h"           /* Declare the functions in this module */
#include "error.h"         /* AST error reporting facilities */
#include "ast_err.h"       /* AST error codes */

/* Function Prototypes */
/* =================== */
static void Report( const char * );

/* Function definitions */
/* ==================== */
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
