/*
*  Name:
*     grf_3.2.c

*  Purpose:
*     Implement the grf module required by AST V3.2 if no graphics system 
*     is available.

*  Description:
*     This file implements the low level graphics functions required
*     by the rest of AST V3.2, except for those already defined in
*     grf_2.0.c (i.e. those needed by AST V2.0). These implementations
*     simply report an error when called.

*  Inheritance:
*     This module is not a class and does not inherit.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     23-NOV-2004 (DSB):
*        Original version.
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
int astGScales( float *alpha, float *beta ){
   Report( "astGScales" );
   return 0;
}               

int astGCap( int cap, int value ){
   return 0;
}               

static void Report( const char *name ){
   astError( AST__GRFER, "%s: The graphics facilities implement by %s "
             "(introduced at AST V3.2) are needed but are unavailable.",
             name, name );
   astError( AST__GRFER, "Re-link using a suitable option such as '-pgplot' "
             "with the ast_link script, or add an implementation of this "
             "function to your 'grf' module." );
}
