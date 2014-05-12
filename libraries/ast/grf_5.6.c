/*
*  Name:
*     grf_5.6.c

*  Purpose:
*     Implement the grf module required by AST V5.6 if no graphics system
*     is available.

*  Description:
*     This file implements the low level graphics functions required
*     by the rest of AST V5.6, except for those already defined in
*     earlier grf_xxx.c files (i.e. those needed by ealier versions
*     of AST). These implementations simply report an error when called.

*  Inheritance:
*     This module is not a class and does not inherit.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     4-MAR-2011 (DSB):
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
int astGBBuf( void ){
   Report( "astGBBuf" );
   return 0;
}

int astGEBuf( void ){
   Report( "astGEBuf" );
   return 0;
}

static void Report( const char *name ){
   astError( AST__GRFER, "%s: The graphics facilities implement by %s "
             "(introduced at AST V5.6) are needed but are unavailable.",
             name, name );
   astError( AST__GRFER, "Re-link using a suitable option such as '-pgplot' "
             "with the ast_link script, or add an implementation of this "
             "function to your 'grf' module." );
}


