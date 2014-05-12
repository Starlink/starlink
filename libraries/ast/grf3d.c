/*
*  Name:
*     grf3d.c

*  Purpose:
*     Implement the grf3D interface if no graphics system is available.

*  Description:
*     This file implements the low level 3D graphics functions required
*     by the rest of AST. These implementations simply report an error
*     when called.

*  Inheritance:
*     This module is not a class and does not inherit.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (JACH - UCLan)

*  History:
*     20-JUN-2007 (DSB):
*        Original version.
*/

/* Header files */
/* ============ */
#include "grf3d.h"         /* Declare the functions in this module */
#include "error.h"         /* AST error reporting facilities */
#include "ast_err.h"       /* AST error codes */

/* Function Prototypes */
/* =================== */
static void Report( const char * );

/* Function definitions */
/* ==================== */
int astG3DCap( int cap, int value ){
   return 0;
}

int astG3DFlush( void ){
   Report( "astG3DFlush");
   return 0;
}

int astG3DLine( int n, float *x, float *y, float *z  ){
   Report( "astG3DLine" );
   return 0;
}

int astG3DQch( float *ch ){
   Report( "astG3DQch" );
   return 0;
}

int astG3DMark( int n, float *x, float *y, float *z, int type, float norm[3] ){
   Report( "astG3DMark" );
   return 0;
}

int astG3DText( const char *text, float ref[3], const char *just,
                float up[3], float norm[3] ){
   Report( "astG3DText" );
   return 0;
}

int astG3DTxExt( const char *text, float ref[3], const char *just,
                 float up[3], float norm[3], float *xb, float *yb,
                 float *zb, float bl[3] ){
   Report( "astG3DTxExt" );
   return 0;
}

int astG3DAttr( int attr, double value, double *old_value, int prim ){
   Report( "astG3DAttr" );
   return 0;
}

static void Report( const char *name ){
   astError( AST__GRFER, "%s: No graphics facilities are available.", name );
   astError( AST__GRFER, "Re-link using an option such as '-pgplot' with "
             "the ast_link script." );
}
