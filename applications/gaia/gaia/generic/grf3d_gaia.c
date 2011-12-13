/*
*  Name:
*     grf3d_gaia.c

*  Purpose:
*     Implement the Grf3D interface using pluggable system for use in GAIA.

*  Description:
*     This file implements the low level 3D graphics functions required
*     by the rest of AST. These implementations simply call registered
*     functions, if defined, or report an error when called.
*
*     To use if remove the AST dummy Grf3D interface (ast_link -grf3d)
*     and then make a call to Grf3d_Register, with appropriate functions
*     that implement the grf3d.h public interface (see GAIA3D).

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (JACH - UCLan)
*     PWD: Peter W. Draper (JAC, Durham University)

*  History:
*     20-JUN-2007 (DSB):
*        Original version.
*     05-SEP-2007 (PWD):
*        Adapted for GAIA.
*/

/* Macros */
/* ====== */
#define astCLASS                 /* Make AST protected functions available */

/* Header files */
/* ============ */
#include "ast.h"
#include "ast_tclerr.h"
#include "grf3d.h"         /* Declare the functions in this module */
#include "grf3d_gaia.h"

/* Function Prototypes */
/* =================== */
static void Report( const char * );

/* Static Pointers to the functions. */
static int (*CapFun)( int cap, int value ) = NULL;
static int (*FlushFun)( void ) = NULL;
static int (*LineFun)( int n, float *x, float *y, float *z  ) = NULL;
static int (*QchFun)( float *ch ) = NULL;
static int (*MarkFun)( int n, float *x, float *y, float *z, int type,
                       float norm[3] ) = NULL;
static int (*TextFun)( const char *text, float ref[3], const char *just,
                       float up[3], float norm[3] ) = NULL;
static int (*TxExtFun)( const char *text, float ref[3], const char *just,
                        float up[3], float norm[3], float *xb, float *yb,
                        float *zb, float bl[3] ) = NULL;
static int (*AttrFun)( int attr, double value, double *old_value,
                       int prim ) = NULL;

/* Register functions that will be called, note only one set allowed */
void Grf3d_Register( Grf3DCapFun capFun, Grf3DFlushFun flushFun,
                     Grf3DLineFun lineFun, GrfG3DQchFun qchFun,
                     Grf3DMarkFun markFun, Grf3DTextFun textFun,
                     Grf3DTxExtFun txExtFun, Grf3DAttrFun attrFun )
{
    CapFun = capFun;
    FlushFun = flushFun;
    LineFun = lineFun;
    QchFun = qchFun;
    MarkFun = markFun;
    TextFun = textFun;
    TxExtFun = txExtFun;
    AttrFun = attrFun;
}


/* Function definitions */
/* ==================== */
int astG3DCap( int cap, int value )
{
    if ( CapFun ) {
        return (*CapFun)( cap, value );
    }
    return 0;
}

int astG3DFlush( void )
{
    if ( FlushFun ) {
        return (*FlushFun)();
    }
    Report( "astG3DFlush");
    return 0;
}

int astG3DLine( int n, float *x, float *y, float *z  )
{
    if ( LineFun ) {
        return (*LineFun)( n, x, y, z );
    }
    Report( "astG3DLine" );
    return 0;
}

int astG3DQch( float *ch )
{
    if ( QchFun ) {
        return (*QchFun)( ch );
    }
    Report( "astG3DQch" );
    return 0;
}

int astG3DMark( int n, float *x, float *y, float *z, int type, float norm[3] )
{
    if ( MarkFun ) {
        return (*MarkFun)( n, x, y, z, type, norm );
    }
    Report( "astG3DMark" );
    return 0;
}

int astG3DText( const char *text, float ref[3], const char *just,
                float up[3], float norm[3] )
{
    if ( TextFun ) {
        return (*TextFun)( text, ref, just, up, norm );
    }
    Report( "astG3DText" );
    return 0;
}

int astG3DTxExt( const char *text, float ref[3], const char *just,
                 float up[3], float norm[3], float *xb, float *yb,
                 float *zb, float bl[3] )
{
    if ( TxExtFun ) {
        return (*TxExtFun)( text, ref, just, up, norm, xb, yb, zb, bl );
    }
    Report( "astG3DTxExt" );
    return 0;
}

int astG3DAttr( int attr, double value, double *old_value, int prim )
{
    if ( AttrFun ) {
        return (*AttrFun)( attr, value, old_value, prim );
    }
    Report( "astG3DAttr" );
    return 0;
}

static void Report( const char *name )
{
    astError( AST__GRFER, "%s: No graphics facilities are available.", name );
}
