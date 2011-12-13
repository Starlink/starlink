#if !defined( GRF3D_INCLUDED ) /* Include this file only once */
#define GRF3D_INCLUDED
/*
*+
*  Name:
*     grf3d.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the grf3d module

*  Invocation:
*     #include "grf3d.h"

*  Description:
*     This include file defines the interface to the grf3d module and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this module.

*  Inheritance:
*     The grf3d module is not a class and does not inherit.

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

*  History:
*     20-JUN-2007 (DSB):
*        Original version.
*-
*/

/* Include the 2D grf header file in order to inherit the GRF__ macros. */
#include "grf.h"

/* Function prototypes. */
/* ==================== */
int astG3DAttr( int, double, double *, int );
int astG3DCap( int, int );
int astG3DFlush( void );
int astG3DLine( int, float *, float *, float * );
int astG3DMark( int, float *, float *, float *, int, float[3] );
int astG3DQch( float * );
int astG3DText( const char *, float[3], const char *, float[3], float[3] );
int astG3DTxExt( const char *, float[3], const char *, float[3], float[3], float *, float *, float *, float[3] );


#endif
