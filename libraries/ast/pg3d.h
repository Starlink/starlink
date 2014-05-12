#if !defined( PG3D_INCLUDED ) /* Include this file only once */
#define PG3D_INCLUDED
/*
*+
*  Name:
*     pg3d.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the pg3d module

*  Invocation:
*     #include "pg3d.h"

*  Description:
*     This include file defines the interface to the pg3d module
*     (implemented in file grf3d_pgplot.c) and provides the type
*     definitions, function prototypes and macros, etc. needed to
*     use this module.
*
*     The functions in the pg3d interface provide control of the view
*     of the 3D world coordinate system visible on the 2D PGPLOT
*     viewport. They are provided for users of the PGPLOT implementation
*     of the grf3D interface distributed with AST. No calls to these
*     functions are made from within AST itself.

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
*-
*/

int PG3DRotateEye( int, float );
int PG3DSetCamera( float[3], float[3], float[3], float );
int PG3DSetEye( float[3] );
int PG3DSetTarget( float[3] );
int PG3DSetUp( float[3] );
int PG3DSetScreen( float );
int PG3DForward( float );
int PG3DAutoCamera( float[3], float[3] );
int PG3DFindNearest( int, float *, float *, float *, int * );

#endif
