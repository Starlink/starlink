#if !defined( GRF3D_GAIA_INCLUDED ) /* Include this file only once */
#define GRF3D_GAIA_INCLUDED
/*
 *+
 *  Name:
 *     grf3d_gaia.h

 *  Type:
 *     C include file.

 *  Purpose:
 *     Define the interface to the grf3d_gaia external routines
 *     and callable function prototypes.

 *  Copyright:
 *     Copyright (C) 1997-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper (Starlink)

 *  History:
 *     05-SEP-2007 (PWD):
 *        Original version.
 *-
 */

/* Callable Function definitions, one for each of Grf3D public interface. */
typedef int (*Grf3DCapFun)( int cap, int value );
typedef int (*Grf3DFlushFun)( void );
typedef int (*Grf3DLineFun)( int n, float *x, float *y, float *z );
typedef int (*GrfG3DQchFun)( float *ch );
typedef int (*Grf3DMarkFun)( int n, float *x, float *y, float *z, int type,
                             float norm[3] );
typedef int (*Grf3DTextFun)( const char *text, float ref[3], const char *just,
                             float up[3], float norm[3] );
typedef int (*Grf3DTxExtFun)( const char *text, float ref[3], const char *just,
                              float up[3], float norm[3], float *xb, float *yb,
                              float *zb, float bl[3] );
typedef int (*Grf3DAttrFun)( int attr, double value, double *old_value,
                             int prim );

/* Register GRF3D functions */
void Grf3d_Register( Grf3DCapFun capFun, Grf3DFlushFun flushFun,
                     Grf3DLineFun lineFun, GrfG3DQchFun qchFun,
                     Grf3DMarkFun markFun, Grf3DTextFun textFun,
                     Grf3DTxExtFun txExtFun, Grf3DAttrFun attrFun );

#endif
