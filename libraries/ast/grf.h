#if !defined( GRF_INCLUDED ) /* Include this file only once */
#define GRF_INCLUDED
/*
*+
*  Name:
*     grf.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the grf module

*  Invocation:
*     #include "grf.h"

*  Description:
*     This include file defines the interface to the grf module and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this module.

*  Inheritance:
*     The grf module is not a class and does not inherit.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     27-JUN-1996 (DSB):
*        Original version.
*     25-OCT-1996 (DSB):
*        Primatives macros defined, extra parameter added to astGAttr.
*     17-FEB-2006 (DSB):
*        Added GRF__ESH and GRF__ESG.
*-
*/

/* Constant Values. */
/* ================ */
/* Values identifying different graphics attributes. */
#define GRF__STYLE  0
#define GRF__WIDTH  1
#define GRF__SIZE   2
#define GRF__FONT   3
#define GRF__COLOUR 4

/* Values identifying different graphics primatives. */
#define GRF__TEXT 0
#define GRF__LINE 1
#define GRF__MARK 2

/* The number of different graphics attributes */
#define GRF__NATTR 5

/* Values identifying capabilities */
#define GRF__ESC     0
#define GRF__MJUST   1
#define GRF__SCALES  2

/* Values identifying types of graphics escape sequence */
#define GRF__ESPER 1
#define GRF__ESSUP 2
#define GRF__ESSUB 3
#define GRF__ESGAP 4
#define GRF__ESBAC 5
#define GRF__ESSIZ 6
#define GRF__ESWID 7
#define GRF__ESFON 8
#define GRF__ESCOL 9
#define GRF__ESSTY 10
#define GRF__ESPOP 11
#define GRF__ESPSH 12
#define GRF__ESH   13
#define GRF__ESG   14

/* Function prototypes. */
/* ==================== */
int astGAttr( int, double, double *, int );
int astGScales( float *, float * );
int astGBBuf( void );
int astGEBuf( void );
int astGFlush( void );
int astGLine( int, const float *, const float * );
int astGMark( int, const float *, const float *, int );
int astGQch( float *, float * );
int astGText( const char *, float, float, const char *, float, float );
int astGTxExt( const char *, float, float, const char *, float, float, float *, float * );
int astGCap( int, int );

#endif
