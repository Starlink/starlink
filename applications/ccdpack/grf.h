#if !defined( GRF_INCLUDED ) /* Include this file only once */
#define GRF_INCLUDED
/*
*+
*  Name:
*     grf.h

*  Purpose:
*     Define the interface to the grf module

*  Language:
*     {routine_language}

*  Invocation:
*     #include "grf.h"

*  Description:
*     This include file defines the interface to the grf module and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this module.

*  Inheritance:
*     The grf module is not a class and does not inherit.

*  Type:
*     C include file.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (Starlink)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1996 (DSB):
*        Original version.
*     25-OCT-1996 (DSB):
*        Primatives macros defined, extra parameter added to astGAttr.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

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

/* Function prototypes. */
/* ==================== */
int astGAttr( int, double, double *, int );
int astGAxScale( float *, float * );
int astGFlush( void );
int astGLine( int, const float *, const float * );
int astGMark( int, const float *, const float *, int );
int astGQch( float *, float * );
int astGText( const char *, float, float, const char *, float, float );
int astGTxExt( const char *, float, float, const char *, float, float, float *, float * );

#endif
