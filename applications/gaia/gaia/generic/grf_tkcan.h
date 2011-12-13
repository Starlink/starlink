#if !defined( GRFTKCAN_INCLUDED ) /* Include this file only once */
#define GRFTKCAN_INCLUDED
/*
 *+
 *  Name:
 *     grf_tkcan.h

 *  Type:
 *     C include file.

 *  Purpose:
 *     Define the interface to the grf_tkcan external routines

 *  Invocation:
 *     #include "grf_tkcan.h"

 *  Description:
 *     This include file defines the interface to the grf_tkcan
 *     routines that are not part of the standard grf interface.

 *  Inheritance:
 *     The grf_tkcan module is not a class and does not inherit.

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
 *     18-AUG-1997 (PWD):
 *        Original version.
 *-
 */

#include "tcl.h"

/* Function prototypes. */
/* ==================== */
#ifdef __cplusplus
extern "C" {
#endif
    int astTk_Init( Tcl_Interp *, const char * );
    int astTk_SetCanvas( const char * );
    void astTk_Tag( const char * );
    void astTk_InitColours();
    void astTk_AddColour( const int, const char * );
    void astTk_LineType( int, int );
    void astTk_ResizeFonts( int );
#ifdef __cplusplus
}
#endif

#endif
