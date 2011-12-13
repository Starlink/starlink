#if !defined( GRF3DVTK_INCLUDED ) /* Include this file only once */
#define GRF3DVTK_INCLUDED
/*
 *+
 *  Name:
 *     Grf3dVtk.h

 *  Type:
 *     C include file.

 *  Purpose:
 *     Define the interface to the Grf3dVtk external routines

 *  Invocation:
 *     #include "grf_tkcan.h"

 *  Description:
 *     This include file defines the interface to the Grf3dVtk
 *     routines that are not part of the standard grf3d interface.

 *  Inheritance:
 *     The grf_tkcan module is not a class and does not inherit.

 *  Copyright:
 *     Copyright (C) 2007 Science and Technology Facilities Council
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
 *     01-SEP-2007 (PWD):
 *        Original version.
 *-
 */


/* Function prototypes. */
/* ==================== */

/* Initialise renderer and create a graphics context. */
void *Grf3dVtk_Init( vtkRenderer *renderer, int lookAtCamera );

/* Set the graphics context. */
void Grf3dVtk_SetContext( void *gc );

/* Free a graphics context. */
void Grf3dVtk_FreeContext( void *gc );

/* Clear the renderer of all actors. */
void Grf3dVtk_Clear();

/* Initialise the standard colour map and reset all others (done once).*/
void Grf3dVtk_InitColours();

/* Add an indexed colour. */
void Grf3dVtk_AddColour( int index, double r, double g, double b );

/* Set the text scale (>0) */
void Grf3dVtk_SetTextScale( double textScale );

#endif


