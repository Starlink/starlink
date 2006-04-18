#if !defined( KAPLIBS_INCLUDED )  /* Include this file only once */
#define KAPLIBS_INCLUDED
/*
*+
*  Name:
*     kaplibs.h

*  Purpose:
*     Define the C interface to the KAPLIBS library.

*  Description:
*     This module defines the C interface to the functions of the KAPLIBS
*     library. The file kaplibs.c contains C wrappers for the Fortran 
*     KAPLIBS routines.

*  Notes:
*     - Given the size of the KAPLIBS library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use KAPLIBS from C extend this file (and
*     kaplibs.c) to include any functions which they need but which are
*     not already included.

*  Copyright:
*     Copyright (C) 2005, 2006 Particle Physics & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David .S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2005 (TIMJ):
*        GRP interface now uses struct
*     7-MAR-2006 (DSB):
*        Added KPG1_RGNDF and KPG1_WGNDF.

*-
*/

#include "ast.h"
#include "star/grp.h"
#include "star/hds.h"
#include "star/hds_fortran.h"


/* A structure used to pass a group of five HDS locators to and from IRQ
   functions. */

typedef struct IRQLocs {
   HDSLoc *loc[ 5 ];
} IRQLocs;


/* Prototypes for public functions */

void kpg1Asget( int, int, int, int, int, int *, int *, int *, AstFrameSet **, int * );
void kpg1Fillr( float, int, float *, int * );
void kpg1Gausr( float, int, int, float, int, int, int, int, float *, float *, int *, float *, float *, float *, int * );
void kpg1Gtgrp( const char *, Grp **, int*, int *);
void kpg1Kygrp( AstKeyMap *, Grp **, int * );
void kpg1Kymap( Grp *, AstKeyMap **, int * );
void kpg1Manir( int, int *, float *, int, int *, int *, int *, int *, float *, int * );
void kpg1Pseed( int * );
void kpg1Rgndf( const char *, int, int, const char *, Grp **, int *, int * );
void kpg1Wgndf( const char *, Grp *, int, int, const char *, Grp **, int *, int * );
void kpg1Wrlst( const char *, int, int, int, double *, int, AstFrameSet *, const char *, int, int *, int, int * );

void irqDelet( int, int * );
void irqRlse( IRQLocs **, int * );
void irqNew( int, const char *, IRQLocs **, int * );
void irqAddqn( IRQLocs *, const char *, int, const char *, int * );
void irqSetqm( IRQLocs *, int, const char *, int, float *, int *, int * );

int kpgGtfts( int, AstFitsChan ** fchan, int * status );

#endif
