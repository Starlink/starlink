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
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007-2010, 2013 Science & Technology Facilities Council.
*     Copyright (C) 2009 University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David .S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (Starlink)
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2005 (TIMJ):
*        GRP interface now uses struct
*     7-MAR-2006 (DSB):
*        Added KPG1_RGNDF and KPG1_WGNDF.
*     25-APR-2006 (TIMJ):
*        Add kpgPtfts
*     03-JUL-2006 (TIMJ):
*        Add kpgStatd
*     10-JUL-2006 (DSB):
*        Add kpg1_wwrt and kpg1_wread.
*     14-AUG-2006 (DSB):
*        Added kpg1_mxmnd, kpg1_mxmnr and kpg1_mxmni.
*     29-NOV-2006 (DSB):
*        Added kpg1Gtaxv.
*     5-FEB-2007 (DSB):
*        Added kpg1_gtwcs.
*     7-FEB-2007 (DSB):
*        Added kpg1_medur.
*     22-MAR-2007 (DSB):
*        Added kpg1_gilst.
*     7-MAR-2008 (DSB):
*        Added IRQ constants.
*     15-JUL-2008 (TIMJ):
*        const and size_t to match Grp
*     2009 August 25 (MJC/EC):
*        Removed IRQ interfaces, macros, and type definitions.
*     5-NOV-2009 (DSB):
*        Add kpg1Loctd.
*     13-NOV-2009 (TIMJ):
*        Add kpg1_pixsc
*     2010-05-05 (TIMJ):
*        Modify kpg1Config API
*     2010 August 6 (MJC):
*        Add kpg1Filli.
*     2011-08-22 (TIMJ):
*        kpg1GhstX has a new API
*     2013 December 12 (MJC):
*        Sort so it's easier to see what is available or missing.
*     {enter_further_changes_here}

*-
*/

#include "ast.h"
#include "star/grp.h"
#include "star/hds.h"
#include "star/hds_fortran.h"

/* Macros */
/* ====== */
/* {enter_macros_here} */



/* Type definitions */
/* ================ */
/* {enter_type_definitions_here} */



/* Prototypes for public functions */
/* =============================== */
void fts1Astwn( AstFitsChan *, int, int * );

void kpg1Asffr( AstFrameSet *, const char *, int *, int * );
void kpg1Asmrg( AstFrameSet *, AstFrameSet *, const char *, int, int, int * );
void kpg1Asget( int, int, int, int, int, int *, int *, int *, AstFrameSet **, int * );
void kpg1Asget8( int, int, int, int, int, int *, hdsdim *, hdsdim *, AstFrameSet **, int * );
void kpg1Asndf( int, int, int *, int *, int *, AstFrameSet **, int * );
void kpg1Asndf8( int, int, int *, hdsdim *, hdsdim *, AstFrameSet **, int * );
void kpg1Axcpy( int, int, int, int, int * );

void kpg1Badbx( int, int, int *, int *, int * );

void kpg1Ch2pm( HDSLoc *, AstPolyMap **, int * );
AstKeyMap *kpg1Config( const char *, const char *, AstKeyMap *, int, int * );

double *kpg1Chcof( int,  HDSLoc **, int *, int *, int * );
void kpg1Darad( const char *, int, double *, const char *, int *, double *, double *, int * );
void kpg1Datcp( const HDSLoc *, HDSLoc *, const char *, int * );

void kpg1Elgau( float *, float *, float *, float *, int * );

void kpg1Filli( int, int, int *, int * );
void kpg1Fillr( float, int, float *, int * );
void kpg1Fit1d( int, int, const double x[], const double y[], double *, double *, double *, int * );

void kpg1Gausr( float, int, int, float, int, int, int, int, float *, float *, int *, float *, float *, float *, int * );
AstRegion *kpgGetOutline( int, int * );
void kpg1Ghstd( int, int, const double *, const double *, double, int, int, double *, double *, int *, int * );
void kpg1Ghstr( int, int, const float *, const double *, double, int, int, float *, float *, int *, int * );
void kpg1Gilst( int, int, int, const char *, int *, int *, int *, int * );
void kpg1Gtaxv( const char *, int, int, AstFrame *, int, double *, int *, int * );
void kpg1Gtgrp( const char *, Grp **, size_t*, int * );
int kpgGtfts( int, AstFitsChan ** fchan, int * status );
void kpg1Gtobj( const char *, const char *, void (*)(void), AstObject **, int * );
void kpg1Gtwcs( int, AstFrameSet **, int * );

void kpg1Hdsky( const HDSLoc *, AstKeyMap *, int, int, int * );
void kpg1Hsect( const HDSLoc *, int, int *, int *, HDSLoc *, const char *, int * );
void kpg1Hsstp( int, const int *, double, double, double *, double *, double *, double *, int * );

void kpg1Kygrp( AstKeyMap *, Grp **, int * );
void kpg1Kyhds( AstKeyMap *, const int *, int, int, HDSLoc *, int * );
void kpg1Kymap( const Grp *, AstKeyMap **, int * );

void kpg1Loctd( int, const int *, const int *, const double *, const float *, const int *, int, const float *, int, float, int, float *, float *, int * );

void kpg1Manir( int, int *, float *, int, int *, int *, int *, int *, float *, int * );
void kpg1Medud( int, int, double *, double *, int *, int * );
void kpg1Medur( int, int, float *, float *, int *, int * );
void kpg1Mxmnd( int, int, double *, int *, double *, double *, int *, int *, int * );
void kpg1Mxmni( int, int, int *, int *, int *, int *, int *, int *, int * );
void kpg1Mxmnr( int, int, float *, int *, float *, float *, int *, int *, int * );

void kpg1Opgrd( int, const double[], int, double *, double *, int * );

void kpg1Pseed( int * );
int kpgPtfts( int, const AstFitsChan * fchan, int * status );
void kpgPutOutline( int, float, int, int * );
void kpg1Pxscl( AstFrameSet *, const double *, double *, int * );

void kpg1Rgndf( const char *, size_t, size_t, const char *, Grp **, size_t *, int * );
void kpg1Rnorm( int el, double *array, int seed, int *status );

void kpg1Sdimp( int, int, int *, int * );

void kpg1Wgndf( const char *, const Grp *, size_t, size_t, const char *, Grp **, size_t *, int * );
void kpg1Wrcat( const char *, int, int, int, double *, int, AstFrameSet *, const char *, int, int *, AstKeyMap *, Grp *, Grp *, int, int * );
void kpg1Wread( const HDSLoc *, const char *, AstObject **, int * );
void kpg1Wrlst( const char *, int, int, int, double *, int, AstFrameSet *, const char *, int, int *, int, int * );
void kpg1Wrtab( const char *, int, int, int, double *, int, AstFrameSet *, const char *, int, int *, Grp *, Grp *, int, int * );
void kpg1Wwrt( AstObject *, const char *, const HDSLoc *, int * );


void kpgPixsc( AstFrameSet * iwcs,
               const double at[],
               double pixsc[],
               char *const *value, /* can be NULL */
               char *const *unit, /* can be NULL */
               int chrarr_length, /* length of each element in "value" and "unit". Can be 0 if no value/unit required */
               int *status );

void kpgStatd( int, int, const double[], int, const float[], int *, int *,
               double *, int *, double *, double *, double *, double *,
               int *, int *, double *, int *, double *, double *, double *,
               double * , int * );
void kpgStati( int, int, const int[], int, const float[], int *, int *,
               double *, int *, double *, double *, double *, double *,
               int *, int *, double *, int *, double *, double *, double *,
               double * , int * );

void kpg1CrMapD( int, int, const double x[], const double y[], int, double *, int * );


#endif
