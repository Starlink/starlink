#if !defined( KAPLIBS_INCLUDED )  /* Include this file only once */
#define KAPLIBS_INCLUDED
/*
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

*  Authors:
*     DSB: David .S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2005 (TIMJ):
*        GRP interface now uses struct
*/

#include "ast.h"
#include "star/grp.h"
#include "star/hds.h"

void kpg1Asget( int, int, int, int, int, int *, int *, int *, AstFrameSet **, int * );
void kpg1Fillr( float, int, float *, int * );
void kpg1Gausr( float, int, int, float, int, int, int, int, float *, float *, int *, float *, float *, float *, int * );
void kpg1Gtgrp( const char *, Grp **, int*, int *);
void kpg1Kygrp( AstKeyMap *, Grp **, int * );
void kpg1Kygp1( AstKeyMap *, Grp **, const char *, int * );
void kpg1Kymap( Grp *, AstKeyMap **, int * );
void kpg1Kymp1( Grp *, AstKeyMap **, int * );
void kpg1Manir( int, int *, float *, int, int *, int *, int *, int *, float *, int * );
void kpg1Pseed( int * );
void kpg1Wrlst( const char *, int, int, int, double *, int, AstFrameSet *, const char *, int, int *, int, int * );

void irqDelet( int, int * );
void irqRlse( char [5][DAT__SZLOC], int * );
void irqNew( int, const char *, char [5][DAT__SZLOC], int * );
void irqAddqn( char [5][DAT__SZLOC], const char *, int, const char *, int * );
void irqSetqm( char [5][DAT__SZLOC], int, const char *, int, float *, int *, int * );
#endif
