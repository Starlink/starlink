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
*     Copyright (C) 2004 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     27-JUN-1996 (DSB):
*        Original version.
*     25-OCT-1996 (DSB):
*        Primatives macros defined, extra parameter added to astGAttr.
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

/* Function prototypes. */
/* ==================== */
int astGAttr( int, double, double *, int );
int astGScales( float *, float * );
int astGFlush( void );
int astGLine( int, const float *, const float * );
int astGMark( int, const float *, const float *, int );
int astGQch( float *, float * );
int astGText( const char *, float, float, const char *, float, float );
int astGTxExt( const char *, float, float, const char *, float, float, float *, float * );
int astGCap( int, int );

#endif
