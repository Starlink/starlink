#if !defined( GRFTKCAN_INCLUDED ) /* Include this file only once */
#define GRFTKCAN_INCLUDED
/*
 *+
 *  Name:
 *     grf_tkcan.h
 *
 *  Type:
 *     C include file.
 *
 *  Purpose:
 *     Define the interface to the grf_tkcan external routines
 *
 *  Invocation:
 *     #include "grf_tkcan.h"
 *
 *  Description:
 *     This include file defines the interface to the grf_tkcan
 *     routines that are not part of the standard grf interface. 
 *
 *  Inheritance:
 *     The grf_tkcan module is not a class and does not inherit.
 *
 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *
 *  Authors:
 *     PWD: Peter W. Draper (Starlink)
 *
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
int astTk_Tag( const char * );
void astTk_LineType( int );

#ifdef __cplusplus
}
#endif

#endif
