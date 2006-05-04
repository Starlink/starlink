#if !defined( TCLERR_INCLUDED ) /* Include this file only once */
#define TCLERR_INCLUDED
/*
 *+
 *  Name:
 *     tcl_err.h

 *  Type:
 *     C include file.

 *  Purpose:
 *     Define the interface to the tcl_err external routines.

 *  Invocation:
 *     #include "tcl_err.h"

 *  Description:
 *     This include file defines the interface to the tcl_err
 *     routines that are not part of the standard ast interface.

 *  Inheritance:
 *     The tcl_err module is not a class and does not inherit.

 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils

 *  Authors:
 *     PWD: Peter W. Draper (Starlink)

 *  History:
 *     8-SEP-1997 (PWD):
 *        Original version.
 *-
 */

#include "tcl.h"

/* Function prototypes. */
/* ==================== */
#ifdef __cplusplus
extern "C" {
#endif

void errTcl_Init( Tcl_Interp *);
int errTcl_Inhibit( int );

#ifdef __cplusplus
}
#endif

#endif
