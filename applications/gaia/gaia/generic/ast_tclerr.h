#if !defined( ERR_INCLUDED ) /* Include this file only once */
#define ERR_INCLUDED
/*
 *+
 *  Name:
 *     err.h

 *  Type:
 *     C include file.

 *  Purpose:
 *     Define the interface to the err module.

 *  Invocation:
 *     #include "ast_tclerr.h"

 *  Description:
 *     This include file defines the interface to the err module and
 *     provides the type definitions, function prototypes and macros, etc.
 *     needed to use this module.

 *  Inheritance:
 *     The err module is not a class and does not inherit.

 *  Copyright:
 *     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
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
 *     RFWS: R.F. Warren-Smith (STARLINK)
 *     DSB: David Berry (STARLINK)
 *     PWD: Peter W. Draper (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     6-NOV-1996 (DSB):
 *        Original version.
 *     8-SEP-1997 (PWD):
 *        Converted for use with Tcl.
 *     {enter_changes_here}
 *-
 */
#include <tcl.h>

/* Function prototypes. */
/* ==================== */
#if defined(astCLASS)            /* Protected */
void astPutErr_( int, const char * );

/* Function interfaces. */
/* ==================== */
#define astPutErr(status,message) astPutErr_(status,message)
#endif

void errTcl_Init( Tcl_Interp *newinterp );
void errTcl_LastError( int *status, const char **message );

#endif
