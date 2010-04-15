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
*     #include "err.h"

*  Description:
*     This include file defines the interface to the err module and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this module.

*  Inheritance:
*     The err module is not a class and does not inherit.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1996 (DSB):
*        Original version.
*     {enter_changes_here}
*-
*/

/* Function prototypes. */
/* ==================== */
#if defined(astCLASS)            /* Protected */
void astPutErr_( int, const char * );

/* Function interfaces. */
/* ==================== */
#define astPutErr(status,message) astPutErr_(status,message)
#endif

#endif
