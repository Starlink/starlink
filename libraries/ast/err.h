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
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

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
