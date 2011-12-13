#if !defined( _HDSPAR_INCLUDED )	/* Protect against multiple inclusion	    */
#define _HDSPAR_INCLUDED 1
/*
*+
* Name:
*    hdspar.h

* Purpose:
*    Public C definitions for the HDSPAR library.

* Language:
*    ANSI C

* Type of Module:
*    Package public include file.

* Description:
*    This file contains definitions which are used by the HDSPAR system and
*    which may also be needed by software which calls routines from this
*    system.

* Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

* Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

* History:
*     31-JAN-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*/

/* External interfaces. */
/* ==================== */
#include "star/hds_types.h"      /* HDS typedefs */

/* Function prototypes. */
/* ====================  */
void datAssoc( const char *param,
               const char *access,
               HDSLoc **LOC,
               int *status );

void datCancl( const char *param, int *status );

void datCreat( const char *param,
               const char *type,
               int ndims,
               const int dims[],
               int *status );

#endif
