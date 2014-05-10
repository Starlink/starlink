#if !defined( ERFA_INCLUDED )  /* Include this file only once */
#define ERFA_INCLUDED
/*
*+
*  Name:
*     erfa.h

*  Purpose:
*     Function prototypes for ERFA routines.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Include file

*  Description:
*     Function prototypes for ERFA routines. Note, the
*     --with-external_pal configuration option implies that an external
*     ERFA library will also be used.

*  Authors:
*     DSBJ: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-FEB-2012 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*     USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Include configuration results in order to get any definition for the
   EXTERNAL_PAL macro. This macro is set if the --with-external_pal
   option was set when AST was configured. */
#if HAVE_CONFIG_H
#include <config.h>
#endif

/* If we not using an external ERFA library, rename all ERFA functions so
   that references to "iauXxx" below get translated to "astIauXxx". */
#ifndef EXTERNAL_PAL
#include "erfa2ast.h"
#endif

/* Include the prototypes defined in the erfa header file. */
#include "erfa/erfa.h"

#endif
