#if !defined( SOFAM_INCLUDED )  /* Include this file only once */
#define SOFAM_INCLUDED
/*
*+
*  Name:
*     sofam.h

*  Purpose:
*     Macros defined by the SOFA library.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Include file

*  Description:
*     Macros defined by the SOFA library. This is needed by the pal.c
*     file, which includes source files that include "sofam.h" from the
*     current directory (i.e. the main AST source directory), not the
*     sofa subdirectory.

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

/* Include the macros defined in the corresponding header file in the
   sofa subdirectory. */
#include "sofa/sofam.h"

#endif
