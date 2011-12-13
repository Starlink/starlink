/*
*+
*  Name:
*     sc2ast_typ.h

*  Purpose:
*     Type definitions required for the sc2ast DA code

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Include File

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-08-06 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#ifndef SC2AST_TYP_H
#define SC2AST_TYP_H

/* Define a special type for subarray number rather than using
   an integer. These values must match the indices in the LUT
   arrays in sc2ast.c */

typedef enum {
  SC2AST__NULLSUB = -1, /* No subarray */
  S8A = 0,
  S8B = 1,
  S8C = 2,
  S8D = 3,
  S4A = 4,
  S4B = 5,
  S4C = 6,
  S4D = 7,
  SC2AST__NSUB = 8  /* Total number of subarrays */
} sc2ast_subarray_t;

#endif
