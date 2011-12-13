#include <stdio.h>
#include "nbs_typ.h"

/*
*+
*  Name:
*     nbs_gbl.c

*  Purpose:
*     Define global variables

*  Language:
*     ANSI C

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     WFL: William Lupton (RGO)
*     AA: Alasdair Allan (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     NXG: Norman Gray (Starlink)
*     {enter_new_authors_here}

*  History:
*     XX-XX-19XX (WFL):
*        Original.
*     28-JUN-2004 (AA/TIMJ/NXG):
*        Symbols were undefined under Mac OSX and GCC 3.3, defining the pointer
*        to NULL fixed this (int's set to zero just in case). Looks to be from the
*        ISO-9899:1999 standard (section 6.9.2) (commonly refered to as C99) which
*        declares that:
*
*        "A declaration of an identifier for an object that has file scope
*        without an initializer, and without a storage-class specified for with
*        the storage-class specifier static, constitutes a tentative definition.
*        If a translation unit contains one or more tentative definitions for an
*        identifier, and the translation unit contains no external definitions for
*        that identifier, then the behaviour is exactly as if the translation unit
*        contains a file scope declaration of that identifier, with the composite
*        type as of the end of the translation unit, with an initializer equal to 0."
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* External definitions   */

int nbs_gl_defining = 0; /* Currently defining noticeboard contents? */
item_id nbs_ga_base = NULL; /* Pointer to base of noticeboard currently being
                               defined */

int nbs_ga_alloc_next = 0;
int nbs_ga_alloc_base = 0;
int nbs_ga_alloc_last = 0;
int nbs_ga_alloc_data = 0;

int nbs_gl_item_total = 0;	/* Current total size of Item_descriptor's */
int nbs_gl_fixed_total = 0;	/* Current total size of Fixed_info's */
int nbs_gl_shape_total = 0;	/* Current total size of shape information */
int nbs_gl_boardinfo_total = 0; /* Current total size of Board_info's */
int nbs_gl_data_total = 0;	/* Current total size of primitive data */

int nbs_gl_pid = 0;		/* PID of current process */






