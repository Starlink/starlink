/*
 *+
 *  Name:
 *     smurf_typ.h

 *  Purpose:
 *     type definitions for the smurf application

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Header File

 *  Invocation:
 *     #include "smurf_typ.h"

 *  Description:
 *     Data types used by the smurf functions.

 *  Authors:
 *     Andy Gibb (UBC)
 *     Edward Chapin (UBC)
 *     {enter_new_authors_here}

 *  History:
 *     2005-09-27 (AGG):
 *        Initial test version
 *     2005-09-27 (EC):
 *        Fixed format string macro
 *     2010-11-16 (EC):
 *        Add SMF__BADDIMT
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
 *     Copyright (C) 2005,2010 University of British Columbia.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 3 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful,but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */
#ifndef SMURF_TYP_DEFINED
#define SMURF_TYP_DEFINED

#define DIM_T_FMT "lu"
typedef unsigned long dim_t;
#define SMF__BADDIMT (((dim_t) 0) - 1)

#endif /* SMURF_TYP_DEFINED */
