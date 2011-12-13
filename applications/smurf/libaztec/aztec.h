/*
*+
*  Name:
*     azteclib.h

*  Purpose:
*     Prototypes and constants for libaztec functions

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "aztec.h"

*  Description:
*     Prototypes and constants used by the libaztec functions.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-09-1 (EC):
*        Initial version
*
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council, University of British Columbia.  All Rights Reserved.

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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/
#ifndef AZTEC_H_DEFINED
#define AZTEC_H_DEFINED

#define AZTEC_NBOLO 144

void aztec_fill_smfHead( smfHead * hdr, int indf, int * status );

#endif /* AZTEC_H_DEFINED */
