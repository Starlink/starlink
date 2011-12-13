/*
*+
*  Name:
*     slv.h

*  Purpose:
*     Prototypes for SLV system

*  Language:
*     {routine_language}

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     RWFS: Rodney Warren-Smith (Starlink)
*     {enter_new_authors_here}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if !defined( SLV_INCLUDED )
#define SLV_INCLUDED 1

#include <sys/types.h>

void SlvKill( pid_t, int * );
void SlvKillW( pid_t, int * );
void SlvWaitK( pid_t, int, int * );
#endif
